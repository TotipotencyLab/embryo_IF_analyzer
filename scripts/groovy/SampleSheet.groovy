// SampleSheet.groovy
//
// files.tsv -> samples.tsv: one row per SERIES, from one row per FILE.
//
// The invariant the whole chain rests on:
//
//     the `prefix` column == the output filename prefix == the `name` column
//     of the outline table
//
// which is why the prefix is SANITISED here, by the same RoiExport.sanitize()
// the pipeline uses. A sheet saying "my run" while the disk says "my_run" would
// fail the R side's sample-sheet join with nothing visibly wrong.
//
// Why a file table at all, rather than one sheet:
//
//   * Bio-Formats addresses an image as (path, series index). A sheet without a
//     path can only describe one file.
//   * Series names are unique WITHIN a file and not across files -- Series001 is
//     a Leica default and recurs in every .lif. The alias is what makes the
//     prefix unique, and the R side hard-errors on a duplicate prefix, so this
//     is not optional.
//   * Per-file metadata (imaging date, operator, genotype-if-per-slide) is typed
//     once and seeded onto every series of that file.
//
// Regeneration must not destroy hand-edits, so rows are matched on
// path + series_index -- NOT on prefix, which you may edit, and not on alias,
// which changes the prefix when you edit it.

class SampleSheet {

    String libDir
    Class TSV, RX, SCHEMA_CLS
    Object schema

    static SampleSheet load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader(SampleSheet.class.classLoader)
        def s = new SampleSheet()
        s.libDir = dir.getAbsolutePath()
        s.TSV = gcl.parseClass(new File(dir, "Tsv.groovy"))
        s.RX = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        s.SCHEMA_CLS = gcl.parseClass(new File(dir, "SheetSchema.groovy"))
        s.schema = s.SCHEMA_CLS.loadFromLibDir(s.libDir)
        return s
    }

    // --- files.tsv -----------------------------------------------------------

    /** Default alias for a path: the basename without its extension. */
    static String defaultAlias(String path) {
        def n = new File(path).getName()
        int dot = n.lastIndexOf('.')
        return (dot > 0) ? n.substring(0, dot) : n
    }

    /**
     * A files.tsv skeleton for every image in a directory.
     *
     * The alias defaults to the basename, so two files of the same name in
     * different folders collide on the unique-alias check below -- which is
     * exactly the moment you should be choosing a name for them.
     */
    List<Map> scan(File dir, List<String> extensions) {
        if (!dir.isDirectory()) {
            throw new IllegalArgumentException("Not a directory: " + dir.getAbsolutePath())
        }
        def exts = extensions.collect { it.toLowerCase().replaceAll(/^\./, "") }
        def found = []
        dir.listFiles()?.sort { it.getName() }?.each { File f ->
            if (!f.isFile()) return
            def n = f.getName().toLowerCase()
            if (exts.any { n.endsWith("." + it) }) {
                found << [path: f.getName(), alias: defaultAlias(f.getName()), include: "true"]
            }
        }
        return found
    }

    /** Read and validate files.tsv; fills the alias default. */
    List<Map> readFiles(File f) {
        def rows = TSV.read(f)
        if (!rows) throw new IllegalArgumentException(f.getName() + " has no rows")
        if (!rows[0].containsKey("path")) {
            throw new IllegalArgumentException(
                f.getName() + " has no `path` column; found: " + rows[0].keySet().join(", "))
        }
        rows.eachWithIndex { r, i ->
            r.path = (r.path ?: "").trim()
            if (!r.path) {
                throw new IllegalArgumentException(f.getName() + " row " + (i + 1) + " has a blank path")
            }
            if (!r.containsKey("alias") || !(r.alias ?: "").trim()) {
                r.alias = defaultAlias(r.path)
            }
            if (!r.containsKey("include") || !(r.include ?: "").trim()) {
                r.include = "true"
            }
        }
        return rows
    }

    /**
     * The checks that must happen before anything is read or written.
     *
     * Severities differ on purpose. A duplicate path or alias makes the sheet
     * unbuildable, so it stops. A duplicate BASENAME across two folders is
     * legal -- the alias disambiguates the output -- but it is also the
     * signature of a file copied elsewhere or a name reused for a different
     * dataset, so it is reported loudly rather than passed over.
     *
     * @return list of warning strings; throws on the fatal ones
     */
    List<String> checkFiles(List<Map> rows, File imageRoot) {
        def warn = []
        def dupBy = { String key ->
            rows.groupBy { it[key] }.findAll { k, v -> v.size() > 1 }
        }
        def dupPath = dupBy("path")
        if (dupPath) {
            throw new IllegalArgumentException(
                "files.tsv lists the same path more than once: " + dupPath.keySet().join(", "))
        }
        def dupAlias = dupBy("alias")
        if (dupAlias) {
            throw new IllegalArgumentException(
                "files.tsv uses the same alias for more than one file: " +
                dupAlias.collect { k, v -> k + " (" + v.collect { it.path }.join(", ") + ")" }.join("; ") +
                "\n  The alias is what makes every prefix unique across files; give them different names.")
        }
        // Same name in two folders: legal, but worth saying out loud.
        rows.groupBy { new File(it.path).getName() }.each { name, group ->
            if (group.size() > 1) {
                warn << ("Two or more input files share the basename '" + name + "': " +
                         group.collect { it.path }.join(", ") +
                         ". Their aliases differ so the output is unambiguous, but check they really are " +
                         "different data -- a reused filename is the usual cause.")
            }
        }
        // Same name AND same size: very likely one file copied, which the alias
        // cannot see. Cheap, and the same fingerprint the R side already uses.
        rows.groupBy { r ->
            def f = resolve(r.path, imageRoot)
            new File(r.path).getName() + ":" + (f.isFile() ? f.length() : -1L)
        }.each { fp, group ->
            if (group.size() > 1 && !fp.endsWith(":-1")) {
                warn << ("These inputs have the same basename AND the same size (" + fp + "): " +
                         group.collect { it.path }.join(", ") +
                         ". That is what a copy of one file looks like; analysing it twice would " +
                         "double-count every object in it.")
            }
        }
        return warn
    }

    static File resolve(String path, File imageRoot) {
        def f = new File(path)
        if (f.isAbsolute() || imageRoot == null) return f
        return new File(imageRoot, path)
    }

    // --- samples.tsv ---------------------------------------------------------

    /**
     * Read one file's series metadata. No pixels are touched -- 887 ms for an
     * 8.9 GB .lif, so doing every file every time is cheaper than tracking
     * which ones changed.
     *
     * @return list of maps, one per series, with the machine columns filled
     */
    List<Map> inspect(File image) {
        def reader = Class.forName("loci.formats.ImageReader").newInstance()
        def meta = Class.forName("loci.formats.MetadataTools").createOMEXMLMetadata()
        reader.setMetadataStore(meta)
        reader.setId(image.getAbsolutePath())
        def out = []
        try {
            int n = reader.getSeriesCount()
            def num = { v -> (v == null) ? null : v.value().doubleValue() }
            def unit = { v -> (v == null) ? null : v.unit().getSymbol() }
            for (int s = 0; s < n; s++) {
                reader.setSeries(s)
                def px = meta.getPixelsPhysicalSizeX(s)
                int sizeZ = reader.getSizeZ()
                out << [
                    series_index: s,
                    series_name : meta.getImageName(s) ?: ("Series" + String.format("%03d", s + 1)),
                    size_x      : reader.getSizeX(),
                    size_y      : reader.getSizeY(),
                    size_z      : sizeZ,
                    size_c      : reader.getSizeC(),
                    size_t      : reader.getSizeT(),
                    pixel_type  : Class.forName("loci.formats.FormatTools")
                                       .getPixelTypeString(reader.getPixelType()),
                    pixel_width : num(px),
                    pixel_height: num(meta.getPixelsPhysicalSizeY(s)),
                    // Blank rather than a number for a single plane: there is no
                    // z axis to measure, and Bio-Formats reports none. The same
                    // rule _config.txt follows.
                    pixel_depth : (sizeZ > 1) ? num(meta.getPixelsPhysicalSizeZ(s)) : null,
                    pixel_unit  : unit(px),
                    file_size   : image.length(),
                ]
            }
        } finally {
            reader.close()
        }
        return out
    }

    /** sanitise(<alias>_<series name>), via the same function the pipeline uses. */
    String composePrefix(String alias, String seriesName) {
        return RX.sanitize(alias + "_" + seriesName)
    }

    /**
     * Build every sample row from the file table.
     *
     * @param fileRows   from readFiles()
     * @param imageRoot  base for relative paths, or null
     * @param inherit    file columns to seed onto each series row
     * @param log        closure called with progress lines
     */
    List<Map> build(List<Map> fileRows, File imageRoot, List<String> inherit, Closure log = null) {
        def rows = []
        fileRows.each { fr ->
            def img = resolve(fr.path, imageRoot)
            if (!img.isFile()) {
                throw new IllegalArgumentException(
                    "No such image file: " + img.getAbsolutePath() +
                    (imageRoot ? ("\n  (path " + fr.path + " resolved against image root " +
                                  imageRoot.getAbsolutePath() + ")") : ""))
            }
            long t0 = System.currentTimeMillis()
            def series = inspect(img)
            log?.call("  " + fr.path + ": " + series.size() + " series in " +
                      (System.currentTimeMillis() - t0) + " ms")
            series.each { sr ->
                def row = new LinkedHashMap()
                row.prefix = composePrefix(fr.alias, sr.series_name)
                row.alias = fr.alias
                row.path = fr.path
                row.putAll(sr)
                // Seeded from the file row; the sample row owns it afterwards.
                row.include = fr.include
                inherit.each { c ->
                    if (fr.containsKey(c)) row[c] = fr[c]
                }
                rows << row
            }
        }
        return rows
    }

    /**
     * The composed-prefix check, which is NOT implied by the alias check.
     *
     * Alias "A_Series" with series "001" collides with alias "A" and series
     * "Series001": unique parts, colliding composite. And it must run on the
     * SANITISED string, because two series names differing only in whitespace
     * or punctuation become one filename.
     */
    void checkPrefixes(List<Map> rows) {
        def dup = rows.groupBy { it.prefix }.findAll { k, v -> v.size() > 1 }
        if (dup) {
            def detail = dup.collect { k, v ->
                k + " <- " + v.collect { it.path + "[" + it.series_index + "] " + it.series_name }.join(" AND ")
            }.join("\n    ")
            throw new IllegalArgumentException(
                "These sample prefixes are not unique:\n    " + detail +
                "\n  The prefix becomes the output filename and the `name` column, so two series " +
                "would overwrite each other and the R side would merge them into one sample." +
                "\n  Change an alias in files.tsv, or edit the prefix column.")
        }
    }

    // --- merge ---------------------------------------------------------------

    /**
     * The identity of a row across regenerations.
     *
     * A List, so no separator has to be chosen and no path can be mistaken for
     * two. NOT the prefix: you may edit that, and editing the alias rewrites it.
     */
    static List mergeKey(Map row) {
        // NOT `?:` -- in Groovy 0 is falsy, so `row.series_index ?: ""` turns
        // series 0 of every file into a blank key. Every file has a series 0,
        // so a rerun saw them as new rows and the hand-edited originals were
        // left behind as "missing". Caught by the merge tests; the symptom was
        // silent data loss on regeneration, which is the one thing the merge
        // exists to prevent.
        def idx = row.series_index
        return [(row.path == null ? "" : row.path.toString()),
                (idx == null ? "" : idx.toString())]
    }

    /**
     * Fold freshly-read rows into an existing sheet.
     *
     * machine columns are overwritten (they are facts about the file), seeded
     * and user columns are preserved, new rows are appended, and rows whose
     * file has left files.tsv are reported rather than dropped.
     *
     * @return [rows:, added:, updated:, missing:, reseeded:]
     */
    Map merge(List<Map> fresh, List<Map> existing, List<String> reseed, boolean prune) {
        def machine = schema.columns("samples", SCHEMA_CLS.MACHINE)
        def byKey = [:]
        existing.each { byKey[mergeKey(it)] = it }

        def freshKeys = fresh.collect { mergeKey(it) } as Set
        def out = []
        int added = 0, updated = 0
        def reseeded = [:]

        fresh.each { f ->
            def old = byKey[mergeKey(f)]
            if (old == null) {
                out << f
                added++
                return
            }
            def row = new LinkedHashMap(old)
            machine.each { c -> if (f.containsKey(c)) row[c] = f[c] }
            reseed.each { c ->
                if (f.containsKey(c) && String.valueOf(row[c]) != String.valueOf(f[c])) {
                    reseeded[c] = (reseeded[c] ?: 0) + 1
                    row[c] = f[c]
                }
            }
            // Anything the fresh read produced that the old sheet never had.
            f.each { k, v -> if (!row.containsKey(k)) row[k] = v }
            out << row
            updated++
        }

        def missing = existing.findAll { !freshKeys.contains(mergeKey(it)) }
        if (!prune) {
            out.addAll(missing)
        }
        return [rows: out, added: added, updated: updated,
                missing: missing, reseeded: reseeded]
    }

    /** Column order for the written sheet: schema order first, yours after. */
    List<String> columnOrder(List<Map> rows) {
        def known = schema.columns("samples")
        def extra = []
        rows.each { r -> r.keySet().each { if (!known.contains(it) && !extra.contains(it)) extra << it } }
        def present = known.findAll { c -> rows.any { it.containsKey(c) } }
        return present + extra
    }
}
