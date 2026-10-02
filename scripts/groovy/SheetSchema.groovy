// SheetSchema.groovy
//
// Reads schema/sheet_columns.tsv -- the one place that says what the columns of
// files.tsv, series.tsv and sources.tsv are, who owns each, and what type it
// holds. Sheets: `files`, `series`, `sources`.
//
// It is a file rather than a constant because BOTH languages need the list.
// The R side already keeps a reserved-column list in code and a copy of it in
// note/data_formats.md, held together by a test; adding Groovy as a third copy,
// in a language R cannot read, is where that arrangement would break.
//
// The three owners are the whole design of the series table:
//
//   machine  facts about the file. Overwritten on every regeneration.
//   seeded   written once from the file table, then yours. NOT re-propagated:
//            a fix in files.tsv does not silently rewrite rows you have edited.
//            --reseed is how you ask for that on purpose.
//   user     anything you add. Never touched.

class SheetSchema {

    static final String MACHINE = "machine"
    static final String SEEDED  = "seeded"
    static final String USER    = "user"

    /** The series table's sheet name, and its id column. */
    static final String SERIES    = "series"
    static final String ID_COLUMN = "series_id"

    /** What the id column was called before v0.7.0, for a message that names the cause. */
    static final String OLD_ID_COLUMN = "prefix"

    /**
     * Refuse a series table that has no id column, naming the real cause.
     *
     * A sheet written before v0.7.0 has `prefix` where `series_id` now is.
     * Without this check the batch reads every id as blank, and its duplicate
     * check then reports that all the rows "share" an id -- loud, but about the
     * wrong thing, and with one included row it gets further still. Check the
     * column up front instead and say what happened.
     */
    static void requireId(List<Map> rows, String what) {
        if (!rows) return
        def cols = rows[0].keySet()
        if (cols.contains(ID_COLUMN)) return
        if (cols.contains(OLD_ID_COLUMN)) {
            throw new IllegalArgumentException(
                what + " has a `" + OLD_ID_COLUMN + "` column and no `" + ID_COLUMN + "`: it was " +
                "written before v0.7.0, which renamed the column (and samples.tsv to series.tsv). " +
                "Rename the column by hand, or regenerate: Make_SeriesSheet pointed at the old sheet " +
                "renames it and keeps your edits; Make_LuxendoSheets writes a fresh one.")
        }
        throw new IllegalArgumentException(
            what + " has no `" + ID_COLUMN + "` column; found: " + cols.join(", "))
    }

    List<Map<String, String>> rows

    /** Load from a repo root (the directory holding schema/). */
    static SheetSchema load(String repoRoot) {
        def f = new File(new File(repoRoot), "schema/sheet_columns.tsv")
        if (!f.isFile()) {
            throw new IllegalStateException("No column schema at " + f.getAbsolutePath())
        }
        return fromFile(f)
    }

    /** Load from scripts/groovy, where the runners resolve their library. */
    static SheetSchema loadFromLibDir(String libDir) {
        return load(new File(libDir).getParentFile().getParentFile().getAbsolutePath())
    }

    static SheetSchema fromFile(File f) {
        def gcl = new GroovyClassLoader(SheetSchema.class.classLoader)
        def tsv = gcl.parseClass(new File(new File(f.getParentFile().getParentFile(),
                                                   "scripts/groovy"), "Tsv.groovy"))
        def s = new SheetSchema()
        s.rows = tsv.read(f)
        def want = ["sheet", "column", "owner", "type", "required", "description"]
        def missing = want - s.rows[0].keySet().toList()
        if (missing) {
            throw new IllegalStateException(
                "Column schema is missing column(s): " + missing.join(", "))
        }
        def badOwner = s.rows.findAll { !(it.owner in [MACHINE, SEEDED, USER]) }
        if (badOwner) {
            throw new IllegalStateException(
                "Column schema has unknown owner(s): " +
                badOwner.collect { it.column + "=" + it.owner }.join(", "))
        }
        return s
    }

    List<Map<String, String>> forSheet(String sheet) {
        return rows.findAll { it.sheet == sheet }
    }

    List<String> columns(String sheet) {
        return forSheet(sheet).collect { it.column }
    }

    List<String> columns(String sheet, String owner) {
        return forSheet(sheet).findAll { it.owner == owner }.collect { it.column }
    }

    List<String> required(String sheet) {
        return forSheet(sheet).findAll { it.required == "yes" }.collect { it.column }
    }

    Map<String, String> types(String sheet) {
        def m = [:]
        forSheet(sheet).each { m[it.column] = it.type }
        return m
    }

    String owner(String sheet, String column) {
        def r = forSheet(sheet).find { it.column == column }
        return r ? r.owner : null
    }
}
