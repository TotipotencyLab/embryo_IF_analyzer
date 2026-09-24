// SheetSchema.groovy
//
// Reads schema/sheet_columns.tsv -- the one place that says what the columns of
// files.tsv and samples.tsv are, who owns each, and what type it holds.
//
// It is a file rather than a constant because BOTH languages need the list.
// The R side already keeps a reserved-column list in code and a copy of it in
// note/data_formats.md, held together by a test; adding Groovy as a third copy,
// in a language R cannot read, is where that arrangement would break.
//
// The three owners are the whole design of the sample sheet:
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
