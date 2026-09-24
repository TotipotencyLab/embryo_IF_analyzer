// Tsv.groovy
//
// Tab-separated tables, read and written the one way this repo does it.
//
// Small on purpose. It exists so that the sample sheet, the files table and the
// batch summary cannot disagree about quoting, blank cells or line endings --
// and so that "never write a comma into a column another CLI reads back" stays
// a property of one file rather than a habit.

class Tsv {

    /**
     * Read a TSV with a header row into a list of maps, in file order.
     *
     * Blank lines and `#` comments are skipped so a table can be annotated.
     * A short row is padded rather than rejected: a trailing empty cell is
     * exactly what a spreadsheet writes, and refusing it would be refusing the
     * commonest way these files get edited.
     */
    static List<Map<String, String>> read(File f) {
        if (!f.isFile()) {
            throw new IllegalArgumentException("No such table: " + f.getAbsolutePath())
        }
        def lines = f.getText("UTF-8").readLines().findAll { it.trim() && !it.trim().startsWith("#") }
        if (!lines) {
            throw new IllegalArgumentException("Table is empty: " + f.getName())
        }
        def header = lines[0].split("\t", -1).collect { it.trim() }
        def dup = header.countBy { it }.findAll { k, v -> v > 1 }.keySet()
        if (dup) {
            throw new IllegalArgumentException(
                f.getName() + " has duplicate column(s): " + dup.join(", "))
        }
        def out = []
        lines.drop(1).eachWithIndex { String line, int i ->
            def cells = line.split("\t", -1)
            def row = new LinkedHashMap<String, String>()
            header.eachWithIndex { String h, int j ->
                row[h] = (j < cells.length) ? cells[j].trim() : ""
            }
            out << row
        }
        return out
    }

    /**
     * Write rows as a TSV.
     *
     * @param rows    list of maps
     * @param columns column order; defaults to the union of the rows' keys
     */
    static File write(List<Map> rows, File dest, List<String> columns = null) {
        def cols = columns
        if (cols == null) {
            cols = []
            rows.each { r -> r.keySet().each { if (!cols.contains(it)) cols << it } }
        }
        def sb = new StringBuilder(cols.join("\t")).append("\n")
        rows.each { r ->
            sb.append(cols.collect { c -> cell(r[c]) }.join("\t")).append("\n")
        }
        dest.getParentFile()?.mkdirs()
        dest.setText(sb.toString(), "UTF-8")
        return dest
    }

    /**
     * One cell's text.
     *
     * A tab or newline inside a value would add or break a column silently, so
     * they are refused rather than escaped -- there is no escaping convention
     * here for a reader to have to share. null becomes blank, which is how an
     * absent measurement is written (pixel_depth on a single plane).
     */
    static String cell(Object v) {
        if (v == null) return ""
        def s = v.toString()
        if (s.contains("\t") || s.contains("\n") || s.contains("\r")) {
            throw new IllegalArgumentException(
                "A table cell may not contain a tab or newline: >>>" + s + "<<<")
        }
        return s
    }
}
