// SeriesSpec.groovy
//
// "Which series?" -- parsed once, for every script that asks.
//
// Inspect_ImageFile.groovy and Open_LifFile.groovy both take the same kind of
// answer, and a second copy of this parsing is a second set of edge cases to
// get wrong. The accepted forms:
//
//   (blank)            every series
//   5                  one
//   1 3 7              several
//   1,3,7              commas are fine too -- SciJava keeps a comma inside a
//                      quoted parameter value, unlike argparser on the R side
//   6-8                an inclusive range
//   1,4, 6-8           mixed, with whitespace wherever you like
//   name:slide20/O1_2  every series carrying exactly that name
//
// Out of range and back-to-front ranges are ERRORS naming the valid bounds,
// not silently dropped: asking for 0-2000 in a file of 1563 and quietly getting
// 1563 is how you conclude you inspected everything when you did not.

class SeriesSpec {

    /**
     * @param spec   the user's answer, any of the forms above
     * @param names  series names by index, for the `name:` form
     * @param count  how many series the file has
     * @return sorted, de-duplicated indices; never empty (it throws instead)
     */
    static List<Integer> parse(String spec, List<String> names, int count) {
        def s = (spec ?: "").trim()
        if (!s) {
            return (0..<count).toList()
        }

        if (s.toLowerCase().startsWith("name:")) {
            def want = s.substring(5).trim()
            def hits = (0..<count).findAll { (names[it] ?: "") == want }
            if (hits.isEmpty()) {
                // A trailing space in a Leica name is invisible in every
                // listing, so say whether that is what happened.
                def near = (0..<count).findAll { (names[it] ?: "").trim() == want }
                throw new IllegalArgumentException(
                    "No series is named exactly '" + want + "'." +
                    (near ? ("\n  " + near.size() + " match after trimming whitespace: " +
                             near.take(10) + " -- the name in the file has spaces around it.")
                          : "\n  Run Inspect_ImageFile.groovy to list the names."))
            }
            return hits.sort()
        }

        def out = new LinkedHashSet<Integer>()
        s.split(/[\s,]+/).findAll { it }.each { String tok ->
            def m = (tok =~ /^(\d+)\s*-\s*(\d+)$/)
            if (m.matches()) {
                int lo = m.group(1) as int, hi = m.group(2) as int
                if (lo > hi) {
                    throw new IllegalArgumentException(
                        "Range '" + tok + "' runs backwards; write it as " + hi + "-" + lo)
                }
                (lo..hi).each { out << it }
            } else if (tok ==~ /^\d+$/) {
                out << (tok as int)
            } else {
                throw new IllegalArgumentException(
                    "Cannot read '" + tok + "' as a series. Use an index (5), a range (6-8), " +
                    "a list (1,4,6-8) or a name (name:slide20/O1_2).")
            }
        }
        def bad = out.findAll { it < 0 || it >= count }
        if (bad) {
            throw new IllegalArgumentException(
                "Series " + bad.sort().take(10) + " out of range: this file has 0.." + (count - 1) + ".")
        }
        if (out.isEmpty()) {
            throw new IllegalArgumentException("No series selected by '" + s + "'")
        }
        return out.sort()
    }

    /** "1, 3, 6-8" from [1,3,6,7,8] -- for echoing a selection back. */
    static String describe(List<Integer> idx) {
        if (!idx) return "(none)"
        def sorted = idx.sort()
        def runs = []
        sorted.each { int i ->
            if (runs && runs[-1][1] == i - 1) { runs[-1][1] = i } else { runs << [i, i] }
        }
        return runs.collect { it[0] == it[1] ? "${it[0]}" : "${it[0]}-${it[1]}" }.join(", ")
    }
}
