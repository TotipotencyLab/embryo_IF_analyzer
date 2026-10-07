// RunConfig.groovy
//
// Read a run configuration back in, in the SAME format saveRunConfig() writes:
// two columns, `parameter` and `value`, tab separated.
//
// That sameness is the whole point. Every run already drops a _config.txt
// beside its results, so "tune one image in the GUI, take its config, run the
// batch with it" needs no converter and no second vocabulary. A different
// input format would mean writing one thing and reading another, which is how
// the two drift.
//
// Three rules, all of them about failing loudly:
//
//   * An UNKNOWN key is an error. Silently ignoring `nucleus_sigma` when the
//     parameter is `nucleus_blur_sigma` is how a typo becomes a default that
//     nobody notices -- and the run then looks fine while using settings the
//     operator never chose.
//   * A key that is provenance rather than a parameter (timestamp, image_width,
//     nucleus_count ...) is IGNORED on purpose, so a whole _config.txt can be
//     fed straight back in. The list is explicit; it is not "anything I don't
//     recognise".
//   * A value that will not coerce is an error naming the key and the value.
//     Booleans especially: in Groovy a non-empty String is truthy, so the
//     string "false" read out of a file would enable whatever it guards.
//
// This class is deliberately free of ImageJ imports -- it is text handling, and
// staying dependency-free keeps it testable without an image.

class RunConfig {

    /** Values saveRunConfig() writes that describe the RUN, not the request. */
    static final List<String> PROVENANCE_KEYS = [
        "timestamp", "script", "imagej_version",
        "image_title", "image_width", "image_height", "image_slices", "image_channels",
        "image_frames", "frames_analysed", "frame_interval", "frame_unit",
        "open_method", "source_file", "series_index", "series_name",
        "pixel_width", "pixel_height", "pixel_depth", "pixel_unit",
        "series_id", "output_basename", "z_slices_analysed", "measurements",
        "overview_saved", "overview_channels", "overview_overlay_suffix", "overview_display_range",
        "nucleus_threshold_used", "nucleus_mask_pct", "nucleus_circ_rejected",
        "nucleus_count", "nucleolus_count",
    ]

    /**
     * Parameters that USED to exist, and why they went. A config naming one is
     * read, the key skipped, rather than refused as unknown: every _config.txt
     * written before v0.7.0 carries `position_pattern`, and refusing it would
     * strand every config anyone tuned. Unknown keys stay an error -- a retired
     * key is a known one. The caller reports what was skipped (retiredIn()),
     * since this class does not log.
     */
    static final Map<String, String> RETIRED_KEYS = [
        output_prefix   : "v0.7.0: output is named by the series id alone; " +
                          "type a prefix into the id, or use another output directory",
        position_pattern: "v0.7.0: an interactive run takes its series id from the " +
                          "image title, or from what is typed into 'Series id'",
    ]

    /** The retired keys a raw config sets, so the caller can say they were skipped. */
    static List<String> retiredIn(Map<String, String> raw) {
        return raw.keySet().findAll { RETIRED_KEYS.containsKey(it) }.toList()
    }

    /** The two-column text a config file holds. Kept beside parse() so the
     *  round trip is one file's business; RoiExport.saveRunConfig() writes the
     *  same shape and Test_RunConfig pins the two together. */
    static String format(Map<String, Object> params) {
        def sb = new StringBuilder("parameter\tvalue\n")
        params.each { k, v -> sb.append(k).append("\t").append(v == null ? "" : v.toString()).append("\n") }
        return sb.toString()
    }

    /**
     * Parse a config file into raw strings, in file order.
     *
     * Blank lines and `#` comments are skipped, so a config can be annotated by
     * the person editing it. The `parameter\tvalue` header is optional.
     */
    static Map<String, String> parse(String text, String whence = "config") {
        def out = new LinkedHashMap<String, String>()
        int lineNo = 0
        text.readLines().each { String raw ->
            lineNo++
            def line = raw.trim()
            if (!line || line.startsWith("#")) return
            if (!raw.contains("\t")) {
                // The commonest mistake, and it must not look like an unknown
                // key: say what the format actually is.
                throw new IllegalArgumentException(
                    whence + " line " + lineNo + " has no tab: >>>" + raw + "<<<\n" +
                    "  A run config is two TAB-separated columns, `parameter` and `value` -- " +
                    "the same file saveRunConfig() writes. `key = value` is not the format.")
            }
            def parts = raw.split("\t", -1)
            def k = parts[0].trim()
            def v = (parts.length > 1) ? parts[1].trim() : ""
            if (k == "parameter" && v == "value") return      // the header
            if (!k) return
            if (out.containsKey(k)) {
                throw new IllegalArgumentException(
                    whence + " sets '" + k + "' twice (line " + lineNo + "); " +
                    "the second would silently win")
            }
            out[k] = v
        }
        return out
    }

    static Map<String, String> read(File f) {
        if (!f.isFile()) {
            throw new IllegalArgumentException("No such config file: " + f.getAbsolutePath())
        }
        return parse(f.getText("UTF-8"), f.getName())
    }

    /**
     * Split a raw config into the parameters it sets, coerced to `types`.
     *
     * @param raw    from parse()
     * @param types  parameter name -> "string" | "int" | "double" | "boolean"
     * @param whence for messages
     * @return the parameters only; provenance and retired keys are dropped
     */
    static Map<String, Object> params(Map<String, String> raw, Map<String, String> types,
                                      String whence = "config") {
        def unknown = raw.keySet().findAll { !types.containsKey(it) && !PROVENANCE_KEYS.contains(it) &&
                                             !RETIRED_KEYS.containsKey(it) }
        if (unknown) {
            throw new IllegalArgumentException(
                whence + " has unknown parameter(s): " + unknown.sort().join(", ") + "\n" +
                "  known parameters: " + types.keySet().sort().join(", ") + "\n" +
                "  (provenance fields such as timestamp or nucleus_count are ignored, " +
                "so a whole _config.txt can be fed back in)")
        }
        def out = new LinkedHashMap<String, Object>()
        raw.each { k, v ->
            if (!types.containsKey(k)) return                  // provenance, or retired
            out[k] = coerce(k, v, types[k], whence)
        }
        return out
    }

    /** Coerce one value, erroring rather than guessing. */
    static Object coerce(String key, String value, String type, String whence = "config") {
        def bad = { String why ->
            throw new IllegalArgumentException(
                whence + ": " + key + "=" + (value ?: "(blank)") + " is not " + why)
        }
        switch (type) {
            case "string":
                return value
            case "int":
                if (!(value ==~ /^[+-]?\d+$/)) bad("a whole number")
                return value as Integer
            case "double":
                if (!(value ==~ /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/)) bad("a number")
                return value as Double
            case "boolean":
                // Never silently false: an unrecognised word here would quietly
                // disable the thing it names.
                def t = value?.toLowerCase()
                if (t in ["true", "yes", "1"]) return true
                if (t in ["false", "no", "0"]) return false
                bad("a boolean (true/false, yes/no, 1/0)")
            default:
                throw new IllegalArgumentException("Unknown parameter type '" + type + "' for " + key)
        }
    }

    /** Read a file straight through to coerced parameters. */
    static Map<String, Object> readParams(File f, Map<String, String> types) {
        return params(read(f), types, f.getName())
    }

    /**
     * Write the parameters actually used, as a file that can be read back.
     *
     * Separate from the per-image _config.txt on purpose: that one is the output
     * contract and mixes parameters with results, while this is exactly the set
     * a later run would need. Feeding either back in works; this one has nothing
     * to ignore.
     */
    static File writeParams(Map<String, Object> params, File dest) {
        dest.getParentFile()?.mkdirs()
        dest.setText(format(params), "UTF-8")
        return dest
    }
}
