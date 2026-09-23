---
name: r-ggplot
description: ggplot2 failures that render successfully and misrepresent the data - incomplete scale_*_manual dropping levels from the legend, theme_bw() replacing an earlier theme(), log scales discarding non-positive values, legends stealing panel width, draw order as occlusion order - plus the house style for multi-page PDFs via pdf(onefile=TRUE). Use when writing or reviewing any plotting code in scripts/R/plot_*.r or a CLI that draws.
---

# R plotting: the failures that look like success

Every entry here was **measured in this repo**, not recalled. They share one
shape, and it is the shape this repo cares about everywhere else:

> The code runs. No error, no warning. A finished-looking figure comes out, and
> it is not telling you the truth about the data.

A plot that errors costs you a minute. A plot that lies costs you a conclusion.
So the rule throughout is the same as for the analysis code: **when a plotting
decision changes what the reader sees, the plot has to say so** — in a label, a
subtitle, a caption, or a warning.

`MEASUREMENTS.md` beside this file holds the reproductions: short scripts and
their exact output. Read it when you want the evidence rather than the rule.

---

## ⚠️ `scale_*_manual` silently deletes levels it was not given

The worst one, because the figure actively misrepresents what is present.

```r
d <- data.frame(x = 1:4, y = 1:4, class = c("a", "b", "c", NA))
ggplot(d, aes(x, y, colour = class)) + geom_point() +
  scale_colour_manual(values = c(a = "red", b = "blue"))
```

Measured:

```
point colours drawn:  red | blue | grey50 | grey50
legend breaks:        a | b | NA
```

Class `c` is real and present. It is drawn in `na.value` grey, **identical to
genuine `NA`**, and it is **missing from the legend entirely**. Nothing warns.
A reader cannot tell that the class exists, let alone which points are in it.

**Never pass a partial `values`.** Build the map over the levels actually
present:

```r
lev <- levels(factor(d$class))
pal <- setNames(rep("grey70", length(lev)), lev)
pal[names(user_map)] <- user_map          # user_map is what was asked for
```

When you want unmapped levels to collapse into one legend entry, **recode the
data**, do not rely on `na.value`:

```r
d$class <- factor(ifelse(is.na(d$class) | !d$class %in% names(user_map),
                         "other", d$class),
                  levels = c(names(user_map), "other"))
```

Then say in a caption which levels are inside `other`. Folding them together is
a presentation choice; hiding which ones were folded is a lie.

## ⚠️ `theme_bw()` replaces everything an earlier `theme()` set

`theme_bw()` is a **complete** theme, not a modifier. Adding it discards prior
`theme()` calls:

```r
p + theme(legend.position = "none") + theme_bw()   # legend is BACK
p + theme_bw() + theme(legend.position = "none")   # correct
```

Complete themes are `theme_grey`, `theme_bw`, `theme_linedraw`, `theme_light`,
`theme_dark`, `theme_minimal`, `theme_classic`, `theme_void`. Put the complete
theme first and every tweak after it. Cost here: a legend suppression that
silently came back, caught only because a test asserted the kept case too.

## ⚠️ A log scale discards non-positive values

`scale_*_log10()` drops `<= 0` rows. On a scatter that is missing points; on a
box plot it is a shifted summary. It looks like missing data, not a scale
choice.

Decide before drawing, fall back, and **say so**:

```r
if (use_log && any(v <= 0, na.rm = TRUE)) {
  warning("Not logging ", col, ": ", sum(v <= 0, na.rm = TRUE),
          " value(s) are <= 0 and would be dropped", call. = FALSE)
  use_log <- FALSE
}
```

And **name the transform on the axis** (`area_sum (log10)`). Two panels of the
same quantity, one logged and one not, are otherwise indistinguishable — which
is exactly how a units bug went unnoticed here: `area_sum` was logged and
`volume` was not, although `volume` *is* `area_sum x z_step`.

## Log-by-name vs log-by-data

Do neither automatically. Both were tried here and both were removed:

- **by name** (`grepl("^area_", col)`) — two columns holding the same quantity
  in different units get different axes, because they are spelled differently.
- **by data** (span > ~20x) — unit-invariant, which is the property the name
  rule lacks, but it logs slice indices and splits families that belong
  together (`area_med` at 27.9x logged, `area_mean` at 19.4x not).

Take the columns as an argument, report each column's **span** (max/min over
positive values) so the choice is informed, and log nothing unasked. Span, not
range: span does not change when the units do.

## ⚠️ The legend takes its width from the panel

Not cosmetic. Forty colour levels leave a sliver of chart. Cap the number of
levels that get a key, drop it past the cap, and **record the omission in the
subtitle** — the colour mapping still carries the grouping, but a reader
otherwise hunts for a key that was never going to be there.

A legend is also redundant when `colour` and `facet` map the same column: the
strip already names each panel.

## ⚠️ Draw order is occlusion order

Points drawn last sit on top. De-emphasised greys added after the classes of
interest will cover them, and nothing about the code looks wrong. Draw the
background category first — for a recoded factor, that means ordering the
levels deliberately rather than alphabetically.

## Position labels per group, not per plot

`n=` labels placed at a global maximum float far from their box on a log scale.
Compute the anchor within each group.

## House style: multi-page PDFs

One device, one page per plot, closed on exit. **Not** `ggsave()` per page.

```r
save_plot_list <- function(plots, path, width = 7, height = 5) {
  grDevices::pdf(path, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (nm in names(plots)) {
    ok <- tryCatch({ print(plots[[nm]]); TRUE },
                   error = function(e) { warning("page '", nm, "': ",
                                                 conditionMessage(e),
                                                 call. = FALSE); FALSE })
  }
  invisible(path)
}
```

Why it is written this way:

- `onefile = TRUE` gives one file rather than `Rplots%03d.pdf`.
- `on.exit(dev.off())` — an error mid-loop otherwise leaves the device open,
  and every later plot in the session silently lands in the half-written PDF.
- the per-page `tryCatch` means one bad panel costs one page, not the document.
- `print()` is required: a ggplot object returned from a loop is not drawn.

⚠️ **Never read a PDF back with `readLines()` to check it** — invalid UTF-8
breaks testthat's own error formatting, so the test fails with a confusing
message about encoding instead of about the plot. Match bytes:

```r
bytes <- readBin(path, "raw", file.size(path))
expect_length(grepRaw(charToRaw("/Count 3"), bytes, all = TRUE), 1)
```

## Testing a plot without rendering it

`ggplot_build(p)` gives the computed data; `p$labels`, `p$theme` and
`p$scales` give the decisions. Assert those rather than eyeballing output:

```r
expect_identical(p$labels$x, "volume (log10)")
expect_identical(p$theme$legend.position, "none")
b <- ggplot_build(p)
expect_identical(nrow(b$data[[1]]), nrow(d))    # nothing was dropped
```

That last line is the important habit: **assert that the number of plotted
points equals the number of rows you meant to plot.** Most of the failures on
this page show up as points quietly disappearing.

⚠️ And assert the *kept* case alongside the dropped one. "The legend is absent"
passes on a plot that never had a legend.
