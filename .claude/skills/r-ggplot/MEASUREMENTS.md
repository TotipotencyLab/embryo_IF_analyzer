# Reproductions

Everything in `SKILL.md` is measured, not recalled. These are the scripts and
their exact output, so a claim can be rechecked rather than believed.

Run under **ggplot2 4.0.3, R 4.6.1** on this machine. Re-measure before trusting
any of it against a different ggplot2 — several of these are behaviours, not
documented guarantees.

```bash
Rscript .claude/skills/r-ggplot/measurements.R
```

---

## 1. `scale_colour_manual` with a partial `values=`

```r
d <- data.frame(x = 1:4, y = 1:4, class = c("a", "b", "c", NA))
p <- ggplot(d, aes(x, y, colour = class)) + geom_point() +
     scale_colour_manual(values = c(a = "red", b = "blue"))
b <- ggplot_build(p)
```

```
colours drawn : red | blue | grey50 | grey50
legend breaks : a | b | NA
na.value      : grey50
rows in  : 4   rows plotted: 4
```

Class `c` is present in the data and in the plot. It is drawn in `na.value`
grey — **the same grey as the genuine `NA`** — and it does **not appear in the
legend**. No error, no warning.

Note `rows plotted: 4`: nothing was dropped, which is why the usual
"did I lose points?" check does not catch this one. The points are all there
and two of them are lying about which group they belong to.

⚠️ **The scale must be trained before you can ask it anything.**
`p$scales$scales[[1]]$get_breaks()` on the *unbuilt* plot returns nothing and
warns `No shared levels found between names(values) of the manual scale and
the data's colour values`. Ask `ggplot_build(p)$plot$scales` instead. Worth
knowing when writing a test: the untrained answer looks like a finding.

## 2. `theme_bw()` relative to `theme()`

```r
after  <- ... + theme(legend.position = "none") + theme_bw()
before <- ... + theme_bw() + theme(legend.position = "none")
```

```
theme() then theme_bw() -> legend.position: right
theme_bw() then theme() -> legend.position: none
```

`right` is the default: the suppression was discarded. Complete themes replace
the whole theme, so they go first and tweaks go after.

## 3. A log scale with a zero present

```r
d2 <- data.frame(x = c(0, 1, 10, 100), y = 1:4)
ggplot(d2, aes(x, y)) + geom_point() + scale_x_log10()
```

```
rows in  : 4   rows plotted: 3
x values : -Inf | 0 | 1 | 2
```

The zero becomes `-Inf` and the point is not drawn. The remaining values are
the log10 transforms of 1, 10 and 100. One of four observations left the figure
and the only sign is a generic removed-rows warning that is easy to suppress
and easier to ignore.

This is why both plot paths here decide *before* drawing, fall back to linear,
and name the fallback in a warning and in the axis label.

## 4. Draw order is level order

```r
d3 <- data.frame(x = c(1, 1), y = c(1, 1),
                 g = factor(c("other", "oocyte"), levels = c("other", "oocyte")))
```

```
levels in order: other -> oocyte
drawn in order : grey70 -> red  (last is on top)
```

Two points at the same coordinates; only the last drawn is visible. Factor
level order decides it, and the default is alphabetical — so a de-emphasised
`other` category will cover the classes of interest whenever its name happens
to sort later. Set the levels deliberately, background first.

---

## What these have in common

Three of the four leave the row count untouched. The figure has the right
number of points, no warning is raised, and the picture is wrong anyway. The
only reliable defence is to assert the *decisions* — labels, scales, themes,
level order — rather than to look at the output and judge it plausible.
