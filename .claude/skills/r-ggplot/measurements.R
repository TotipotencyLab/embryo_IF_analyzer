suppressPackageStartupMessages(library(ggplot2))
cat("ggplot2:", as.character(packageVersion("ggplot2")), " R:", R.version.string, "\n\n")

cat("--- 1. scale_colour_manual with a partial values= ---\n")
d <- data.frame(x=1:4, y=1:4, class=c("a","b","c",NA))
p <- ggplot(d, aes(x,y,colour=class)) + geom_point() +
     scale_colour_manual(values=c(a="red", b="blue"))
b <- ggplot_build(p)
cat("colours drawn :", paste(b$data[[1]]$colour, collapse=" | "), "\n")
# NB: the scale must be TRAINED, which ggplot_build() does. Asking the
# unbuilt plot object returns nothing and warns about "no shared levels".
cat("legend breaks :", paste(b$plot$scales$scales[[1]]$get_breaks(), collapse=" | "), "\n")
cat("na.value      :", b$plot$scales$scales[[1]]$na.value, "\n")
cat("rows in  :", nrow(d), "  rows plotted:", nrow(b$data[[1]]), "\n\n")

cat("--- 2. theme_bw() after vs before theme() ---\n")
after  <- ggplot(d, aes(x,y,colour=class)) + geom_point() + theme(legend.position="none") + theme_bw()
before <- ggplot(d, aes(x,y,colour=class)) + geom_point() + theme_bw() + theme(legend.position="none")
cat("theme() then theme_bw() -> legend.position:",
    format(after$theme$legend.position %||% "(unset -> default)"), "\n")
cat("theme_bw() then theme() -> legend.position:", format(before$theme$legend.position), "\n\n")

cat("--- 3. log10 with a zero present ---\n")
d2 <- data.frame(x=c(0,1,10,100), y=1:4)
b2 <- suppressWarnings(ggplot_build(ggplot(d2, aes(x,y)) + geom_point() + scale_x_log10()))
cat("rows in  :", nrow(d2), "  rows plotted:", sum(is.finite(b2$data[[1]]$x)), "\n")
cat("x values :", paste(b2$data[[1]]$x, collapse=" | "), "\n\n")

cat("--- 4. draw order ---\n")
d3 <- data.frame(x=c(1,1), y=c(1,1), g=factor(c("other","oocyte"), levels=c("other","oocyte")))
b3 <- ggplot_build(ggplot(d3, aes(x,y,colour=g)) + geom_point(size=10) +
                   scale_colour_manual(values=c(other="grey70", oocyte="red")))
cat("levels in order:", paste(levels(d3$g), collapse=" -> "), "\n")
cat("drawn in order :", paste(b3$data[[1]]$colour, collapse=" -> "), " (last is on top)\n")
