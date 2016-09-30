# a simple function to draw logarithmic histograms
logHist <- function (what, main, xlab, ylab, breaks = 500, base = 10) {
    x <- log(what, base = base)
    par(xaxt='n', mar = c(5,4,4,4) + 0.3)
    #plot.new()
    hist(x, breaks = breaks, main = main, xlab = xlab, ylab = ylab, freq = T)
    par(xaxt='s')
    xax <- axis(1, fg = "#ffffff", col.axis = "#ffffff")
    axis(1, xax, 10 ^ xax)
    d  <- density(x, adjust = 2)
    par(new = T)
    plot(d$x, d$y, col = "#ff0000", axes = F, xlab = "", ylab = "", type = "l")
    axis(side = 4, at = pretty(range(d$y)), col = "#ff0000")
    mtext("Density", side = 4, line = 3)
}

# a simple function to draw histograms
normalHist <- function (what, main, xlab, ylab, breaks = 500, base = 10) {
    x <- what
    par(mar = c(5,4,4,4) + 0.3)
    #plot.new()
    hist(x, breaks = breaks, main = main, xlab = xlab, ylab = ylab, freq = T)
    d  <- density(x, adjust = 2)
    par(new = T)
    plot(d$x, d$y, col = "#ff0000", axes = F, xlab = "", ylab = "", type = "l")
    axis(side = 4, at = pretty(range(d$y)), col = "#ff0000")
    mtext("Density", side = 4, line = 3)
}
