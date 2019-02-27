#' Add log-root axes to histogram plot
#'
#' @param points The data to plot
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' open_dwells <- segment.open_dwells(bursts.recombine(bursts))
#' hist(log10(open_dwells), axes=FALSE, breaks=30)
#' cplot.log_root_axes(open_dwells)
#'
#' @export
#' @importFrom graphics hist axis box
cplot.log_root_axes <- function (points) {

    tabulate <- hist(log10(points), plot = FALSE)

    gaps <- tabulate$breaks
    gaps <- gaps[-length(gaps)] # remove last gap (fencepost thing)

    counts <- tabulate$counts

    v <- 0:ceiling(sqrt(max( counts )))
    y_ticks <- v*v

    l <- floor(min(gaps))
    u <- ceiling(max(gaps))

    f <- function(v) {

        range <- log10(1:10)
        return(range + v)

    }

    label <- function (x) {
        c(c(x),rep(NaN,9))
    }
    
    x_ticks  <- unlist(sapply(l:u, f))
    x_labels <- unlist(sapply(l:u, label))

    axis(side = 1, at = x_ticks, labels=FALSE)
    axis(side = 1, at = l:u, las = TRUE)
    axis(side = 2, at = y_ticks)
    box()

}






#' Plot Time Series (ts) of P(Open).
#'
#' @param bursts List of multiple segments
#' @param main The title of the plot.
#' @param xlim Pass xlim argument to plot() to focus on window of time series
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' cplot.popen_ts(bursts, "P(Open) Time Series, 2018-09-20")
#'
#' @export
#' @importFrom graphics plot lines
cplot.popen_ts <- function(bursts, main="P(Open) Time Series", xlim=NULL) {

    XLIM <- xlim

    times  <- sapply(bursts, segment.start_time)
    popens <- sapply(bursts, segment.popen)

    plot(times,popens, main=main, ylab="P(Open)", xlab="time (s)", ylim=c(0,1), xlim = XLIM)
    lines(times, popens)

}




#' Plot Time Series (ts) of P(Closed).
#'
#' @param bursts List of multiple segments
#' @param main The title of the plot.
#' @param xlim Pass xlim argument to plot() to focus on window of time series
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' cplot.pclosed_ts(bursts, "P(Closed) Time Series, 2018-09-20")
#'
#' @export
#' @importFrom graphics plot lines
cplot.pclosed_ts <- function(bursts, main="P(Closed) Time Series", xlim=NULL) {

    XLIM <- xlim

    times  <- sapply(bursts, segment.start_time)
    pcloseds <- sapply(bursts, segment.pclosed)

    plot(times,pcloseds, main=main, ylab="P(Closed)", xlab="time (s)", ylim=c(0,1), xlim=XLIM)
    lines(times, pcloseds)

}
