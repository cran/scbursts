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
#' @param ... Options to pass to plot
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
cplot.popen_ts <- function(bursts, main="P(Open) Time Series", ...) {

    times  <- sapply(bursts, segment.start_time)
    popens <- sapply(bursts, segment.popen)

    plot(times,popens, main=main, ylab="P(Open)", xlab="time (s)", ylim=c(0,1), ...)
    lines(times, popens)

}




#' Plot Time Series (ts) of P(Closed).
#'
#' @param bursts List of multiple segments
#' @param main The title of the plot.
#' @param ... Options to pass to plot
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' cplot.pclosed_ts(bursts, main="P(Closed) Time Series, 2018-09-20")
#'
#' @export
#' @importFrom graphics plot lines
cplot.pclosed_ts <- function(bursts, main="P(Closed) Time Series", ...) {

    times  <- sapply(bursts, segment.start_time)
    pcloseds <- sapply(bursts, segment.pclosed)

    plot(times,pcloseds, ylab="P(Closed)", xlab="time (s)", ylim=c(0,1), main=main, ...)
    lines(times, pcloseds)

}



#' Histogram of Conductance States
#'
#' @param bursts List of multiple segments
#' @param ... other arguments passed to histogram
#' @examples
#'
#' infile <- system.file("extdata", "example4.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' cplot.conductance_hist(bursts, main="example4.dwt conductance state histogram")
#'
#' @export
cplot.conductance_hist <- function(bursts, ...) {

    ### bursts  is a list
    if (!is.data.frame(bursts)) {

        get_states <- function (seg) { seg$states }

        ### The gaps between bursts are not otherwise included
        gaps <- rep(0, length(bursts)+1)
        states <- c(unlist(sapply(bursts, get_states)), gaps)

        range <- bursts.conductance_states(bursts)
        
    } else {
        states <- bursts$states
        range <- segment.conductance_states(bursts)
    }
    hist(states, xlab=paste("Conductance states = ", toString(range)), ...)
}


