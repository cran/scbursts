#' Read a MIL ".hst" file to a table.
#' 
#' Read a MIL ".hst" file to a table. By default these files are in log10(Milliseconds)-sqrt(Freq), but unless "raw" is set to TRUE, this function returns a table containing Seconds-Freq
#'
#' @param filename The filename
#' @param extract Extract either "open" or "closed" histogram
#' @param raw Data is given as log10(milliseconds)-Sqrt(Freq). Setting raw=FALSE yields output as Seconds-Frequency
#' @return A tables with columns "bin", "freq" and "fit".
#' @examples
#'
#' # import some of the data included with the package
#' infile <- system.file("extdata", "example1_hst.hst", package = "scbursts")
#' open_hst   <- hst.read(infile, extract="open")
#' closed_hst <- hst.read(infile, extract="closed")
#'
#' head(open_hst)
#' head(closed_hst)
#'
#' @export
#' @importFrom utils read.table
hst.read <- function (filename, extract="open", raw=FALSE) {

    extract <- tolower(extract)
    
    if (extract == "open") {         
        offset <- 1
    } else if (extract == "closed") {
        offset <- 5
    } else {
        stop("extract either \"open\" or \"closed\"")
    }

    ### Assumes file has the format (DOS)
    ### Notice the two blank lines and the leading tabs. Also the note DOS line endings

    #### 1     2           3           4        5      6            7          8
    
    ####	histograms bin-sqrt(hst/sum(hst))-sqrt(pdf/sum(hst)):
    ####	0.018840	0.013985	0.016598		0.018878	0.013922	0.007139	
    ####	0.023106	0.362802	0.373325		0.022677	0.096458	0.065489	
    ####	0.028338	0.292684	0.335782		0.027241	0.096458	0.071523	
    ####	0.034754	0.259760	0.291282		0.032723	0.113961	0.078059	
    ####
    ####                                       ...
    ###
    ####	4.660111	0.086209	0.102153		2.666868	0.108738	0.129563	
    ####	5.715281	0.052327	0.075907		3.203560	0.074975	0.101162	
    ####	7.009368	0.039556	0.051607		3.848258	0.053922	0.073837	
    ####	8.596470	0.027970	0.031486		4.622698	0.031132	0.049720	
    ####	10.542933	0.024223	0.016837		5.552989	0.027845	0.030405	
    ####
    ####


    # load lines
    FileInput <- readLines(filename) 

    # Notify user of the header of the file
    write(sprintf("%s header:\n\n\t%s\n", filename, FileInput[1]), stderr())
    
    # Ignore the first line
    table <- read.table(filename, skip=1, sep="\t", header=FALSE)
     
    # extract the open v.s. closed columns
    bin  <- table[,1+offset]
    freq <- table[,2+offset]
    fit  <- table[,3+offset]

    # Correct for the square root
    if (!raw) {
        bin  <- (10**bin) / 1000 # undo log10 and ms -> s
        freq <- freq**2
        fit  <- fit**2
    }
    
    data <- data.frame(bin,freq,fit)
    attr(data, "name") <- util.basename(filename)
    
    return(data)

}







#' Extract header from hst file.
#'
#' @param filename The filename
#' @return A string containing the header
#' @examples
#' 
#' # import some of the data included with the package
#' infile <- system.file("extdata", "example1_hst.hst", package = "scbursts")
#'
#' open_table <- hst.read(infile, extract="open")
#' closed_table <- hst.read(infile, extract="closed")
#' header <- hst.extract_header(infile)
#'
#' # Make adjustments to the histogram, if you wish
#' hst.write(open_table, closed_table, file=file.path(tempdir(), "output_hist.hst"), header=header)
#' 
#' @export
hst.extract_header <- function (filename) {
    # Just return the first line
    return(readLines(filename)[1])
}





#' Write bursts to a log10(ms)-sqrt(Frequency) .hst file from open and closed tables.
#'
#' @param open_hist The table (bin,freq,fit) for open times
#' @param closed_hist The table (bin,freq,fit) for closed times
#' @param file The filename
#' @param header The header info
#' @param fromraw Unless FALSE, assume we need to write a log10(milliseconds)-sqrt(Frequency) plot
#' @examples
#'
#' infile <- system.file("extdata", "example1_hst.hst", package = "scbursts")
#' 
#' open = hst.read(infile, extract="open")
#' closed = hst.read(infile, extract="closed")
#' header = hst.extract_header(infile)
#'
#' ### Do stuff
#' hst.write(open, closed, file=file.path(tempdir(), "new_histogram.hst"), header=header)
#' @export
#' @importFrom utils read.csv
hst.write <- function (open_hist, closed_hist, file="", header=NULL, fromraw=FALSE) {
    
    tabs <- rep("", length(open_hist$bin))
    
    if (!fromraw) {
        obin <- sprintf("%.6f", log10(open_hist$bin * 1000))
        ofreq <- sprintf("%.6f", sqrt(open_hist$freq))
        ofit  <- sprintf("%.6f", sqrt(open_hist$fit))

        cbin <- sprintf("%.6f", log10(closed_hist$bin * 1000))
        cfreq <- sprintf("%.6f", sqrt(closed_hist$freq))
        cfit  <- sprintf("%.6f", sqrt(closed_hist$fit))

        t <- cbind(tabs, obin, ofreq, ofit, tabs, cbin, cfreq, cfit, tabs)
    } else {
        t <- cbind(tabs, open_hist, tabs, closed_hist, tabs)
    }

    if (is.null(header)) {
        header_string <- "	histograms bin-sqrt(hst/sum(hst))-sqrt(pdf/sum(hst)):\r"
        # header_string <- gsub("\n", "\r\n", header_string)
    } else {
        header_string <- paste(header, "\r")
    }

    write(header_string, file) 
    write.table(t, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\r\n", quote = FALSE) 
    write("\r\n\r\n", file, append=TRUE)

}
