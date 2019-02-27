#' Read a .evt file to a table. Times are in seconds
#'
#' @param filename The filename
#' @return A list of tables with columns "states" and "times".
#' Each table corresponds to a contiguous segment from a recording.
#' @examples
#'
#' # import some of the data included with the package
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#'
#' head(transitions[[1]])
#'
#' @export
#' @importFrom utils read.csv tail
evt.read <- function (filename) {
    
    ### Assumes file has the format

    ### blah blah blah
    ### 
    ### ... 
    ###
    ### Events
    ### 1	0.05027597	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05032199	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05035479	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05090435	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05092179	6.346019E-012	1.943602E-011	1	0.000000E+000

    # load lines
    FileInput <- readLines(filename) 

    # Jump to where the data starts
    skip_line <- tail(grep("^Events$", FileInput), n=1)

    # Read everything past 'Events'
    table <- read.csv(filename, skip=skip_line, sep="\t",header=FALSE)
     
    # extract the needed columns
    all_states <- table[,5]
    all_times  <- table[,2]
    segs <- c(1, which(diff(table[,1]) > 0)+1, length(table[,1])+1)

    segments <- list()
    for (i in 1:(length(segs)-1)) {
        states <- all_states[(segs[i]):(segs[i+1]-1)]
        times <-  all_times[(segs[i]):(segs[i+1]-1)]

        data <- data.frame(states, times)
        attr(data, "name") <- util.basename(filename)
        segments[[i]] <- data
    }
    
    return(segments)

}







#' Extract header from evt file.
#'
#' @param filename The filename
#' @return A string containing the header
#' @examples
#' 
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#'
#' # Get Dwells
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' # Get Header
#' header <- evt.extract_header(infile)
#'
#' evt.write(dwells_c, header=header, file=file.path(tempdir(), "fixed_example1_tac.evt"))
#' @export
#' @importFrom utils tail
evt.extract_header <- function (filename) {
    
    ### Assumes file has the format

    ### blah blah blah
    ### 
    ### ... 
    ###
    ### Events
    ### 1	0.05027597	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05032199	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05035479	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05090435	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05092179	6.346019E-012	1.943602E-011	1	0.000000E+000

    # load lines
    FileInput <- readLines(filename) 

    # Jump to where the data starts
    header_end <- tail(grep("^Events$", FileInput), n=1)

    header_string <- paste(paste(FileInput[1:header_end], collapse='\r\n'), "\r", sep="")
    
    return(header_string)

}





#' Write bursts to a .evt file.
#'
#' @param segments A segment or list of segments to write to filename
#' @param filename The filename
#' @param header The header information for the evt file, if available
#' @examples
#' 
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#'
#' # Get Dwells
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' # Get Header
#' header <- evt.extract_header(infile)
#'
#' evt.write(dwells_c, header=header, file=file.path(tempdir(), "fixed_example1_tac.evt"))
#' @export
#' @importFrom utils read.csv
evt.write <- function (segments, filename="", header=NULL) {
    
    # Later code assumes that there are multiple segments
    if (is.data.frame(segments))
        segments <- list(segments)
    
    if (is.null(header)) {

        header_string <- "5
File
Acquire	Z:\\nonsense.dat	0	
Sweeps
"
        
        for (i in 1:length(segments))
            header_string <- paste(header_string, sprintf("%d	0	0	0	0	0
", i),sep="")
        
        header_string <- paste(header_string, "Segments
",sep="")
        for (i in 1:length(segments))
            header_string <- paste(header_string, sprintf("%d	0	0	0	0	0
", i),sep="")

        header_string <- paste(header_string, "Events\r",sep="")

        ## Use DOS line endings
        header_string <- gsub("\n", "\r\n", header_string)

    } else {
        header_string <- header
    }

    
    write(header_string, filename) 

    for (i in 1:length(segments)) {

        segment <- segments[[i]]
        
        data <- evt.from_dwells(segment)

        times <- data$times

        times <- sprintf("%.8f", times)

        states <- data$states


        ## This forces a tab to be placed at the beginning
        col1 <- rep(i, length(times))
        col3 <- rep("0.000000E+000",length(times))
        col4 <- rep("0.000000E+000",length(times))
        col6 <- rep("0.000000E+000",length(times))
        
        data  <- data.frame(col1, times, col3, col4, states, col6)

        write.table(data, filename, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\r\n", quote = FALSE) 

    }
}








#' Calculate pulse lengths. Converts transition times to dwell durations.
#'
#' @param tables Either a single table or a list of tables with columns "states" and "times"
#' @return A segment or a list of segments with one less row, where each row
#' represents pulse in state 0 (closed dwell) of duration 0.51231, instead
#' of the time at which the state transitioned.
#'
#' @examples
#' 
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' head(dwells[[1]])
#'
#' @export
evt.to_dwells <- function(tables) {

    if (!is.data.frame(tables)) {

        dwells <- lapply(tables, evt.to_dwell)

        ## Warning Message
        if (any(lapply(dwells, segment.verify) == FALSE)) {

            if (length(which(unlist(lapply(dwells, segment.verify)) == FALSE)) == 1)
                warning(paste('Dwell', (which(unlist(lapply(dwells, segment.verify)) == FALSE)), 'seems to have been misrecorded!'))
            else
                warning(paste('Dwells', (which(unlist(lapply(dwells, segment.verify)) == FALSE)), 'seem to have been misrecorded!'))
        }

        return(dwells)
        
    } else {

        segment <- evt.to_dwell(tables)

        if (segment.verify(segment) == FALSE)
            warning('This segment has been misrecorded!')
        
        return(segment)
    }
}







## Not Exported. Call evt.to_dwells instead
evt.to_dwell <- function(table) {

    states <- table$states
    times  <- table$times
    
    ## Calculate the durations, the last one gets thrown away
    dwells <- diff(times)

    ## remove the first pulse, and ignore the trailing end-dwell
    states <- states[1:length(states)-1]

    ## NOTE: the use of "name" here, is kinda an abuse.
    if (!is.null(segment.name(table))) {
        segment <- segment.create(states, dwells, name=segment.name(table))
    } else {
        segment <- segment.create(states, dwells)
    }
    
    return(segment)
}








#' Converts dwell durations to absolute transition times.
#'
#' @param segments A segment or multiple segemtns
#' @return A dataframe or multiple dataframes of states and transition times
#' @examples
#' 
#' dwells_file <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(dwells_file)
#' 
#' transitions <- evt.from_dwells(dwells)
#'
#' @export
#' @importFrom stats diffinv
evt.from_dwells <- function(segments) {

    if (!is.data.frame(segments)) {

        return(lapply(segments, evt.from_dwells))

    } else {

        segment <- segments
        
        times <- diffinv(segment$dwells)

        ## The last dwell is always a 1, meaning at the end you transition to 0.
        states <- append(segment$states, 0)

        data  <- data.frame(states, times)

        return(data)
        
    }
}
