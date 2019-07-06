#' Write a dwt file to disk. Writes DOS line endings. Dwells are in milliseconds
#'
#' @param segments A segment or multiple segments with $dwells and $states
#' @param file Filename to write to
#' @param seg Segment number to write in .dwt header.
#' @param append Add ot the end of a file or overwrite? (defaults to false)
#' @examples
#' 
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' 
#' dwt.write(dwells, file=file.path(tempdir(), "dwells.dwt"))
#' 
#' @export
#' @importFrom utils write.table
dwt.write <- function(segments, file="", seg=1, append=FALSE) {

    if (!is.data.frame(segments)) {

        # Erase file
        if(file.exists(file)) {
            file.remove(file)
        }
            
        for (i in 1:length(segments)) {
            dwt.write(segments=segments[[i]], file=file, seg=i, append=TRUE)
        }

    } else {

        segment <- segments
        
        header <- sprintf("Segment: %d   Dwells: %d", seg, length(segment$states))

        write(header, file, append=append) 

        states <- segment$states
        dwells <- segment$dwells

        dwells <- dwells * 1000 # seconds to milliseconds 

        dwells <- sprintf("%.6f", dwells)

        # This forces a tab to be placed at the beginning
        junk <- rep("",length(dwells))
        
        data  <- data.frame(junk,states, dwells)
        
        write.table(data, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\n", quote = FALSE) 
        
    }
}





#' Read a .dwt file.
#' 
#' Read a .dwt file. Result is a list of "segments", which is a dataframe extra data. See "segment" for more details. Converts millisecond dwells to seconds.
#'
#' @param filename Filename to read from
#' @param separating_factor In lieu of a known time between segments, seperate with a multple of the longest dwell.
#' @return A list of bursts (possibly a singleton)
#' @examples
#' 
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' 
#' dwt.write(dwells, file=file.path(tempdir(), "dwells.dwt"))
#' 
#' # Quit R, come back the next day
#' \dontrun{
#' dwells <- dwt.read("dwells.dwt")
#' }
#' 
#' @export
#' @importFrom utils read.csv
dwt.read <- function (filename, separating_factor=1000) {


    # load lines
    FileInput <- readLines(filename) 
   
    # auto-check if file is from qub

    f = substr(FileInput[2],1,1)
    c1 <- c(2,1) 
    if (f=='\t') {c1 <- c(3,2)}
    

    # header <- FileInput[[1]]

    segs <- c(which(grepl("Segment:", FileInput)), length(FileInput) + 1)

    ### Track the longest space
    max_dwell <- separating_factor
    
    bursts <- list()
    for (i in 1:(length(segs)-1)) {
        table <- read.csv(filename, skip=segs[i], nrows=segs[i+1]-segs[i]-1, sep="\t", header=FALSE)

        ### NOTE: Column 1 is empty, and thats how we get the spacing right.

        dwells <- table[,c1[[1]]]

        dwells <- dwells / 1000 # milliseconds to seconds

        ### Take the longest of all gaps, bound from below by seperating_factor itself
        max_dwell <- max(max(dwells)*separating_factor, max_dwell)
        
        states <- table[,c1[[2]]]

        bursts[[i]] <- segment.create(states, dwells, seg=i, start_time=0, name=util.basename(filename), ignore_errors = TRUE)
    }

    bursts <- bursts.start_times_update(bursts,gaps=rep(max_dwell,length(segs)-2))
 
    ## Warning Message
    if (any(lapply(bursts, segment.verify) == FALSE)) {

        if (length(which(unlist(lapply(bursts, segment.verify)) == FALSE)) == 1)
            warning(paste('Burst (or record)', (which(unlist(lapply(bursts, segment.verify)) == FALSE)), 'seems to have been misrecorded!'))
        else
            warning(paste('Bursts (or records)', (which(unlist(lapply(bursts, segment.verify)) == FALSE)), 'seem to have been misrecorded!'))
    }

    ## Warning Message
    if (any(lapply(bursts, segment.check_subconductance) == TRUE)) {

        if (length(which(unlist(lapply(bursts, segment.check_subconductance)) == TRUE)) == 1)
            warning(paste('Burst (or record)', (which(unlist(lapply(bursts, segment.check_subconductance)) == TRUE)),   'has subconductive states!'))
        else
            warning(paste('Bursts (or records)', (which(unlist(lapply(bursts, segment.check_subconductance)) == TRUE)), 'have subconductive states!'))
    }

    return(bursts)
    
}
