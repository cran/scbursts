#' Copy a list of bursts (by value)
#'
#' @param bursts bursts to copy
#' @return A copy of the bursts.
#' @export
bursts.copy <- function (bursts) {
    lapply(bursts, segment.copy)
}


#'
#' Divide a recording into bursts defined by a critical time.
#' 
#' Split segment at long pauses, dividing the segment into multiple -shorter- segments (which are the bursts), Along with the interburst closings, which are referred to as "gaps". (Default time units are seconds)
#'
#' @param segments A segment or multiple segments with $states and $dwells.
#' NOTE: separate segments will remain split, regardless of why they were originally divided.
#' @param t_crit Critical time at which to divide bursts (in seconds by default)
#' @param units what unit the critical time is in ('s','ms','us', or 'ns')
#' @return bursts. Which is a list of segments
#' starting and ending in 1 states (open dwell)
#' @examples
#'
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' head(bursts[[1]])
#'
#' @export
bursts.defined_by_tcrit <- function(segments, t_crit, units="s") {
 
    if (units == "s") {         
        t_crit = t_crit
        warning(sprintf("UNITS: Using t_crit = %f SECONDS", t_crit))
    } else if (units == "ms") {
        t_crit = t_crit / 1000
    } else if (units == "us") {
        t_crit = t_crit / 1000000
    } else if (units == "ns") {
        t_crit = t_crit / 1000000000
    } else {
        stop("units must be either 's', 'ms', 'us', or 'ns'")
    }

    if (!is.data.frame(segments)) {
        ## Merge all the bursts into one long list.
        if (length(segments) > 1) {
            warning('Merging all recordings into one recording. A large (but arbitrary) amount of time will seperate the recordings.')
        }
        segment <- bursts.recombine(bursts.space_out(segments, sep_factor = max(10, 10*t_crit)))
    } else {
        segment <- segments
    }
    

    ### Find all gaps
    gap_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- gap_func(segment)
    pauses <- c(c(TRUE),pauses,c(TRUE)) ### Causes first and last burst to be included



    # Extract the gaps
    filter_gaps <- function(i) {

        if (i == 1) {
            return (NULL)
        } else if (i == length(pauses)) {
            return (NULL)
        }
        
        if (pauses[i]) {
            return(segment$dwells[i-1])
        } else {
            return (NULL)
        }
    }

    gaps <- Filter(Negate(is.null), sapply(1:length(pauses),filter_gaps))
    gaps <- unlist(gaps, use.names=FALSE)

    
    
    ### Turn gaps into selected regions (indices only)
    find_next <- function(n) {

        ## Not on a pulse
        if (!isTRUE(pauses[n])) {
            return (NULL)
        }
            
        if (n == length(pauses)) {
            return(NULL)
        }
       
        for (i in (n+1):(length(pauses))) {
            if (pauses[i]) {
                # n is the n+1^nth index of the segment, and i the i+1^st
                # So (n+1:i-1) in the segment -> (n:i-2)
                return (n:(i-2))   
            }
        }
        return(NULL)
    }
    
    ### Create list of bursts (indices only)
    burst_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))


    ### Select the bursts using the indices
    burst <- function(i) {

        df <- segment[unlist(burst_selectors[i]),]

        s <- segment.create(
            df$states,
            df$dwells,
            seg=i,
            start_time=0,
            name=segment.name(segment),
            ignore_errors=TRUE
        )

        return(s)
    }
    
    bursts <- lapply(seq_along(burst_selectors), burst)
    bursts <- bursts.start_times_update(bursts, gaps)
    
    ## Warning Message
    if (any(lapply(bursts, segment.verify) == FALSE)) {

        if (length(which(unlist(lapply(bursts, segment.verify)) == FALSE)) == 1)
            warning(paste('Burst', (which(unlist(lapply(bursts, segment.verify)) == FALSE)), 'seems to have been misrecorded!\n'))
        else
            warning(paste('Bursts', toString(which(unlist(lapply(bursts, segment.verify)) == FALSE)), 'seem to have been misrecorded!\n'))
    }

        ## Warning Message
    if (any(lapply(bursts, segment.check_subconductance) == TRUE)) {

        if (length(which(unlist(lapply(bursts, segment.check_subconductance)) == TRUE)) == 1)
            warning(paste('Burst (or record)', (which(unlist(lapply(bursts, segment.check_subconductance)) == TRUE)),   'has subconductive states!\n'))
        else
            warning(paste('Bursts (or records)', toString(which(unlist(lapply(bursts, segment.check_subconductance)) == TRUE)), 'have subconductive states!\n'))
    }
    
    return(bursts)

}

#'
#' (DON'T USE THIS) Fix meta-data of bursts.
#'
#' YOU PROBABLY WON'T EVER HAVE TO CALL THIS DIRECTLY. Attach the meta-data to each segment saying when it began. It interleaves the durations of the bursts and gaps, and assigns the sum of those durations up to a point as the starting time.
#'
#' @param bursts List of segments
#' @param gaps vector of gap times.
#' @return A list of segments, one per burst, with updated start_times
#' @export
bursts.start_times_update <- function (bursts, gaps) {

    starting_time <- function(i) {
        if (i == 1) {
            return(0)
        } else {
            t <- segment.start_time(bursts[[i-1]]) + sum(bursts[[i-1]]$dwells) + gaps[i-1]
            return(t)
        }
    }

    ### CANNOT BE PARALLELIZED!
    for (i in 1:length(bursts)) {
        ### NOTE: Probably should create an actual setter method
        attr(bursts[[i]],"start_time") <- starting_time(i)
    }

    return(bursts)

}




#' Get the gaps between bursts.
#' 
#' Extract vector of gaps from the bursts. This is done using the start_time attribute, which is mostly hidden in the data. (The gaps at the ends may have length 0)
#'
#' @param bursts The list of segments
#' @return A vector of N+1 gaps for N bursts times
#' @examples
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' gaps <- bursts.get_gaps(bursts)
#'
#' head(gaps)
#' @export
bursts.get_gaps <- function (bursts) {

    if (length(bursts) == 0) {
        warning("list not long enough to extract bursts from")
        return(list())
    }
    
    start_times <- sapply(bursts, segment.start_time)
    if (length(start_times) > 1 && sum(start_times) <= 0) {
        warning("These have nonsensical starting times!!!
        It is not clear how much time transpired inbetween them

        Consider looking at any of the following for solutions:

        > help(bursts.defined_by_tcrit)
        > help(bursts.space_out)

        Or (for more technical uses)

        > help(bursts.start_times_update)")
        stop("Exiting. Need to fix the metadata of the bursts.")
    }

    durations   <- sapply(bursts, segment.duration)

    diff_start <- diff(start_times)
    head_durations <- durations[1:length(durations)-1]
    
    gaps <- diff_start - head_durations

    return(gaps)
    
}









#' Remove the first and last burst from the list.
#'
#' @param bursts The list of all bursts
#' @return A shorter list of bursts
#' @examples
#'
#' infile <- system.file("extdata", "example1_tac.evt", package = "scbursts")
#' transitions <- evt.read(infile)
#' dwells <- evt.to_dwells(transitions)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#'
#' # If there seem to be bad bursts at the ends
#' bursts <- bursts.remove_first_and_last(bursts)
#' 
#' @export
bursts.remove_first_and_last <- function (bursts) {
    if ( length(bursts) <= 2 ) {
        return(list())
    } else {
        return(bursts[2:length(bursts)-1])
    }
}





#' Combine bursts into one recording (with obvious spaces between them).
#' 
#' From a list of segments, return the concatenated  segment containing all bursts. Inverse of functions like bursts.defined_by_tcrit
#'
#' @param bursts The list of all bursts
#' @return The segment containing all bursts.
#' @examples
#' 
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#'
#' # This is a single segment!
#' record <- bursts.recombine(bursts)
#'
#' # Which means you can do stuff like this
#' open_dwells <- segment.open_dwells(bursts.recombine(bursts))
#' 
#' @export
bursts.recombine <- function (bursts) {

    ## This is a silly way to do it, but it works!

    all <- function (x) { return(TRUE) }

    return ( bursts.select(bursts, all, one_file=TRUE) )

}






#' Artificially add amount of time between bursts (in absence of recording information).
#' 
#' Given a list of segments separated by an unknown amount of time, one may want to space the segments by some amount of time, so that they can be plotted. This function takes a separating factor, and splits up the segments by either that factor (in seconds), or that many multiples of the largest observed dwell.
#'
#' @param segments The segments to space out
#' @param sep_factor the factor by which to separate the segments.
#' Either the factor in seconds, or a multiple of the longest observed dwell.
#' @return The segments again, but with modified meta-data.
#' @examples
#' infile <- system.file("extdata", "example_multiple_segments.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#'
#' # Still a list, but the meta-data is fixed
#' spaced_records <- bursts.space_out(dwells, sep_factor=1000)
#'
#' # Combine them, and they'll be nicely spaced out.
#' single_record <- bursts.recombine(spaced_records)
#'
#' # You can now plot that single_record using one of the plot functions.
#' 
#' @export
bursts.space_out <- function (segments, sep_factor=1000) {

    max_dwell <- sep_factor

    for (s in segments) {
        max_dwell <- max( max(s$dwells) * sep_factor, max_dwell )
    }
    
    segments <- bursts.start_times_update(segments, gaps=rep(max_dwell, length(segments)-1))

    return(segments)
    
}







#' From a list of bursts, extract those that interest you by passing a selecting function.
#'
#' @param bursts The list of all bursts
#' @param func A function of a segment that returns either TRUE or FALSE
#' @param one_file TRUE or FALSE: Return a single file to write to disk, or a list of bursts.
#' The one_file will return a file with all unselected bursts zeroed out.
#' @return A shorter list of bursts OR if one_file is passed one segment with zeros where the other bursts might have been originally. Defaults to FALSE.
#' @examples
#' 
#' high_popen <- function (seg) {
#'     segment.popen(seg) > 0.7
#' }
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#'
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' subset <- bursts.select(bursts, high_popen)
#'
#' # To export to one .dwt file
#' subset_f <- bursts.select(bursts, high_popen, one_file=TRUE)
#' 
#' @export
bursts.select <- function (bursts, func, one_file=FALSE) {

    ## "Filter" is ambiguous in the terriory of ion-channel analysis, and so
    ## it's prefereable to use "select" instead.

    filtered <- Filter(func, bursts)
    
    if (!one_file) {
        return(filtered)
    }

    if (length(filtered) == 0) {
        return (filtered)
    }
    
    gaps <- bursts.get_gaps(filtered)

    ##### We MIGHT be missing the first and last gap. #####

    ## Add the first gap (if necessary)
    start <- segment.start_time(filtered[[1]])
    if (start != 0) {

        gaps <- append(start, gaps)

        interleave_gaps_first <- TRUE
        
    } else {

        interleave_gaps_first <- FALSE

    }


    ## Add the last gap (if necessary)
    last_burst    <-   bursts[[length(bursts)]]
    last_filtered <- filtered[[length(filtered)]]
    if (segment.start_time(last_filtered) != segment.start_time(last_burst)) {

        end <- segment.start_time(last_burst) + segment.duration(last_burst)      
        len <- end - (segment.duration(last_filtered) + segment.start_time(last_filtered))
        gaps <- append(gaps, len)

    } 
    
    
    faux_segment <- function (dwell) {
        segment.create(c(0),c(dwell), ignore_errors=TRUE)
    }

    ## list of size one dataframes
    faux_segs <- lapply(gaps, faux_segment)

    ## https://stackoverflow.com/questions/16443260/interleave-lists-in-r
    if (interleave_gaps_first) {
        a <- faux_segs
        b <- filtered
    } else {
        a <- filtered
        b <- faux_segs
    }
    
    ## interleave the lists
    idx <- order(c(seq_along(a), seq_along(b)))
    super_list <- (c(a,b))[idx]

    
    ## super list is now a list of segments - which are just dataframes.
    ## We're going to fold all these dataframes up into one big one.
    flat <- Reduce(rbind, super_list, data.frame())

    ## NOTE: I should probably be doing this in a better way
    attr(flat, "name") <- attr(bursts[[1]], "name")
    attr(flat, "seg")  <- 1
    attr(flat, "start_time") <- 0

    return (flat)
    
}








#' Order a list of bursts by some function. For instance, popen.
#'
#' @param bursts The list of all bursts
#' @param func A function of a segment that returns a numeric value
#' @param reverse By default, return in ascending order. Use reverse=TRUE to change that.
#' @return A list sorted by func. By default in ascending order (unless reversed)
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' # A sorted list of bursts. 
#' sorted <- bursts.sort(bursts, segment.popen)
#'
#' # You can also write your own functions. If you want P(Open) =~ P(Closed)
#' variance_fun <- function (seg) {
#'     # Any function that maps a segment to a number works.
#'     return(  segment.popen(seg) * segment.pclosed(seg)  )
#' }
#' 
#' weird_sort <- bursts.sort(bursts, variance_fun)
#' 
#' @export
bursts.sort <- function (bursts, func, reverse=FALSE) {

    vec_order <- order(sapply(bursts, func))
    
    sorted <- bursts[vec_order]

    if (reverse) {
        sorted <- rev(sorted)
    }

    return (sorted)
    
}





#' Return popens of every burst.
#'
#' @param bursts The list of all bursts
#' @return The popen values
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' 
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' popens <- bursts.popens(bursts)
#' hist(popens)
#' 
#' @export
bursts.popens <- function (bursts) {sapply(bursts, segment.popen)}





#' Return pcloseds of every burst.
#'
#' @param bursts The list of all bursts
#' @return The pclosed values
#' @examples
#'
#' infile <- system.file("extdata", "example1_qub.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' pcloseds <- bursts.popens(bursts)
#' hist(pcloseds)
#' 
#' @export
bursts.pcloseds <- function (bursts) {sapply(bursts, segment.pclosed)}








#' Return a list of all the (sub)conductance states.
#' 
#' @param bursts The list of all bursts
#' @return a list of all the (sub)conductance states.
#' @examples
#' 
#' infile <- system.file("extdata", "example4.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' bursts.conductance_states(bursts)
#'
#' @export
bursts.conductance_states <- function (bursts) {
    sort(unique(unlist(lapply(bursts,segment.conductance_states))))
}




#' Check if segment contains subconductive states
#' 
#' @param bursts The list of all bursts
#' @return True if it contains an conductance other than 0 or 1, False otherwise.
#' @examples
#' 
#' infile <- system.file("extdata", "example4.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#' 
#' bursts.check_subconductance(bursts)
#'
#' @export
bursts.check_subconductance <- function (bursts) {
    any(sapply(bursts,segment.check_subconductance))
}



## Filter out empty bursts and reindex them
bursts.remove_null <- function (bursts) {
    bs <- bursts.copy(bursts)
    filtered <- Filter(Negate(is.null), bs)
    for (i in 1:length(filtered)) {
        attr(filtered[[i]], "seg") <- i
    }
    return(filtered)
}



#' Imposes a deadtime to each segment in a burst.
#'
#' The user specifies a deadtime in microseconds. The function applies
#' segment.impose_deadtime to each segment in the burst.
#' (See segment.impose_deadtime for details.)
#'
#' @param  bursts a burst containing segments of dwells and states.
#' @param  deadtime the briefest possible event in microseconds.
#' @return A modified copy of the original burst 
#' @examples
#' 
#' infile <- system.file("extdata", "example4.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#'
#' bursts_d <- bursts.impose_deadtime(bursts, deadtime=0.01)
#'
#' @export
bursts.impose_deadtime <- function(bursts, deadtime){
    partial <- function(seg) { segment.impose_deadtime(seg, deadtime) }
    adjusted <- lapply(bursts, partial)

    ## Some bursts might be gone as a result of the new deadtime.
    ## Therefore must filter out empty bursts and reindex them
    filtered <- bursts.remove_null(adjusted)
    return(filtered)
}


#' Imposes a fixed conductance level (0 or 1) to all dwells with subconductance levels to each segment in a burst
#'
#' The user specifies the desired level ('open' or 'closed'). The function applies
#' segment.subconductance_as to each segment in the burst.
#' (See segment.subconductance_as for details.)
#'
#' @param  bursts the list of segments
#' @param  level either 'open' or 'closed'
#' @return A modified copy of the original burst 
#' @examples
#' 
#' infile <- system.file("extdata", "example4.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#'
#' bursts_d <- bursts.subconductance_as(bursts, "open")
#'
#' @export
bursts.subconductance_as <- function(bursts, level){

    partial <- function (seg) { segment.subconductance_as(seg, level) } 
    b2 <- lapply(bursts, partial)
    filtered <- bursts.remove_null(b2)
    return(filtered)

}




#' Transform the conductance states according to a user-defined function of conductance level.
#'
#' @param  bursts the list of segments
#' @param  fun a function on conductance levels
#' @return A modified copy of the original bursts
#' @examples
#' 
#' infile <- system.file("extdata", "example4.dwt", package = "scbursts")
#' dwells <- dwt.read(infile)
#' dwells_c <- risetime.correct_gaussian(Tr=35.0052278, dwells, units="us")
#' bursts <- bursts.defined_by_tcrit(dwells_c, 100, units="ms")
#'
#' ### Collapse into three subconductance states
#' fun <- function(amp) {
#'     if (amp < 0.3)
#'         return(0)
#'     else if (amp >= 0.3 && amp < 0.6)
#'         return(0.5)
#'     else
#'         return(1)
#' }
#' 
#' bursts_d <- bursts.modify_conductance(bursts, fun)
#'
#' @export
bursts.modify_conductance <- function(bursts, fun) {
    partial <- function (x) { segment.modify_conductance(x, fun) }
    b2 <- lapply(bursts, partial)
    filtered <- bursts.remove_null(b2)
    return(filtered)
}
