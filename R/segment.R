#' Create a "segment" object
#' 
#' This is a low-level function, mostly for use internally by other functions. There aren't many reasons to use this. Create object containing table data and metadata. The object can be used as a dataframe, and the metadata can be accessed with the functions: segment.seg, segment.start_time, segment.filename
#' 
#' @param states a vector of states
#' @param dwells a vector of dwell durations (same length as states)
#' @param seg The segment number. Defaults to 1
#' @param start_time When the dwells began. Defaults to 0
#' @param name Suffix-less version of the original filename. 60uM.dwt -> '60uM'
#' @param ignore_errors Do not report faulty segments (not many reasons to do this)
#' @return The segment object: A dataframe with extra metadata.
#' @examples
#'
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=0, name="example_segment")
#' 
#' segment.name(my_burst)
#' 
#' @export
segment.create <- function (states, dwells, seg=1, start_time=0, name="burst", ignore_errors=FALSE) {

    data  <- data.frame(states, dwells)

    attr(data, "name") <- name
    attr(data, "seg")  <- seg
    attr(data, "start_time")  <- start_time

    # This is reasonable to include, but will interfere with the check done in bursts.
    if (!ignore_errors && !segment.verify(data))
        warning('Burst seems to have been misrecorded!')

    if (!ignore_errors && segment.check_subconductance(data))
        warning('Burst has subconductive states!')
    
    return(data)
    
}

#' Copy a segment
#' 
#' This is a low-level function, mostly for use internally by other functions. There aren't many reasons to use this. 
#' 
#' @param segment The segment to copy
#' @return A duplicate identical content.
#' @export
segment.copy <- function (segment) {

    states <- segment$states
    dwells <- segment$dwells
    
    data  <- data.frame(states, dwells)

    attr(data, "name")        <- attr(segment, "name")
    attr(data, "seg")         <- attr(segment, "seg")
    attr(data, "start_time")  <- attr(segment, "start_time")

    return(data)
}




#' Extract segment number from segment.
#'
#' @param segment the segment object
#' @return Segment number (integer)
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=0, name="example_segment")
#' 
#' segment.seg(my_burst)
#' 
#' @export
segment.seg <- function(segment) {attr(segment, "seg")}




#' Extract start_time from segment.
#'
#' @param segment the segment object
#' @return Segment start_time (float)
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.start_time(my_burst)
#' 
#' @export
segment.start_time <- function(segment) {attr(segment, "start_time")}



#' Extract name from segment.
#'
#' @param segment the segment object
#' @return Segment name (string)
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.name(my_burst)
#' 
#' @export
segment.name <- function(segment) {attr(segment, "name")}



#' Get duration of a segment.
#'
#' @param segment the segment object
#' @return the duration
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.duration(my_burst)
#' 
#' @export
segment.duration <- function(segment) {
    sum(segment$dwells)
}



#' Extract number of dwells in segment.
#'
#' @param segment the segment object
#' @return number of dwells
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.count_dwells(my_burst)
#' 
#' @export
segment.count_dwells <- function(segment) {length(segment$states)}



#' Extract number of open dwells. In the case of subconductive states,
#' count the number of non-zero states.
#'
#' @param segment the segment object
#' @return number of open dwells
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.count_open(my_burst)
#' 
#' @export
segment.count_open <- function(segment) {length(segment.open_dwells(segment))}




#' Extract number of closed dwells. In the case of subconductive states,
#' a dwell is only closed if the conductance is exactly zero.
#'
#' @param segment the segment object
#' @return number of closed dwells
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.count_closed(my_burst)
#' 
#' @export
segment.count_closed <- function(segment) {length(segment.closed_dwells(segment))}





#' Extract open dwells. (Any conductance greater than zero)
#'
#' @param segment the segment object
#' @return the open dwells
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' open_dwells <- segment.open_dwells(my_burst)
#' head(open_dwells)
#' 
#' @export
segment.open_dwells <- function(segment) { subset(segment, states > 0)$dwells }



#' Extract closed dwells.
#'
#' @param segment the segment object
#' @return the closed dwells
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' closed_dwells <- segment.closed_dwells(my_burst)
#' head(closed_dwells)
#' 
#' @export
segment.closed_dwells <- function(segment) { subset(segment, states == 0)$dwells }





#' Extract dwells in conductance range. lower <= x <= upper
#'
#' @param segment the segment object
#' @param level The conductance to extract
#' @return the dwells in a given range
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' half_open <- segment.dwells_by_conductance(my_burst, 0.5)
#' head(half_open)
#' 
#' @export
segment.dwells_by_conductance <- function(segment, level) {
    subset(segment, states == level)$dwells
}




#' Extract dwells in conductance range. lower <= x <= upper
#'
#' @param segment the segment object
#' @param lower lower bound on conductance (defaults to 0)
#' @param upper upper bound on conductance (defaults to infinity)
#' @return the dwells in a given range
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' half_open <- segment.dwells_by_conductance_range(my_burst, lower=0.2, upper=0.7)
#' head(half_open)
#' 
#' @export
segment.dwells_by_conductance_range <- function(segment, lower=0, upper=Inf) {
    subset(segment, lower <= states & states <= upper)$dwells
}








#' Calculate empirical P(Open) of a segment. (A state is considered open if the conductance is non-zero)
#' 
#' Calculate empirical P(Open) of a segment. NOTE: Assuming that burst starts and ends with 1
#'
#' @param segment The dwells and states table
#' @return The ratio of open time to total time
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#'
#' # P(Open) of this burst
#' segment.popen(my_burst)
#' 
#' @export
segment.popen <- function (segment) {

    open_times <- subset(segment, states > 0, select=dwells)

    total_duration <- sum(segment$dwells)
    
    return (sum(open_times) / total_duration)
}




#' Calculate empirical P(Closed) of a segment.
#' 
#' Calculate empirical P(Closed) of a segment. NOTE: Assuming that burst starts and ends with 1
#'
#' @param segment The dwells and states table
#' @return The ratio of closed time to total time
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' # P(Closed) of this burst
#' segment.pclosed(my_burst)
#' 
#' @export
segment.pclosed <- function (segment) {

    popen <- segment.popen(segment)
    
    return ( 1 - popen )
}


#' Calculate empirical P(Lower <= Conductance <= Upper) of a segment.
#'
#' @param segment the segment object
#' @param level conductance level
#' @return The probability of being in this conductance state
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.pconductance(my_burst, 0.5)
#' 
#' @export
segment.pconductance <- function(segment, level) {

    selection <- subset(segment, states == level)$dwells

    total_duration <- sum(segment$dwells)
    
    return (sum(selection) / total_duration)

}




#' Calculate empirical P(Lower <= Conductance <= Upper) of a segment.
#'
#' @param segment the segment object
#' @param lower lower bound on conductance (defaults to 0)
#' @param upper upper bound on conductance (defaults to infinity)
#' @return The probability of being in these conductance states
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.pconductance_range(my_burst, lower=0.5, upper=0.5)
#' 
#' @export
segment.pconductance_range <- function(segment, lower=0, upper=Inf) {

    selection <- subset(segment, lower <= states & states <= upper)$dwells

    total_duration <- sum(segment$dwells)
    
    return (sum(selection) / total_duration)

}











#' Detect misrecorded data.
#' 
#' Segments should have a very specific shape, but recordings can produce errors that make non-sensical segments. In particular, ones contain multiple consecutive states of equal conductance, or end in closings. This function detects whether a segment satisfies the constraint that the segment conductances are not the same from one dwell to the next, and begin and end with a closing.
#'
#' @param segment The dwells and states table
#' @return True if a valid segment, False otherwise
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.verify(my_burst)
#'
#' # Now, a bad burst with two adjacent open dwells
#' states  <-  c(0,      1,    0,    1,    1,    0,    1,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.1,  0.6,  1.1,  0.8,  1.1)
#'
#' # This will issue a warning
#' faulty_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="faulty_segment")
#'
#' # This will differentiate good and faulty bursts
#' segment.verify(faulty_burst)
#'
#' # If you have a list of bursts, you can select the good ones with
#' # vbursts <- bursts.select(bursts, segment.verify)
#' 
#' @export
#' @importFrom utils head tail
segment.verify <- function (segment) {

    if (length(segment$states) == 0)
        return(TRUE)
    
    ## Begins with 0
    if (head(segment$states,n=1) == 0)
        return(FALSE)

    ## Ends with 0
    if (tail(segment$states,n=1) == 0)
        return(FALSE)

    ## Contains consecutive 0s or 1s.
    if (any(diff(segment$states) == 0))
        return(FALSE)

    ## Otherwise is OK
    return(TRUE)
}





#' Return a list of all the (sub)conductance states.
#' 
#' @param segment The dwells and states table
#' @return a list of all the (sub)conductance states.
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.conductance_states(my_burst)
#'
#' @export
segment.conductance_states <- function (segment) {sort(unique(segment$states))}




#' Check if segment contains subconductive states
#' 
#' @param segment The dwells and states table
#' @return True if it contains an conductance other than 0 or 1, False otherwise.
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' segment.check_subconductance(my_burst)
#'
#' @export
segment.check_subconductance <- function (segment) {
    return(length(setdiff(segment$states, c(0,1))) > 0)
}



#' Collapses a segment into dwells with alternating conductance levels.
#'
#' Segments may contain consecutive dwells with the same conductance level. 
#' consecutives_to_dwells sums together all consecutive dwells with the same
#' conductance level. The result is a segment containing dwells that alternate
#' in conductance level (i.e. 1,0,1,0,1,...)
#'
#' @param  segment The dwells and states table
#' @return A modified copy of the original segment 
segment.consecutives_to_dwells <- function(segment) {

    d  = segment$dwells
    s  = segment$states
    s2 = c()
    d2 = c()

    ## Silly case
    if (length(s) == 1)
        return(segment)

    s2[1] = s[1]
    d2[1] = d[1]
    for (i in 2:length(s)) {
        if (s[i] == s[i-1]) {
            d2[length(d2)] <- d2[length(d2)] + d[i]
        } else {
            s2 <- append(s2, s[i])
            d2 <- append(d2, d[i])
        }
    }
    
    seg <- segment.create(s2,d2,attr(segment,"seg"),attr(segment,"start_time"),attr(segment,"name"))
    return(seg)
}


#' Imposes a deadtime to a segment by removing any dwell that is shorter than the deadtime.
#'
#' The user specifies a deadtime in microseconds. The function effectively undoes
#' the work of the event detection algorithm by reverting the conductance level 
#' (of the brief dwell) back to the previous conductance level in the time sequence.
#' The function then returns a collapsed segment containing alternating dwells.
#'
#' @param  segment the segment containing dwells and states.
#' @param  deadtime the briefest possible event in microseconds.
#' @return A modified copy of the original segment 
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' my_burst_d <- segment.impose_deadtime(my_burst, deadtime=0.3)
#'
#' @export
segment.impose_deadtime <- function(segment, deadtime){

    start_time <- attr(segment, "start_time")
    d  = segment$dwells
    s  = segment$states
    dt = deadtime / 1e6

    ### If first dwell is < dead_time, then set the conductance level to 0.
    ### Add the zero to the starting time --- increasing the length of the gap.
    while (length(d) > 0 && d[1] < dt) {
        start_time  <- start_time + d[1]
        d = d[2:length(d)]
        s = d[2:length(d)]
    }

    
    if (length(d) == 0) {
        warning("The segment completely disappears with this deadtime!")
        s2 <- segment.create(s,d,attr(segment,"seg"),start_time,attr(segment,"name"))
        return(s2)
    }
        
    ### For all other dwells in the segment, if the dwell is < dead_time, change the 
    ### conductance level to the previous (in the time sequence) dwell's conductance level. 
    for (i in 2:length(d)){
        if (d[[i]] < dt) {
            s[[i]] = s[[i-1]]
        }
    }
    
    s2 <- segment.create(s,d,attr(segment,"seg"),start_time,attr(segment,"name"))
    #returns a collapsed segment of dwells with alternating conductance levels.
    return(segment.consecutives_to_dwells(s2))
}



#' Imposes a fixed conductance level (0 or 1) to all dwells with subconductance levels.
#'
#' The user specifies the desired level ('open' or 'closed'). The function will modify
#' any subconductance level (that is not 0 or 1) to be the desired level 1 for 'open'
#' or 0 for 'closed'. The function then reutrns a collapsed segment containing 
#' alternating dwells.
#' (See segment.consecutives_to_dwells for details about the collapsed segment.)
#'
#' @param  segment the segment containing dwells and states.
#' @param  level either 'open' or 'closed'
#' @return A modified copy of the original segment 
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
#' 
#' my_burst_d <- segment.subconductance_as(my_burst, "open")
#'
#' @export
segment.subconductance_as <- function(segment, level) {

    s2 = segment.copy(segment)

    #Sets desired conductance level to 1 or zero depending on users' choice.
    #Returns a warning message if level is not 'open' or 'closed'
    if      (level == 'open')   {l = 1}
    else if (level == 'closed') {l = 0}
    else    {return('Conductance level must be either \'open\' or \'closed\'.')}

    #For all the dwells in the segment, if the conductance level is not a 1 or a 0,
    #then set the conductance level as the desired level l.
    fun <- function (x) {
        if (x == 0 || x == 1)
            return(x)
        else
            return(l)
    }
    
    s2$states <- sapply(s2$states, fun)
    
    #Returns a collapsed segment of dwells with alternating conductance levels.
    return(segment.consecutives_to_dwells(s2))
}





#' Transform the conductance states according to a user-defined function of conductance level.
#'
#' @param  segment the segment containing dwells and states.
#' @param  fun a function on conductance levels (states)
#' @return A modified copy of the original segment 
#' @examples
#' 
#' # It's more likely that you created states or dwells with some function
#' states  <-  c(0,    0.2,    0,    1,    0,  0.5,    0,  0.7,    0,    1)
#' dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
#' my_burst <- segment.create(states, dwells, seg=1, start_time=3.14159, name="example_segment")
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
#' my_burst_d <- segment.modify_conductance(my_burst, fun)
#'
#' @export
segment.modify_conductance <- function(segment, fun) {
    s2 = segment.copy(segment)
    s2$states <- sapply(s2$states,fun)
    return(segment.consecutives_to_dwells(s2))
}


