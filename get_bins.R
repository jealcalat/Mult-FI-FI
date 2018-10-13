# discretize time in bins of custom resolution
# use: get_bins(x,x.min,x.max,resolution)
get_bins <- function(x,x.min,x.max,resolution){
  bins <- x %>% 
  {
    t <- . # basically asign x to t to be used as a placeholder variable
    t <- cut(t,(x.min:x.max)*resolution) # find intervals between xmin and xmax
    t <- ifelse(is.na(t),0,t) # return the levels of the intervals
    t %>% as.integer() # convert to integers
  }
  bins # same as return(x), but more efficient
}
