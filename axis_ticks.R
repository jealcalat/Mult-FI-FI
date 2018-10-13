# Function to get intermediate axis ticks. 
# https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r
# x must be a seq, and the breaks in scale_x_continuous (or _y_) must be the same


every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if(sum(!x %% 1 == 0) != 0){
  x[which(x %% 1 == 0)] <- x[which(x %% 1 == 0)] %>% paste0(.,".0")
  }
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
