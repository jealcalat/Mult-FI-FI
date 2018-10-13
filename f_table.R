
# seq table freq

f_table <- function(x,x.min,x.max,bin_res){
  
  bin <- seq(x.min,x.max,bin_res) # vector of bins
  nas <- rep(NA,length(bin)) # vector of NAs of length bin
  m <- matrix(c(bin,nas),ncol = 2) # matrix to store results
  
  for (k in 1:length(bin)) {
  
    f <- sum(m[k,1] == x)
    m[k,2] <- f
  
  }
  m[,2]
}
