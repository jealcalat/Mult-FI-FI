# d stands for discrete

kld_d <- function(P,Q){
  # find 0s
  idx = c(which(P == 0),which(Q == 0))
  if(!is_empty(idx)){
  # remove 0s
    P = P[-idx] 
    Q = Q[-idx]
    }
  # get the ratio of distributions
  PQ = P/Q
  # the distance (discrepancy) is the dot product of vectors
  dist = P %*% log(PQ,base = 2)
  # return distance
  dist
}
