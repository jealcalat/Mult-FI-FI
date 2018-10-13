# low_high_low.R

# Church two step breakpoint algorithm, from Church et al, 1994.
# -- Note:
#   'aco' parameter is unnecessary, it's just to set a wd
#   to save the diagnostic plots, which are cumulative records
#   with lines at the start, stop and middle points. 
#   remove it if analyzing simple schedule (i.e., ona IF+peak)


low_high_low <- function(r_times){
  
  # initial values
  s1 <- 0 # start
  s2 <- 0 # stop
  t1 <- 0 # first low rate duration
  t2 <- 0 # high rate duration
  t3 <- 0 # second low rate duration
  r1 <- 0 # first low rate
  r2 <- 0 # high rate
  r3 <- 0 # second low rate
  index_s1 <- 0 # index to get start (inner for loop)
  index_s2 <- 0 # index to get stop (outer for loop)
  algorithm_s1 <- 0 # as above, indexed by index_s1
  algorithm_s2 <- 0
  end_trial <- max(r_times) 
  
  r <- (length(r_times))/180 # get overall response rate
  # 'if' conditional states that trials must have more than 3 responses
  if(length(r_times) > 3){  
    # loop through response times, except the last
    # then choose best t1 (inner for loop), then get best s2 estimate (outer for loop)
    for(i in 1:(length(r_times) - 1)){ 
      
      s2[i] <- r_times[i + 1] 
      t3[i] <- end_trial - s2[i]

      if(t3[i] <= 0){
        r3[i] <- 0 # necessary to avoid division by 0
        t3[i] <- 0
      } else {
        r3[i] <- sum(r_times > s2[i])/t3[i]
      }
      
      for(j in seq_along(s2)){ # loop through putative s2 and choose t1
        
        t1[j] <- r_times[j] 
        r1[j] <- sum(r_times <= t1[j])/t1[j]
        t2[j] <- s2[i] - t1[j] # high-rate duration
        
        if(t2[j] == 0){ 
          r2[j] <- 0 # necessary to avoid division by 0
        } else {
          r2[j] <- sum(r_times > t1[j] & r_times <= s2[i])/t2[j]
        }
        
        algorithm_s1[j] <- t1[j]*(r - r1[j]) + t2[j]*(r2[j] - r) + t3[i]*(r - r3[i])
        index_s1 <- which(algorithm_s1 == max(algorithm_s1,na.rm = T))
        # some times two indexes with same algorithm value
        # get the first of them
        if(length(index_s1) > 1){
          index_s1 <- min(index_s1)
        }
        
      } # end inner loop (get s1 value)
      
      algorithm_s2[i] <- t1[index_s1]*(r - r1[index_s1]) + t2[i]*(r2[i] - r) + t3[i]*(r - r3[i])
      
    } # end outer for loop (get s2 value after s1)
    
    index_s2 <- which(algorithm_s2 == max(algorithm_s2,na.rm = T))
    
    if(length(index_s2) > 1){ 
      index_s2 <- min(index_s2)
    }
    
    start <- t1[index_s1]
    stop <- s2[index_s2]
    mid <- (start + stop)/2
    low_rate_1 <- sum(r_times <= start)/start
    low_rate_2 <- sum(r_times > stop)/(end_trial - stop)
    spread <- stop - start
    
    high_rate <- sum(r_times > start & r_times <= stop)/spread
    
    bpts <- 
      data.frame(start = start,
                 stop = stop,
                 mid = mid,
                 spread = spread,
                 high_rate = high_rate,
                 r1 = low_rate_1,
                 r3 = low_rate_2)
    
    bpts
  } # end if 
  
}
