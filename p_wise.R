# p_wise.R v3
# Implements piecewise linear regression. Minimize the deviance with b1 and b2
# Changes from v2: More generic version; needs just r_times as input

p_wise <- function(r_times){  
  
  if(length(r_times)>9){
    # get a 2x2 df with #resp per second
    resp_sec <- r_times %>% 
    {
      bins <- .
      rate <- f_table(bins,1,180,1) # f_table is a custom fn
      rate
    }
    
    resp_sec <- data.frame(x = 1:180, y = resp_sec)
    b1 <- 1:170; b2 <- b1 + 10
    grid <- subset(expand.grid(b1 = b1, b2 = b2), b1 < b2)
    
    wx <- which.min(mapply(dv, grid$b1, grid$b2, MoreArgs = resp_sec))
    
    v <- c(t(grid[wx, ]))
    
    start <- v[1]
    stop <- v[2]
    spread <- stop - start
    middle <- (stop + start)/2
    r1 <- sum(r_times <= start)/start
    r2 <- sum(r_times > start & r_times <= stop)/spread
    r3 <- sum(r_times > stop)/(180 - stop)
    
    metrics <- c(start,stop,spread,middle,r1,r2,r3)
    metrics
  } # end if 
} 
