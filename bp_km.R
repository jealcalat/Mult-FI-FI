

bp_km <- function(r_times,plot,plot_info){
  
  # get a 2x2 df with #resp per second
  res <- 1
  x <- seq(1,180,res)
  r_times <- get_bins(r_times,1,180,1)
  resp_sec <- r_times %>% 
  {
    bins <- .
    rate <- f_table(bins,1,180,res) # f_table is a custom fn
    rate
  }
  
  resp_sec <- data.frame(x = x, y = resp_sec)
  resp_sec_scaled <- apply(resp_sec, 2, scale) # clustering works better scaled
  colnames(resp_sec_scaled) <- c("x","y")
  set.seed(123)
  
  gap <- clusGap(resp_sec_scaled,pam,K.max = 4, B = 100) # gap statistic
  # to compute the optimal number of clusters btw 1 and 4
  # I restricted to 4 because of the nature of the data
  k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method="Tibs2001SEmax")
  
  km <- pam(resp_sec_scaled, k)
  
  k_vec <- 1:k
  
  if(length(k_vec) == 3 & length(k_vec) > 2){
    
  s1 <- which(km$clustering == 2)[2]
  
  s2 <- last(which(km$clustering == 2)) # first last
  
  s2 <- last(which(km$clustering[-s2] == 2)) # second last
  
  } else {
    s1 <- 1
    s2 <- 180
  }
  
  if(length(k_vec) > 3){
    s3 <- which(km$clustering == 4)[1]
  } else {
    s3 <- 180
  }
  
  start <- x[s1]
  stop <- x[s2]
  middle <- (start + stop)/2
  spread <- stop - start
  
  r1 <- sum(r_times <= start)/start
  r2 <- sum(r_times > start & r_times <= stop)/spread
  
  r3 <- sum(r_times > stop)/(179 - stop)
  
  metrics <- data.frame(opt_k = k,
                        start = start,
                        stop = stop,
                        s3 = s3,
                        spread = spread,
                        middle = middle,
                        r1 = r1,r2 = r2,r3 = r3)
  
  # if(plot == 1){
  #   # pdf(file = paste0(plot_info[1],plot_info[2],
  #   #                   "_",plot_info[3],"_trial_",
  #   #                   plot_info[4],"-",plot_info[5],".pdf"),
  #   #     height = 3,width=5)
  #   # 
  #   # plot(resp_sec, col = km$clustering,
  #   #      #xlim = c(0,180),
  #   #      xlab = "Time in trial",ylab = "Resp. per second",
  #   #      main = paste0("subject: ",plot_info[2],
  #   #                    ", trial: ",plot_info[4],
  #   #                    ", session: ",plot_info[3]
  #   #                    ," lever:",plot_info[5]))
  #   # # lines(fitted(km)[, "y"] ~ x, resp_sec_scaled)
  #   # abline(v = c(x[s1],x[s2]))
  #   # points(x = x[s3],y = resp_sec[s3,2],cex = 2,pch = 16)
  #   # dev.off()
  # }
  
  metrics
} 
