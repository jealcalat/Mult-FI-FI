# rR_corr.R
# computes correlation between events; 
# r stands for response and R for reinforcer
# ==========================================================================
# Inputs: 
#   t_ev: matrix of 2x2 with [time,event]
#   R: id of first event (e.g., reinforcer)
#   r: id of second event (e.g., response)
#   sec_mult: resolution of the correlation. For example, if tvec
#     is in seconds and we want to compute correlation in milliseconds,
#     we must put sec_mult = 1000.
#   bin_res: resolution of bin. If we want to compute correlation in bins of 
#     200 millisenconds, put bin_res = 200
# NOTE:
# This function depends on get_bins 
# ==========================================================================

rR_corr <- function(t_ev,r,R,sec_mult,bin_res){
  
  # The max time at which subject get a reinforfer
  max_t <- max(t_ev[t_ev[,2] == R,1]) * sec_mult
  
  # vector of events 
  v_events <- c(r,R)
  
  # Create matrix of 3 columns to get bins and frecuencies of events in those bins
  m_rR <- 
    seq(0,max_t,bin_res) %>% 
    seq_along() %>%
    data.frame(bin_r = .,
               r = NA,
               R = NA)
  
  for(j in 1:2){
    
    e <- v_events[j]
    event_bins <- t_ev[which(t_ev[,2] == e),1] * sec_mult %>% 
      get_bins(.,1,max_t,bin_res)
    
    
    for(i in 1:nrow(m_rR)){
      
      sum    <-  sum(m_rR[i,1] == event_bins)
      m_rR[i, j + 1] <- sum # j + 1 because m_rR have 3 columns
      
    }
  }
  
  rho <- cor.test(m_rR[,2],m_rR[,3])
  rho$estimate
}
