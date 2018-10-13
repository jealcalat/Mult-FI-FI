# resp_rate_df.R
# Separate by bins the data by cde, 1 = right, 2 = left lever
# needs tidyverse and funtion bind_all_Ss.R

resp_rate_df <- function(dfs,res,n_trials){
  
  # dfs <- read_sep_by_lever(k,1)
  # print(k)
  
  if(is.null(res)) res <- 1
  if(!is.null(dfs)) {
  
  dfs     <- na.omit(dfs)
  
  # get max bin
  # get the bins of all response times,
  # then return bins
  # Note: if other resolution desired, change parameter 'res'
 
  dfs <- dfs %>% filter(cum_trial <= n_trials)
  
  #Establece
  dfs$bins <- dfs$cum_dt %>% 
              get_bins(.,1,180,res)
  
  # filter if bins is greater than T*3
  dfs    <- dfs %>% 
    as.data.frame() %>% 
    filter(bins <= 180)
  # separates right component
  dfs_der     <- dfs %>% filter(cde == 1)
  # code resp on right component
  dfs_der$rsp <- ifelse(dfs_der$evento==1,1,0)
  # code head entry 
  dfs_der$ec  <- ifelse(dfs_der$evento==12,1,0)
  # computes response rate by bin
  dfs_der_sum <- dfs_der %>% 
    group_by(bins,cde,sujeto) %>%
    dplyr::summarise(
      resp=sum(rsp)/180,
      ec_resp=sum(ec)/180,
      n_resp = sum(rsp),
      n_ec   = sum(ec)
    ) %>% as.data.frame()
  # the same as in the other
  dfs_izq     <- dfs %>% filter(cde == 2)
  
  dfs_izq$rsp <- ifelse(dfs_izq$evento==11,1,0)
  dfs_izq$ec  <- ifelse(dfs_izq$evento==12,1,0)
  
  dfs_izq_sum <- dfs_izq %>% 
    group_by(bins,cde,sujeto) %>%
    dplyr::summarise(
      resp=sum(rsp)/180,
      n_resp = sum(rsp),
      n_ec   = sum(ec),
      ec_resp=sum(ec)/180
    ) %>% as.data.frame()
  
  dfs_res_ec <- bind_rows(dfs_izq_sum,dfs_der_sum )
 
  dfs_res_ec
  }
}
