# bp_pwise.R
# reemplaza break_point

bp_pwise <- function(list_Ss=list_Ss,n=n,aco=aco,rp_index=c(1,11)){
  
  last_n_list <- last_n_ses(list_Ss=list_Ss,n=n,aco=aco)
  
  metrics_all_subj <- lapply(last_n_list,function(k){
    
    df_t <- read_sep_by_lever(k,1) %>%
    filter(evento %in% rp_index) %>%
      `[`(c("evento","sujeto","cde","sesion","cum_dt","trial")) # select columns
    
    df_t$bins <- get_bins(df_t$cum_dt,1,180,1) 
    
    if(!is.null(df_t)){
      
      subj <- unique(df_t$sujeto)
      sess <- unique(df_t$sesion)
      cde <- unique(df_t$cde)
      
      b1b2 <- lapply(cde,function(l){
        df_cde <- df_t[df_t$cde == l,]
        n_trials <- unique(df_cde$trial)
        df_cdetr <- lapply(n_trials,function(j){ # second lapply
          df_tr <- df_cde[df_cde$trial == j,]
          r_times <- df_tr$bins 
          dir <- paste0("~/Dropbox/LAyCC/pwise_v2_plots/",k,"-",j,"-",l)
          met <- p_wise(r_times,dir) # returns a vector of s1,s2,spread,mid,r1,r2,r3
          if(!is_null(met)){
            names(met) <- c("start","stop","spread","middle","r1","r2","r3","rs")
            met_trial <- t(met) %>% as.data.frame() # makes named met a df
            met_trial$trial <- j
            met_trial
          } else {
            met_trial <- rep(NA,8) 
            names(met_trial) <- c("start","stop","spread","middle","r1","r2","r3","rs")
            met_trial <- t(met_trial) %>% as.data.frame()
            met_trial$trial <- j
            met_trial
          }
        }) %>% bind_rows() %>% as.data.frame()
        # df_cdetr have the metrics by trial; now add info of cde 
        df_cdetr$cde <- l
        df_cdetr
      }) %>% bind_rows()
      # b1b2 have the metrics by trial and cde; add subj, sess
      
      b1b2$sesion <- sess
      b1b2$sujeto <- subj
      
    }
  }) %>% bind_rows()
  metrics_all_subj # return metrics
}
