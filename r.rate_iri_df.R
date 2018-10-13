
# IRI_rates

r.rate_iri_df <- function(dfs,n_trials){

  
  if(!is.null(dfs)) {

    dfs     <- na.omit(dfs)
    dfs <- dfs %>% filter(cum_trial <= n_trials)
    
    # separates right component
    dfs_der     <- dfs %>% filter(cde == 1)
    # code resp on right component
    dfs_der$rsp <- ifelse(dfs_der$evento==1,1,0)
    dfs_der$rw_t <- ifelse(dfs_der$evento == 2,dfs_der$cum_dt,0)
    # code head entry 
    dfs_der$ec  <- ifelse(dfs_der$evento==12,1,0)
    # computes response rate by trial and IRI
    dfs_der_sum <- dfs_der %>% 
      group_by(trial,cde,sujeto) %>%
      dplyr::summarise(
        resp    = sum(rsp)/max(rw_t),
        ec_resp = sum(ec)/max(rw_t),
        n_resp  = sum(rsp),
        n_ec    = sum(ec),
        iri     = max(rw_t)
      ) %>% as.data.frame()
    # the same as in the other
    dfs_izq     <- dfs %>% filter(cde == 2)
    dfs_izq$rw_t <- ifelse(dfs_der$evento == 21,dfs_der$cum_dt,0)
    dfs_izq$rsp <- ifelse(dfs_izq$evento==11,1,0)
    dfs_izq$ec  <- ifelse(dfs_izq$evento==12,1,0)
    
    dfs_izq_sum <- dfs_izq %>% 
      group_by(trial,cde,sujeto) %>%
      dplyr::summarise(
        resp    = sum(rsp)/max(rw_t),
        ec_resp = sum(ec)/max(rw_t),
        n_resp  = sum(rsp),
        n_ec    = sum(ec),
        iri     = max(rw_t)
      ) %>% as.data.frame()
    
    dfs_res_ec <- bind_rows(dfs_izq_sum,dfs_der_sum)
    
    dfs_res_ec
  }
}
