# bind_all_ss.R 
# Based on raster_pico_aco_v1
# This function binds all dfs by subject session ids returned by read_sep_by_lever.R
# Input: a sequence of subjects' file names (e.g., 325,326,327...)
# Output: large df with all Ss and sessions

# list_Ss = vector of subjects, e.g., 326:333
# n = last sessions to analyze, e.g., n = 5

bind_all_Ss = function(list_Ss,n,peak,aco){
  last_n_list = last_n_ses(list_Ss=list_Ss,n=n,aco=aco)
  all_df = lapply(last_n_list,function(k){
    read_sep_by_lever(k,peak)
    })
  all_df = bind_rows(all_df) 
  all_df = all_df %>% 
    group_by(sujeto,cde) %>% 
    arrange(desc(sesion)) %>% 
    mutate(cum_trial = rleid(trial))
  
  all_df
}
