# resp_times.R
# Ultima modificacion: 16.09.2018
# Obtiene los tiempos de respuesta (literalmente: 
# cada segundo o bin en que dio una respuesta) de
# data frames. Se especifica ademas de cuantos ensayos
# (los ultimos n_trials).
# df debe tener las columnas:
# - cum_trial, que contiene los ensayos acumulados
# - cum_dt, que contiene el tiempo por ensayo
# - evento, que contiene la lista de eventos (e.g., 1, 2, 12..)
# - cde, que contiene el codifo de componente (1,2)
# - sesion

resp_times <- function(df,n_trials){
  
  df <- df %>% filter(cum_trial <= n_trials)
  
  df$bins <- df$cum_dt %>% 
  {
    dt <- .
    max_bin <- max(dt) %>% ceiling()
    bins <- dt %>% cut((0:max_bin) * 1) %>% ifelse(is.na(.),0,.)
    bins
  }
  # filter if bins is greater than T*3
  df <- df %>% 
    as.data.frame() %>% 
    filter(bins <= 180)
  
  df <- df %>%
  {
    var     <- . 
    event   <- var %>% `[[`("evento")
    boolean <- ifelse(event == 1 | event == 11,1,0) %>% as.logical()
    var     <- var[boolean,]
    var
  }
  
  resptimes <- df %>% select(sujeto,evento,sesion,cde,bins)
  
  resptimes
  
}
