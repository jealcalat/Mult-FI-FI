
pooled_ind_ana_km_df <- function(list_ss,sessions,max_trials){  
  # Obtener metricas de ensayos individuales,
  # fase de acolplado se indica con el 1 (ver argumentos de break_point.R)
  
  aco.df <- bp_km_wrap(list_ss,sessions,1,c(1,11))

  aco.df$phase <- "aco"
  # Obtener metricas de ensayos individuales para fase de training
  training.df <- bp_km_wrap(list_ss,sessions,0,c(1,11))
  
  training.df$phase <- "training"
  # Dado que los analisis se hacen sobre ensayos individuales
  # se obtienen las metricas por ensayo. Para contar los ensayos,
  # se escribe 1 en esta columna.
  aco.df$trial <- 1
  # Contar ensayos por sujeto y fase, reorganizar
  # en orden descendente para sesion
  aco.df<- aco.df %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  training.df$trial <- 1
  
  training.df <- training.df %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  
  # ultimos max_trials (e.g., 30) ensayos por componente
  boleantrial <- training.df$cum_trial <= max_trials
  training.df <- training.df[boleantrial,]
  boleantrial <- aco.df$cum_trial <= max_trials
  aco.df      <- aco.df[boleantrial,]
  # pool de ambas fases
  pool_tr_aco <- bind_rows(aco.df,training.df)
  # retornar pool_tr_aco
  pool_tr_aco 
}
