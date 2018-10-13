# pooled_ind_ana_df.R
# Ultima modificacion: 01.10.2018
# Funcion que usa break_point.R para realizar los analisis
# de ensayos individuales. 
# Retorna un data frame con start, stop, high rate, spread y midddle (mid)
# Esta funcion solo funciona para analisis de multiple IF IF acoplado

pooled_ind_ana_df <- function(list_ss,sessions,max_trials){  
	# Obtener metricas de ensayos individuales,
  # Obtener metricas de ensayos individuales para fase de training
  # se indica con 0
  training.df <- break_point(list_ss,sessions,0)
  training.df$phase <- "training"
  # Dado que los analisis se hacen sobre ensayos individuales
  # se obtienen las metricas por ensayo. Para contar los ensayos,
  # se escribe 1 en esta columna.

  training.df$trial <- 1
  # Contar ensayos por sujeto y fase, reorganizar
  # en orden descendente para sesion
  training.df <- training.df %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  # fase de acolplado se indica con el 1 (ver argumentos de break_point.R)
  aco.df <- break_point(list_ss,sessions,1)
  # codificar fase
  aco.df$phase <- "aco"
  
  aco.df$trial <- 1
  aco.df<- aco.df %>% 
    group_by(sujeto,cde) %>%
    arrange(desc(sesion)) %>%
    mutate(cum_trial = cumsum(trial))
  	
  # ultimos max_trials (e.g., 30) ensayos por componente
  boleantrial <- training.df$cum_trial <= max_trials
  training.df <- training.df[boleantrial,]
  boleantrial <- aco.df$cum_trial <= max_trials
  aco.df      <- aco.df[boleantrial,]
  # pool de ambas fases
  pool_tr_aco <- bind_rows(training.df,aco.df)
  # retornar pool_tr_aco
  pool_tr_aco 
}
