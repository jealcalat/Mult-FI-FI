# read_sep_by_lever.R 
# Based on raster_pico_aco_v1
# Input: File name from last_n_ses.R and peak where 1 = get just
# peak trials and 0 = get all trials
# Output: Single dataframe of SubjectSesion with 8 columns

read_sep_by_lever <- function(sj,peak){

  if(!is_empty(read.table(sj,sep = ";",header = T))){
  if(sum(read.table(sj,sep = ";",header = T)[2] == 53,na.rm=TRUE)>0 |
     sum(read.table(sj,sep = ";",header = T)[2] == 54,na.rm=TRUE)>0 |
     sum(read.table(sj,sep = ";",header = T)[2] == 5,na.rm=TRUE)>0  |
     sum(read.table(sj,sep = ";",header = T)[2] == 51,na.rm=TRUE)>0){
   # subject = sj # uncomment to use subject to identify errors
    sj <- read.table(sj,sep = ";",header = T,stringsAsFactors = F)
  
    #sj=na.omit(sj) # siempre omitir NA para evitar errores con algunas funciones, como sum, cumsum, etc
    sj$t <- sj$t/10 # convertimos a s
    sj$dt <- c(0,diff(sj$t)) # calculamos un delta t
    
    # Incluir todos los eventos para que al agruparlos por cum_id
    
    sj$mark.v <- ifelse(sj$evento==5 | sj$evento==3 |  
                        # sj$evento==2 | sj$evento==21 |
                        sj$evento==53 | sj$evento==33 |  
                        sj$evento==54 | sj$evento==34 |
                        sj$evento==51 | sj$evento==31,1,0)
    
    # cada que mark==1, suma acumulativamente, permitiendo crear identificadores
    # para cada programa IF activo
    sj$cum_id <- cumsum(sj$mark.v)
    
    t_ref    <- sum(sj$evento %in% c(21,2))
    
    i <- which(cumsum(sj$evento == 2 | sj$evento == 21) >= t_ref)[1]

    sj <- sj[1:i,]
    
    # select right: if peak = 1, evento = 53, else all "right"
    
    if(peak == 1){
      
      derecha <- sj[c("t","evento","sujeto","sesion","dt","cum_id")] %>% 
        group_by(cum_id) %>%  
        filter(cumsum(evento == 53)!=0) %>% 
        as.data.frame()
      
    } else {
      derecha <- sj[c("t","evento","sujeto","sesion","dt","cum_id")] %>% 
        group_by(cum_id) %>%  
        filter(cumsum(evento==5)!=0) %>% 
        as.data.frame()
    }
    
    if(!is_empty(derecha[[1]])){
      # checar si la respuesta en pico fue codificada como 13
      # else fue codificada como 1, avanzar
      if (sum(sj$evento==13)>0){ # no necesario para pico_acoplado
        derecha$cum <- cumsum(derecha$evento==13)
      } else {                   
        derecha$cum <- cumsum(derecha$evento==1)
      }
      
      derecha$t <- cumsum(derecha$dt)
      derecha$cde <- 1
    }
    
    # selecciona izquierda pico
    if(peak == 1){
    izquierda <- sj[c("t","evento","sujeto","sesion","dt","cum_id")] %>% 
      group_by(cum_id) %>% filter(cumsum(evento==54)!=0)%>% as.data.frame()
    } else {
      izquierda <- sj[c("t","evento","sujeto","sesion","dt","cum_id")] %>% 
        group_by(cum_id) %>% 
        filter(cumsum(evento==51)!=0) %>% 
        as.data.frame()
    }
    
    if(!is_empty(izquierda[[1]])){
      if (sum(sj$evento==14)>0){
        izquierda$cum <- cumsum(izquierda$evento==14)
      } else { 
        izquierda$cum <- cumsum(izquierda$evento==11)
      }
      
      izquierda$t <- cumsum(izquierda$dt)
      
      izquierda$cde <- 2 
    }
    
    # volvemos a juntarlas & verificamos que no esten vacias,
    # de lo contrario, asignamos la que no este vacia
    
    if(!is_empty(derecha[[1]]) & !is_empty(izquierda[[1]])){
      iz_der <- bind_rows(izquierda,derecha)
    } else if(is_empty(derecha[[1]])){
      iz_der <- izquierda
    } else {
      iz_der <- derecha
    }
    
    # crear columna de trials por componente
    # print(subject) # uncomment to identify df with errors
    
    if(!is_empty(iz_der[[1]])){
      
      setDT(iz_der)[, trial := rleid(cum_id),by=.(cde)]
    
      setDT(iz_der)[, cum_dt := cumsum(dt) - cumsum(dt)[1],by = .(trial,cde)] 
    
      iz_der <- as.data.frame(iz_der)
    
      iz_der['cum_id'] <- NULL
     }
    
    } # end if of events
  } # end if of empy file name
  if(length(iz_der$cum) > 6){ # is there at least one response?
    iz_der # return iz_der
  }
}  # end function 

