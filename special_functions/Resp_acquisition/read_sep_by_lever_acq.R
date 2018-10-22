
read_sep_by_lever <- function(sj){
 x <- sj
  if(!is_empty(read.table(sj,sep = ";",header = T))){
    if(sum(read.table(sj,sep = ";",header = T)[2] == 5,na.rm=TRUE)>0 |
       sum(read.table(sj,sep = ";",header = T)[2] == 51,na.rm=TRUE)>0){
      # subject = sj # uncomment to use subject to identify errors
      sj <- read.table(sj,sep = ";",header = T,stringsAsFactors = F)

      #sj=na.omit(sj) # siempre omitir NA para evitar errores con algunas funciones, como sum, cumsum, etc
      sj$t <- sj$t/10 # convertimos a s
      sj$dt <- c(0,diff(sj$t)) # calculamos un delta t
      
      # Incluir todos los eventos para que al agruparlos por cum_id
      
      sj$mark.v <- ifelse(sj$evento== 8 | sj$evento == 51 |  
                            sj$evento== 7 | sj$evento == 5 |  
                            sj$evento==54 | sj$evento==34,1,0)
      
      # cada que mark==1, suma acumulativamente, permitiendo crear identificadores
      # para cada programa IF activo
      sj$cum_id <- cumsum(sj$mark.v)
      
      t_ref    <- sum(sj$evento %in% c(21,2))
      
      # A continuacion se seleccionan solo los ensayos reforzados
      i <- which(cumsum(sj$evento == 2 | sj$evento == 21) >= t_ref)[1]
      
      sj <- sj[1:i,]
      
      derecha <- sj[c("t","evento","sujeto","sesion","dt","cum_id")] %>% 
        group_by(cum_id) %>%  
        filter(cumsum(evento == 8)!=0) %>% 
        as.data.frame()
      
      if(!is_empty(derecha[[1]])){
        derecha$cum <- cumsum(derecha$evento==11)
        derecha$t <- cumsum(derecha$dt)
        derecha$cde <- 1
      }
      
      # selecciona izquierda pico
      
      izquierda <- sj[c("t","evento","sujeto","sesion","dt","cum_id")] %>% 
        group_by(cum_id) %>% 
        filter(cumsum(evento==7)!=0) %>% 
        as.data.frame()
      
      if(!is_empty(izquierda[[1]])){
        
        izquierda$cum <- cumsum(izquierda$evento==1)
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
        
        setDT(iz_der)[, trial := rleid(cum_id),by =.(cde)]
        
        setDT(iz_der)[, cum_dt := cumsum(dt) - cumsum(dt)[1],by = .(trial,cde)] 
        
        iz_der <- as.data.frame(iz_der)
        
        iz_der['cum_id'] <- NULL
      }
    #  print(x)
    } # end if of events
  } # end if of empy file name
  if(length(iz_der$cum) > 6){ # is there at least one response?
    iz_der # return iz_der
  }
}  # end function 

