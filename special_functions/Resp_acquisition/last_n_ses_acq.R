# last_n_ses.R
# Function that takes the files names of last n sessions (e.g., n = 5)
# Its outputs will be a list, inputs of plots and other analysis. 

last_n_ses = function(list_Ss,n){
  sessions = lapply(list_Ss,function(x){
    su <- paste0("SM",x)
    # List of subjects matching su
    sj <- list.files(pattern = paste0("^",su,"(.*).txt"),recursive = F)
    # verify if n > length(sj)
    if(n > length(sj)){
      n <- length(sj)
      print(paste0("n is greater than max (",n, ") so ",n," will be taken"))
    }
    # a <- first session
    a <- length(sj) - n + 1
    # Vector from a to n session
    b <- seq(a,length(sj)) 
    # Return list of sessions
    files_names = lapply(b, function(i){
      fns = paste0(su,"S",i,".txt")
    })
    files_names = unlist(files_names)
  })
  sessions = unlist(sessions)
  return(sessions)
}
