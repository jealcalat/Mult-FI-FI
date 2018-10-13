# sem_ul_lwl.R

sem <- function(x) sd(x,na.rm = T)/sqrt(length(x))

upper <- function(x) mean(x,na.rm = T) + sem(x)
lower <- function(x) mean(x,na.rm = T) - sem(x)
