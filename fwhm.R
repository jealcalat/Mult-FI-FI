# fwhm.R and mfw.R 
# Full width half maximum and maximum at full width 
# Different functions. Some trials have more than one peak. This is a problem
# I tried to solve with the more complex one (second)

fwhm <- function(x,y){
  
  xy <- data.frame(x = x,y = y)
  xy <- xy[xy$x > 0 & xy$x < 180,]
  
  xmax <- xy$x[xy$y==max(xy$y)]
  x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
  x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
  fwhm <- x2 - x1
  fwhm
}

mfw <- function(x,y){
  
  xy <- data.frame(x = x,y = y)
  xy <- xy[xy$x > 0 & xy$x < 180,]
  
  xmax <- xy$x[xy$y==max(xy$y)]
  x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
  x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
  # Resize vector between x1 and x2
  xy <- xy[xy$x >= x1 & xy$x <= x2,]
  xmax2 <- xy$x[xy$y==max(xy$y)]
  xmax2
}

# 
# fwhm <- function(x,y){
#   
#   xy <- data.frame(x = x,y = y)
#   
#   xy <- xy[xy$x > 0 & xy$x <= 150,]
# 
#   y.peaks <- pracma::findpeaks(xy$y,npeaks = 4)
#   
#   y.max1 <- max(y.peaks[,1])
#     
#   y.max2 <- min(y.peaks[,1])
# 
#   x.max1 <- xy$x[xy$y == y.max1]
#   x.max2 <- xy$x[xy$y == y.max2]
#   
#   while(x.max1 > x.max2){
#     
#     xy <- xy[xy$x >= x.max2,]
#     
#     y.peaks <- pracma::findpeaks(xy$y,npeaks = 3)
#    
#     y.max1 <- max(y.peaks[,1])
#       
#     y.max2 <- min(y.peaks[,1])
#       
#     x1_peak <- x.max2
#     x.max1 <- xy$x[xy$y == y.max1]
#     x.max2 <- xy$x[xy$y == y.max2]
#    
#   }
#   
#   if(y.max1 != y.max2){
#     x1 <- xy$x[xy$x < x.max1][which.min(abs(xy$y[xy$x < x.max1] - y.max1/2))]
#     x2 <- xy$x[xy$x > x.max1 & xy$x < x.max2][which.min(abs(xy$y[xy$x > x.max1 & xy$x < x.max2] - y.max1/2))]
#   } else {
#     x1 <- xy$x[xy$x < x.max1][which.min(abs(xy$y[xy$x < x.max1] - y.max1/2))]
#     x2 <- xy$x[xy$x > x.max1][which.min(abs(xy$y[xy$x > x.max1] - y.max1/2))]
#   }
#   
#   if(y.peaks[1,1] == y.max1 & exists("x1_peak")) rm(x1_peak)
#   
#   if(exists("x1_peak")) {
#     
#     x1 <- x1_peak
#     
#     }
#   
#   fwhm <- x2 - x1
#   
#   fwhm 
# 
# }
# 

# fwhm <- function(x,y){
#    
#  xy <- data.frame(x = x,y = y)
#  xy <- xy[xy$x > 30 & xy$x <= 150,]
#  y.peaks <- pracma::findpeaks(xy$y,npeaks = 6)
#  max_y  <- max(y.peaks[,1])
#  y_half <- max_y/2
#  x.max <- xy$x[xy$y == max_y]
#  x1 <- xy$x[xy$x < x.max][which.min(abs(xy$y[xy$x < x.max] - y_half))]
#  x2 <- xy$x[xy$x > x.max][which.min(abs(xy$y[xy$x > x.max] - y_half))]
#   
# }

#
# Use
# resp_times_M336 <- resp_times %>%
#   filter(sujeto == "M326", sesion == 35,cde == 2,phase == "training")
# 
# xy <- density(resp_times_M336$bins %>% na.omit())
# 
# fwhm(xy$x,xy$y)
# 
# x = xy$x
# y = xy$y
# 
# plot(x,y)
#
# set.seed(123)
# x = rnorm(100)
# dx <- density(x)
#
# y = dx$y
# x = dx$x
# fwhm(dx$x,dx$y)
#
# plot(x,y)
# points(c(x1, x2), c(y[x==x1], y[x==x2]), col="red")
# segments(x0 = x1,x1 = x2,y0 = y[x==x1], y1= y[x==x1])
# text(x = x.max1,y = max(y)/2*.9,"FWHM")

