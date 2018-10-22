# fwhm.R and mfw.R 
# Full width half maximum and maximum at full width 
# Different functions. Some trials have more than one peak. This is a problem
# I tried to solve with the more complex one (second)

fwhm <- function(x,y){

  xy <- data.frame(x = x,y = y)
  xmax <- xy$x[xy$y==max(xy$y)][1]
  x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
  x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
  fwhm <- x2 - x1
  list(fwhm = fwhm,peak = xmax)
}
# 
# mfw <- function(x,y){
#   
#   xy <- data.frame(x = x,y = y)
#   xy <- xy[xy$x > 0 & xy$x < 180,]
#   
#   xmax <- xy$x[xy$y==max(xy$y)]
#   x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
#   x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
#   # Resize vector between x1 and x2
#   xy <- xy[xy$x >= x1 & xy$x <= x2,]
#   xmax2 <- xy$x[xy$y==max(xy$y)]
#   xmax2
# }
# 
# fwhm <- function(x,y){
# 
#   xy <- data.frame(x = x,y = y)
#   xy <- xy[xy$x > 0 & xy$x < 180,]
#   y.peaks <- pracma::findpeaks(xy$y,npeaks = 2)
# 
#   if(length(y.peaks[,1]) !=1 ){
#     p1 <- xy[xy$y == y.peaks[1,1],1]
#     p2 <- xy[xy$y == y.peaks[2,1],1]
#     # Find minimum, which will be the cutoff point
#     min <- optimize(approxfun(xy$x,xy$y),interval=c(p1,p2))$minimum
#     xy <- xy[xy$x <= min,]
#   }
# 
#   xmax <- xy$x[xy$y==max(xy$y)]
#   x1 <- xy$x[xy$x < xmax][which.min(abs(xy$y[xy$x < xmax] - max(xy$y)/2))]
#   x2 <- xy$x[xy$x > xmax][which.min(abs(xy$y[xy$x > xmax] - max(xy$y)/2))]
#   fwhm <- x2 - x1
# 
#   list(fwhm = fwhm,peak = xmax)
# 
# }

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

