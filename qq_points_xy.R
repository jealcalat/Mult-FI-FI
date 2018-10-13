
qq_points <- function(xy){
  
  sx <- sort(xy[,1]) 
  sy <- sort(xy[,2])
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx) sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx) sy <- approx(1L:leny, sy, n = lenx)$y
  qq <- data_frame(q1 = sx,q2 = sy)
  colnames(qq) <- colnames(xy)
  qq
}
