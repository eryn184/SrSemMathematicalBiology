#Euler Method


euler <- function(dy.dx=function(x,y){}, h, y0, start=0, end=1) {
  nsteps <- (end-start)/h
  ys <- numeric(nsteps+1)
  ys[1] <- y0
  for (i in 1:nsteps) {
    x <- start + (i-1)*h
    ys[i+1] <- ys[i] + h*dy.dx(x,ys[i])
  }
  ys
}
