#Basic Models


#Model 1
#Start with the natural dynamics. The law of exponential evolution or Malthus law. 
#We use r = .118849 based on file 'EDA.R' growth rate from first two data points (before carrying capacity is reached)

#dy/dt = ry(t)

r <- .118849

model1 <- function(r, t) {
  exp(r*t)
}



#Model 2
#dy/dt = r y(t) + b(t) y(t) 
#b(t) = sin(t)
#dy/dt = r y(t) + sin(t) y(t)


euler(dy.dt=function(t,y){(.118849*y) + ((.118849/1100000)*y^2)}, .1, 253000)




