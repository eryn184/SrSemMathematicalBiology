#Basic Models

library(tidyverse)
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

tnot =1972
sean=10
##r=1.084164

r=2.16



results <- euler(dy.dt=function(t,y){(r*y) - 0.25*(exp((-2*(sin(t*pi))^2)))*y - ((r/1100000)*y^2)}, .1, 253000, 0, sean)

##results <- euler(dy.dt=function(t,y){(.118849*y) - exp(-5*sin(t))*y + ((.118849/1100000)*y^2)}, .1, 253000, 0, 10)


results <- data.frame(results)

ggplot(results, mapping = aes(x = 1:(sean*10 +1), y = results)) +
  geom_point()

results

#test

results <- euler(dy.dt=function(t,y){(cos(t)-y)/(t+1)}, .01, 0, 0, 10)
results <- data.frame(results)

ggplot(results, mapping = aes(x = 1:1001, y = results)) +
  geom_point()


