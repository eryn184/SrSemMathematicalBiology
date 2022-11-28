# Final Models for Paper

library(tidyverse)
library(deSolve)



# Euler's Method Function -------------------------------------------------

euler <- function(dy.dt=function(t,y){}, h, y0, start=0, end=1) {
  nsteps <- (end-start)/h
  ys <- numeric(nsteps+1)
  ys[1] <- y0
  for (i in 1:nsteps) {
    t <- start + (i-1)*h
    ys[i+1] <- ys[i] + h*dy.dt(t,ys[i])
  }
  ys
}

# Model 1- Natural Pop Growth ---------------------------------------------

#Start with the natural dynamics. The law of exponential evolution or Malthus law. 
#We use r = .118849 based on file 'EDA.R' growth rate from first two data points (before carrying capacity is reached)

#dy/dt = ry(t)

r <- .118849

model1 <- function(r, t) {
  exp(r*(t-1972)) + 253000
}

t <- c(1972:2030)

results <- model1(r, t)
results <- data.frame(results)
results <- results %>%
  mutate(year = c(1972:2030))

results %>%
  ggplot(mapping = aes(x = year, y = results)) +
  geom_point()+
  geom_line() +
  xlab("Year") +
  ylab("Deer Population") +
  theme_bw()




# Model 2- Natural Growth and Constant Harvest by Time --------------------
#Model 2
#dy/dt = r y(t) + b(t) y(t) 


model2 <- function(y0, t, r, B) {
  (y0 - (B/r)) * exp(r*(t- 1992)) + (B/r)
}


model2(1211000, 2000, .33, 390703.5)




# Model 3- Carrying Capacity,  Harvest,  Pop Growth -----------------------
sean = 10 
b = 390700
deerpop = 1211000 ##253000 
## b is the estimated harvest per year
## results 2 is the carrying capacity model plus estimated harvest per year it is a little low 
## Adjusted carrying capacity as it was being affected by the constant harvest 
results_2<-euler(dy.dt=function(t,y){(r*y) - 0.18*(exp((-2*(sin(t*pi))^2)))*y -  ((r/(1100000+(b/1.4)))*y^2)-(b)}, .1,deerpop , 0, sean)


## adjusted the carrying capacity value in the function by adding back in a factor of the estimated harvest value per year



results_2 <- data.frame(results_2)

ggplot(results_2, mapping = aes(x = 1:(sean*10 +1), y = results_2)) +
  geom_point()






# Lotka-Volterra Model ----------------------------------------------------
#An experimental model treating humans as a "predator" of deer
# Main Code from https://www.r-bloggers.com/2010/03/lotka-volterra-model%C2%A0%C2%A0intro/. 
# Then converted into a function
## EXPERIMENTAL MODEL - ISOLATED ENVIRIONMENT


PreyPred <- function(x_prey, y_pred, a, b, g, d){
  
  Pars <- c(a, b, g, d)
  State <- c(x = x_prey, y = y_pred)
  
  
  LotVmod <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dx = x*(a - b*y)
      dy = -y*(g - d*x)
      return(list(c(dx, dy)))
    })
  }
  
  Time <- seq(0, 100, by = 1)
  out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
  
  ggplot(out) + 
    geom_line(aes(x=time, y = x, color ="Deer")) +
    geom_line(aes(x=time, y = y, color = "Human")) +
    xlab('Time (in Years)') +
    ylab('Population (in millions)') +
    ggtitle("Experimental Model of an Isolated Enviornment with Deer and Humans") + 
    scale_color_manual(name = "Legend", values = c("Human" = "blue", "Deer" = "red"))
  
}

PreyPred(0.253, 0.2209, 0.11, 0.3, 0.088, 0.15)






# EDA- Basic Graphs of the Data -------------------------------------------

deer %>%
  ggplot(aes(x = Year)) +
  geom_point(mapping = aes(y = Deer_Population)) +
  geom_line(aes(y = Deer_Population)) +
  geom_point(aes(y = Deer_Harvest), color = "red") +
  geom_line(aes(y = Deer_Harvest), color = "red") +
  geom_point(aes(y= Total_Deer_Hunters), color = "blue") +
  geom_line(aes(y = Total_Deer_Hunters), color = "blue") +
  xlab("Year") +
  ylab("Deer Population")+
  theme_bw()

