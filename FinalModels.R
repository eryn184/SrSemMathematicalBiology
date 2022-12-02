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
#dy/dt = r y(t) - B
#y(t)=(y_0-B/r) e^(r(t-t_0))+B/r

#r = .33 (adjusted r based on an isolated value of growth when accounting for harvest effects and starting at 1992)

model2 <- function(y0, t, r, B) {
  (y0 - (B/r)) * exp(r*(t- 1992)) + (B/r)
}


t <- c(1992:2025)
results <- model2(1211000, t, .33, 390703.5)
results <- data.frame(results)
results <- results %>%
  mutate(year = c(1992:2025))

results %>%
  ggplot(mapping = aes(x = year, y = results)) +
  geom_point()+
  geom_line() +
  xlab("Year") +
  ylab("Deer Population") +
  theme_bw()




# Model 3: Model based on logistic equation -------------------------------
# dy/dt = r y(t) - r / K * y^2(t)
# incorporates carrying capacity (K)

years = 30
r=.33
deerpop = 1211000

results<-euler(dy.dt=function(t,y){(r*y) - ((r/1100000)*y^2)}, .1,deerpop , 0, years)

results <- data.frame(results)
results$x <- 1:(years*10+1)
results$year <- 1992+(results$x-1)*0.1

ggplot(results, aes(x= year, y = results)) +
  geom_point() 



# Model 4- Carrying Capacity,  Harvest,  Pop Growth -----------------------
sean = 10 
b = 390700
r=2.16
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
# Similar Code from https://www.r-bloggers.com/2010/03/lotka-volterra-model%C2%A0%C2%A0intro/.
# Actual code written based off of: https://cran.r-project.org/web/packages/deSolve/vignettes/deSolve.pdf
# Then converted into a function
## EXPERIMENTAL MODEL - ISOLATED ENVIRIONMENT


PreyPred <- function(x_prey, y_pred, a, b, g, d){
  
  Pars <- c(a, b, g, d)
  Init_Conds <- c(x = x_prey, y = y_pred)
  
  
  LotVmodel <- function (Time, Init_Conds, Pars) {
    with(as.list(c(Init_Conds, Pars)), {
      dX = x*(a - b*y)
      dY = -y*(g - d*x)
      return(list(c(dX, dY)))
    })
  }
  
  Time <- seq(0, 100, by = 1)
  df <- as.data.frame(ode(func = LotVmodel, y = Init_Conds, parms = Pars, times = Time, method = "euler"))
  
  
  ggplot(df) + 
    geom_line(aes(x=time, y = x, color ="Deer")) +
    geom_line(aes(x=time, y = y, color = "Human")) +
    xlab('Time (in Years)') +
    ylab('Population (in millions)') +
    ggtitle("Experimental Model of an Isolated Enviornment with Deer and Humans") + 
    scale_color_manual(name = "Legend", values = c("Human" = "blue", "Deer" = "red"))
  
}


PreyPred(0.253, 0.2209, 0.11, 0.3, 0.088, 0.15)

## 0.11 is the initial growth rate from 1972 - 1982
## 0.3 is the average rate of which the the amount of deer is harvested each year.
## 0.088 is the average death rate of human beings.
## 0.15 is the rate in which predators increase by consuming prey. Estimate.






# EDA- Basic Graphs of the Data -------------------------------------------
colors <- c("Deer Population" = "red", "Deer Harvest" = "blue", "Deer Hunters" = "orange")

deer <- read_csv("Deer Data (Update1).csv")
names(deer)<-str_replace_all(names(deer), c(" " = "_"))

deer %>%
  ggplot(aes(x = Year)) +
  geom_point(mapping = aes(y = Deer_Population, color = "Deer Population")) +
  geom_line(aes(y = Deer_Population, color = "Deer Population")) +
  geom_point(aes(y = Deer_Harvest, color = "Deer Harvest")) +
  geom_line(aes(y = Deer_Harvest, color = "Deer Harvest")) +
  geom_point(aes(y= Total_Deer_Hunters, color = "Deer Hunters")) +
  geom_line(aes(y = Total_Deer_Hunters, color = "Deer Hunters")) +
  xlab("Year") +
  ylab("Deer Population")+
  labs(color = "Key")+
  scale_color_manual(values = colors)+
  theme_bw()


# Visualizations - model graphs -------------------------------------------

# b(t)
b <- function(t){0.18*(exp((-2*(sin(t*pi))^2)))} 
ggplot(data.frame(x=seq(0,5,by=.001)), aes(x=x)) + 
  stat_function(fun=b) +
  labs(x="t", y="b(t)")

# Sean's model graph (cleaned)
sean = 10 
b = 390700
r=2.16
deerpop = 1211000
results_2<-euler(dy.dt=function(t,y){(r*y) - 0.18*(exp((-2*(sin(t*pi))^2)))*y -  ((r/(1100000+(b/1.4)))*y^2)-(b)}, .1,deerpop , 0, sean)

results_2 <- data.frame(results_2)
colnames(results_2) <- "Deer Population"
results_2$x <- 1:(sean*10+1)
results_2$Year <- 1992+(results_2$x-1)*0.1

ggplot(results_2, mapping = aes(x = Year, y = `Deer Population`)) +
  geom_point()+
  geom_line()



# Euler's Visualizations --------------------------------------------------
# A simplified example of Euler's method with different h values if wanting
# to use visuals to explain Euler's method in paper

#Graph of actual function: y = x^2
x_2 = function(x){x^2}
ggplot(data.frame(x=c(-10:10)), aes(x=x)) + 
  stat_function(fun=x_2)

#Modified (simplified) version of Nicole's function
euler2 <- function(dy.dt=function(t){}, h, y0, start=0, end=1) {
  nsteps <- (end-start)/h
  ys <- numeric(nsteps+1)
  ys[1] <- y0
  for (i in 1:nsteps) {
    t <- start + (i-1)*h
    ys[i+1] <- ys[i] + h*dy.dt(t)
  }
  ys
}

dy_dx = function(x){2*x}

#Euler's: Step size of 5
results_5 <- euler2(dy_dx, 5,100,-10,10)
results_5 <- as.data.frame(results_5)
colnames(results_5) <- "y"
results_5$change_x <- 1:(20/5+1)
results_5$x <- -10+(results_5$change_x-1)*5
ggplot(results_5, aes(x=x, y=y)) +
  geom_point()

#Euler's: Step size of 2
results_4 <- euler2(dy_dx, 2,100,-10,10)
results_4 <- as.data.frame(results_4)
colnames(results_4) <- "y"
results_4$change_x <- 1:(20/2+1)
results_4$x <- -10+(results_4$change_x-1)*2
ggplot(results_4, aes(x=x, y=y)) +
  geom_point()

#Eulers: Step size of 1
results_1 <- euler2(dy_dx, 1,100,-10,10)
results_1 <- as.data.frame(results_1)
colnames(results_1) <- "y"
results_1$change_x <- 1:(20+1)
results_1$x <- -10+(results_1$change_x-1)
ggplot(results_1, aes(x=x, y=y)) +
  geom_point()

#Euler's: Step size of .1
results_2 <- euler2(dy_dx, .1,100,-10,10)
results_2 <- as.data.frame(results_2)
colnames(results_2) <- "y"
results_2$change_x <- 1:(20*10+1)
results_2$x <- -10+(results_2$change_x-1)*.1
ggplot(results_2, aes(x=x, y=y)) +
  geom_point()

#Euler's: Step size of .01
results_3 <- euler2(dy_dx, .01,100,-10,10)
results_3 <- as.data.frame(results_3)
colnames(results_3) <- "y"
results_3$change_x <- 1:(20*100+1)
results_3$x <- -10+(results_3$change_x-1)*.01
ggplot(results_3, aes(x=x, y=y)) +
  geom_point()

