library(tidyverse)
library(growthcurver)
library(deSolve)



v <- read_csv("Deer Data (Update1).csv")
gc_fit <- SummarizeGrowth(v$Year, v$`Deer Population`)
plot(gc_fit)
gc_fit$vals$r

view(growthdata)

  model <- lm(`Deer Population` ~ `Deer Harvest` + `Total Deer Hunters`, data = v)
summary(model)

growth_rate = v %>%
  # first sort by year
  arrange(`Year`) %>%
  mutate(Diff_year = `Year` - lag(`Year`),  # Difference in time (just in case there are gaps)
         Diff_growth = `Deer Population` - lag(`Deer Population`), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/lag(`Deer Population`) * 100) # growth rate in percent

growth_rate2 = growth_rate %>%
  # first sort by year
  arrange(`Year`) %>%
  mutate(Diff_growth_death = `Deer Harvest` - lag(`Deer Harvest`), # Difference in route between years
         Rate_percent_death = ((Diff_growth_death / Diff_year)/lag(`Deer Harvest`) * 100), # growth rate in percent
         Rate_percent_birth = (Rate_percent + Rate_percent_death), 
         Harv_vs_Pop = (`Deer Harvest`/`Deer Population`)) # Difference in route between years



mean(growth_rate$Rate_percent, na.rm = TRUE)

mean(growth_rate2$Rate_percent_death[2:8])

## Basic Exponential Graph with the first population and the average growth rate per year from 1972-1982
x <- c()

for (i in 1:50) {
  t = 25300*exp(0.113438735*i)
  x[i] <- t
}

x
plot(x)

## Carrying Capacity

l <- c()

for(i in 1:15) {
  l[i] <- 0.113438735*growth_rate$`Deer Population`[i] *(1-(growth_rate$`Deer Population`[i]/1100000))
}

plot(l)


l2 <- c()

for(i in 1:1000) {
  p <- 1100000/(253000+3.347826087*exp((-0.113438735)*(1100000)*i))
  l2[i] <- p
}

l2





## Lotka Volterra Model
 

# Example of Lotka Volterra model being used from https://www.r-bloggers.com/2010/03/lotka-volterra-model%C2%A0%C2%A0intro/.
LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)




out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

  

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)







# New method for lotka volterra model
library(demodelr)

## Example found online: 

lynx_hare_eq <- c(
  dHdt ~ r * H - b * H * L,
  dLdt ~ e * b * H * L - d * L
)


# Define the parameters (as a named vector)
lynx_hare_params <- c(r = 2, b = 0.5, e = 0.1, d = 1) # parameters: a named vector

# Define the initial condition (as a named vector)
lynx_hare_init <- c(H = 253, L = 220.9)

# Define deltaT and the time steps:
deltaT <- 0.005 # timestep length
n_steps <- 50 # must be a number greater than 1

# Compute the solution via Euler's method:
out_solution <- euler(system_eq = lynx_hare_eq,
                      parameters = lynx_hare_params,
                      initial_condition = lynx_hare_init,
                      deltaT = deltaT,
                      n_steps = n_steps
)

# Make a plot of the solution, using different colors for lynx or hares.
ggplot(data = out_solution) +
  geom_line(aes(x = t, y = H), color = "red") +
  geom_line(aes(x = t, y = L), color = "blue") +
  labs(
    x = "Time",
    y = "Lynx (red) or Hares (blue)"
  )



PreyPred <- function(x_prey, y_pred, a, b, g, d){
  
  Pars <- c(a, b, g, d)
  State <- c(x = x_prey, y = y_pred)
  
  
  LotkaVoltmodel <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dx = x*(a - b*y)
      dy = -y*(g - d*x)
      return(list(c(dx, dy)))
    })
  }
  
  Time <- seq(0, 100, by = 1)
  out <- as.data.frame(ode(func = LotkaVoltmodel, y = State, parms = Pars, times = Time))
  
  matplot(out[,-1], type = "l", xlab = "Time (Years) ", ylab = "Population (In millions)")
  legend("topright", c("Deer", "Human"), lty = c(1,2), col = c(1,2), box.lwd = 0)
  
}






---------------------------------------------------------------------------------------------------------------------------------------------------------------------------




## My code using: https://cran.r-project.org/web/packages/deSolve/vignettes/deSolve.pdf

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

PreyPred(0.253, 0.2209, 0.11, 0.3, 0.0880, 0.15)

## 0.11 is the initial growth rate from 1972 - 1982
## 0.3 is the average rate of which the the amount of deer is harvested each year.
## 0.088 is the average death rate of human beings.
## 0.15 is the rate in which predators increase by consuming prey. Estimate.