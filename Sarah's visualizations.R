library(tidyverse)

#Population
deer_data <- read_csv("Deer Data (Update1).csv")
ggplot(deer_data, aes(x=Year, y=`Deer Population`)) +
  geom_point()+
  geom_line()

#Harvest
harvest_data <- read_csv("deer_harvest.csv")
#Since 2000
ggplot(harvest_data, aes(x = Season, y = `Total harvest`))+
  geom_point()+
  geom_line()

#All harvest
#harvest before 2000
deer_data_harvest <- deer_data[1:4,c(1,4)]
colnames(deer_data_harvest) <- c("Season", "Total harvest")
all_harvest <- rbind(deer_data_harvest, harvest_data)
ggplot(all_harvest, aes(x = Season, y = `Total harvest`))+
  geom_point()+
  geom_line()

#b(t)
b <- function(t){0.18*(exp((-2*(sin(t*pi))^2)))} 
ggplot(data.frame(x=seq(0,5,by=.001)), aes(x=x)) + 
  stat_function(fun=b) +
  labs(x="t", y="b(t)")

#Sean's model graph
sean = 10 
b = 390700
r=2.16
deerpop = 1211000 ##253000 
results_2<-euler(dy.dt=function(t,y){(r*y) - 0.18*(exp((-2*(sin(t*pi))^2)))*y -  ((r/(1100000+(b/1.4)))*y^2)-(b)}, .1,deerpop , 0, sean)

results_2 <- data.frame(results_2)
colnames(results_2) <- "Deer Population"
results_2$x <- 1:(sean*10+1)
results_2$Year <- 1992+(results_2$x-1)*0.1

ggplot(results_2, mapping = aes(x = Year, y = `Deer Population`)) +
  geom_point()+
  geom_line()

#Logistic model graph
years = 20
r=.33
deerpop = 1211000

results<-euler(dy.dt=function(t,y){(r*y) - ((r/1100000)*y^2)}, .1,deerpop , 0, years)

results <- data.frame(results)
colnames(results) <- "Deer Population"
results$x <- 1:(years*10+1)
results$Year <- 1992+(results$x-1)*0.1

ggplot(results, aes(x= Year, y = `Deer Population`)) +
  geom_point() 

#Euler's method visualization
#Graph of actual function
x_2 = function(x){x^2}
ggplot(data.frame(x=c(-10:10)), aes(x=x)) + 
  stat_function(fun=x_2)

#Modified (simplified) version of Nicole's function
euler <- function(dy.dt=function(t){}, h, y0, start=0, end=1) {
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
results_5 <- euler(dy_dx, 5,100,-10,10)
results_5 <- as.data.frame(results_5)
colnames(results_5) <- "y"
results_5$change_x <- 1:(20/5+1)
results_5$x <- -10+(results_5$change_x-1)*5
ggplot(results_5, aes(x=x, y=y)) +
  geom_point()

#Euler's: Step size of 2
results_4 <- euler(dy_dx, 2,100,-10,10)
results_4 <- as.data.frame(results_4)
colnames(results_4) <- "y"
results_4$change_x <- 1:(20/2+1)
results_4$x <- -10+(results_4$change_x-1)*2
ggplot(results_4, aes(x=x, y=y)) +
  geom_point()

#Eulers: Step size of 1
results_1 <- euler(dy_dx, 1,100,-10,10)
results_1 <- as.data.frame(results_1)
colnames(results_1) <- "y"
results_1$change_x <- 1:(20+1)
results_1$x <- -10+(results_1$change_x-1)
ggplot(results_1, aes(x=x, y=y)) +
  geom_point()

#Euler's: Step size of .1
results_2 <- euler(dy_dx, .1,100,-10,10)
results_2 <- as.data.frame(results_2)
colnames(results_2) <- "y"
results_2$change_x <- 1:(20*10+1)
results_2$x <- -10+(results_2$change_x-1)*.1
ggplot(results_2, aes(x=x, y=y)) +
  geom_point()

#Euler's: Step size of .01
results_3 <- euler(dy_dx, .01,100,-10,10)
results_3 <- as.data.frame(results_3)
colnames(results_3) <- "y"
results_3$change_x <- 1:(20*100+1)
results_3$x <- -10+(results_3$change_x-1)*.01
ggplot(results_3, aes(x=x, y=y)) +
  geom_point()




