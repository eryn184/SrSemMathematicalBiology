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
b_t <- data.frame(matrix(ncol = 2, nrow = 5001))
colnames(b_t)<-c("t","y")
b_t$t <- seq(0,5,by=.001)
b_t$y <- b(b_t$t)
ggplot(b_t, aes(x=t,y=y))+
  geom_point()

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
