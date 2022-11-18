tnot =1972
sean=10
##r=1.084164

r=2.16



results <- euler(dy.dt=function(t,y){(r*y) - .18*(exp((-2*(sin(t*pi))^2)))*y - ((r/1100000)*y^2)}, .1, deerpop, 0, sean)
results <- data.frame(results)

ggplot(results, mapping = aes(x = 1:(sean*10 +1), y = results)) +
  geom_point()

results

## doesnt plot just for reference on the fact that i came to the conclusion to add b back into carry cap

results_15<-euler(dy.dt=function(t,y){(r*y) - 0.18*(exp((-2*(sin(t*pi))^2)))*y -((r/(1100000))*y^2) -b}, .1,deerpop , 0, sean)
  results_15 <- data.frame(results)
  
  ggplot(results_15, mapping = aes(x = 1:(sean*10 +1), y = results_15)) +
    geom_point()

b = 390700
deerpop = 1211000 ##253000 
## b is the estimated harvest per year
## results 2 is the carrying capacity model plus estimated harvest per year it is a little low 
results_2<-euler(dy.dt=function(t,y){(r*y) - 0.18*(exp((-2*(sin(t*pi))^2)))*y -  ((r/(1100000+(b/1.4)))*y^2)-(b)}, .1,deerpop , 0, sean)


## adjusted the carrying capacity value in the function by adding back in a factor of the estimated harvest value per year



results_2 <- data.frame(results_2)

ggplot(results_2, mapping = aes(x = 1:(sean*10 +1), y = results_2)) +
  geom_point()

results



