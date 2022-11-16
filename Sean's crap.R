tnot =1972
sean=10
##r=1.084164

r=2.16



results <- euler(dy.dt=function(t,y){(r*y) - .18*(exp((-2*(sin(t*pi))^2)))*y - ((r/1100000)*y^2)}, .1, deerpop, 0, sean)
results <- data.frame(results)

ggplot(results, mapping = aes(x = 1:(sean*10 +1), y = results)) +
  geom_point()

results

b = 390700
deerpop = 253000 ##1211000

results_2<-euler(dy.dt=function(t,y){(r*y) - 0.18*(exp((-2*(sin(t*pi))^2)))*y -  ((r/1100000)*y^2)-(b)}, .1,deerpop , 0, sean)


results_2 <- data.frame(results_2)

ggplot(results_2, mapping = aes(x = 1:(sean*10 +1), y = results_2)) +
  geom_point()

results



