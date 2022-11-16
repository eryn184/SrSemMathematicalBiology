
library(tidyverse)
deer <- read_csv("Deer Data (Update1).csv")

names(deer)<-str_replace_all(names(deer), c(" " = "_"))

deer <- deer[-9, ]

mean(deer$Deer_Harvest)
#390703.5


deer2 <- read_csv("Deer Data (Update1).csv")
names(deer)<-str_replace_all(names(deer), c(" " = "_"))

(540000 - 253000)/ 253000
# r = 1.134387
# 113.4387%

(1211000 - 540000)/ 540000
#r = 1.242593
#124.2593%

(1.134387 + 1.242593)/2
# r = 1.18849




# finding an r independent of harvest -------------------------------------

deer3 <- read_csv("Deer Data (Update1).csv")
names(deer3)<-str_replace_all(names(deer3), c(" " = "_"))
deer3 <- deer3[-1, ]

deer <- deer[-1,]


mean(deer$Deer_Harvest)
#390703.5


deer3$Deer_Population <- (deer3$Deer_Population - 390703.5)


#new r value, hopefully independent 

(961296.5 - 820296.5)/820296.5
r = 0.1718891

(878296.5 - 961296.5)/961296.5








# model 2- harvest/time ---------------------------------------------------


model2 <- function(y0, t, r, B) {
  (y0 - (B/r)) * exp(r*(t- 1992)) + (B/r)
}


model2(1211000, 2000, .33, 390703.5)






# EDA to figure this relationship out -------------------------------------

deer <- read_csv("Deer Data (Update1).csv")
names(deer)<-str_replace_all(names(deer), c(" " = "_"))
deer <- deer[]



deer %>%
  ggplot(aes(x = Year)) +
  geom_point(mapping = aes(y = Deer_Population)) +
  geom_line(aes(y = Deer_Population)) +
  geom_point(aes(y = Deer_Harvest)) +
  geom_line(aes(y = Deer_Harvest))

deer <- deer %>%
  mutate(DeerPopNatural = Deer_Population + Deer_Harvest)

deer %>%
  ggplot(aes(x = Year)) +
  geom_point(mapping = aes(y = Deer_Population, color = "red")) +
  geom_line(aes(y = Deer_Population, color = "red")) +
  geom_point(aes(y = DeerPopNatural)) +
  geom_line(aes(y = DeerPopNatural)) +
  geom_line(aes(y = Deer_Harvest)) +
  theme_bw()

deer %>%
  ggplot(aes(x = Year)) +
  geom_point(mapping = aes(y = Deer_Population, color = "red")) +
  geom_line(aes(y = Deer_Population, color = "red")) +
  geom_point(aes(y = DeerPopNatural)) +
  geom_line(aes(y = DeerPopNatural)) +
  ylim(900000,1800000)




()