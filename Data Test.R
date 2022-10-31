library(tidyverse)
library(growthcurver)



v <- read_csv("Deer Data - Copy.csv")
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

