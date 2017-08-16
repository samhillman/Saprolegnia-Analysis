library(tidyverse)

head(alltempdata)

alltempdata.summary <- alltempdata %>%
  group_by(isolate, temperature) %>%
  summarise(mean.growthrate = mean(growthrate, na.rm = TRUE)) %>%
  ungroup()

glmdata %>%
  group_by(isolate, temperature) %>%
  summarise(mean.growthrate = mean(growthrate, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = isolate, y = temperature, colour = isolate) +
  geom_line()

boxplot(temperature ~ mean.growthrate)

head(alltempdata.summary)



