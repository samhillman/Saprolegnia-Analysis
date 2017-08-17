library(tidyverse)

cleaned.tempdata <- read.csv("cleaned data\\cleaned.tempdata.csv")

#add data - actual growth and then growth rate
growthrate.tempdata <- mutate(cleaned.tempdata, 
                          actualgrowth.mm = totalgrowth.mm - plugarea.mm, 
                          growthrate = actualgrowth.mm / timepoint)

write.csv(growthrate.tempdata, 
          file = "cleaned data\\growthrate.tempdata.csv", row.names = FALSE)

#see how many of each temperature point have
group_by(growthrate.tempdata, temperature) %>%
  summarise(n())

theme_set(theme_bw())

##### Line graphs of growth rate vs temperature
#generates summary of mean growth rates
summarydata <- clean.tempdata %>%
  group_by(isolate, temperature) %>%
  summarise(mean.growthrate = mean(growthrate, na.rm = TRUE)) %>%
  ungroup() 
#plots the mean growth rates against temperature (could combine in one pipe)
ggplot(summarydata) +
  aes(x = temperature, y = mean.growthrate, colour = isolate) +
  geom_point() +
  geom_smooth()
#OR facet wrap by isolate 
ggplot(summarydata) +
  aes(x = temperature, y = mean.growthrate) +
  facet_wrap(~isolate) +
  geom_point() + 
  geom_smooth()

##### Box plots
plot(jitter(clean.tempdata$temperature), clean.tempdata$growthrate)

#add new variable with data needed for box plot - can be piped if needed
boxplot.data <- select(clean.tempdata, isolate, replicate, temperature, growthrate)

ggplot(boxplot.data) +
  aes(x = factor(temperature), y = growthrate) +
  geom_boxplot() +
  facet_wrap(~isolate) +
  labs(title = "Growthrate of Saprolegnia parasitica 
       in different temperature regimes",
       x = "Temperature (degrees Celcius)",
       y = "Growthrate (mm^2 / hour)")

filter(clean.tempda)





