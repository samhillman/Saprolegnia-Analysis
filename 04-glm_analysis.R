install.packages("lme4")
library(lme4)
library(tidyverse)

glm.tempdata <- read.csv("cleaned data\\cleaned.tempdata.csv")

head(glm.tempdata)
str(glm.tempdata)

#add data - actual growth and then growth rate
#clean data - filter out where edge has been hit and the contaminated
glm.tempdata <- mutate(glm.tempdata, actualgrowth.mm = totalgrowth.mm - plugarea.mm, 
                      growthrate = actualgrowth.mm / timepoint) %>%
                filter(hit.edge == "no", contaminated == "no") 

head(glm.tempdata)

glm.tempdata$replicate <- as.factor(glm.tempdata$replicate)

temp.model <- lmer(growthrate ~ temperature + (1|isolate/replicate), 
                   REML = TRUE, data = glm.tempdata, na.action = na.exclude)

summary(temp.model)


