install.packages("lme4")
library(lme4)

head(alltempdata)
str(alltempdata)

#add data - actual growth and then growth rate
alltempdata <- mutate(alltempdata, actualgrowth.mm = totalgrowth.mm - plugarea.mm, 
                      growthrate = actualgrowth.mm / timepoint)

plot(jitter(alltempdata$temperature), alltempdata$growthrate)

#changing data for glm analysis
glmdata <- alltempdata %>%
  filter(hit.edge == "no", contaminated == "no") 
head(glmdata)

glmdata$replicate <- as.factor(glmdata$replicate)

temp15 <- subset(glmdata, temperature == 15)
temp15$replicate <- with(temp15, reorder(replicate, growthrate, median, na.rm=T))
boxplot(growthrate ~ replicate, data=temp15)

temp.model <- lmer(growthrate ~ temperature + (1|isolate/replicate), 
                   REML = TRUE, data = alltempdata, na.action = na.exclude)

summary(temp.model)
