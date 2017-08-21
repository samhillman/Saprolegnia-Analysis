install.packages("lme4")
install.packages("pbkrtest")
install.packages("MuMIn")
library(lme4)
library(tidyverse)
library(pbkrtest)
library(MuMIn)

glm.tempdata <- read.csv("cleaned data\\growthrate.tempdata.csv")
head(glm.tempdata)

#add data - actual growth and then growth rate
#clean data - filter out where edge has been hit and the contaminated
glm.tempdata <- mutate(glm.tempdata, actualgrowth.mm = totalgrowth.mm - 
                         plugarea.mm, 
                      growthrate = actualgrowth.mm / timepoint) %>%
                filter(hit.edge == "no", contaminated == "no") 

glm.tempdata$replicate <- as.factor(glm.tempdata$replicate)

#mixed effect model
temp.model <- lmer(growthrate ~ temperature + daysinculture + 
                     speciesisolatedfrom + (1|isolate/replicate), 
                   REML = TRUE, data = glm.tempdata, na.action = na.exclude)

summary(temp.model)
#get pvalues from model
drop1(temp.model, test = "Chisq")
#large rsquared value means random effect is having an effect
r.squaredGLMM(temp.model)

#model validation
hist(glm.tempdata$growthrate)
qqnorm(glm.tempdata$growthrate)
qqline(glm.tempdata$growthrate, lty = 2)
#checking normal distribution of residuals
sresid <- resid(temp.model, type = "pearson")
hist(sresid)
#plots residuals vs fitted values
fits <- fitted(temp.model)
plot(sresid ~ fits)
#check for equal variances / homoscedasticity
plot(temp.model)

#####Testing other variations of the model
#test to see if instead of (1|isolate/replicate) can use unique ID value instead
test2 <- unite(glm.tempdata, id, isolate, replicate, temperature, sep=".", 
               remove = FALSE)
test2.model <- lmer(growthrate ~ temperature + daysinculture + 
                      speciesisolatedfrom + (1|id), 
                    REML = TRUE, data = test2, na.action = na.exclude)

summary(test2.model)
#here we find that days in culture is now non-significant, and that there is
#negligible different in the rsquared value so interaction is expkaining a lot 
#of our growthrate (y variable)
drop1(test2.model, test = "Chisq")
r.squaredGLMM(test2.model)


#can try to take out the random effect in total to see what happens
test3.model <- glm(growthrate ~ temperature + daysinculture + 
                     speciesisolatedfrom, 
                    family = gaussian, data = test2, na.action = na.exclude)

#so temp and species is significant even without the random effect
summary(test3.model)
drop1(test3.model, test = "F")

#this null model and rsquared represents the variance in y that is explained by
#the random structure only (ie random effect of isolate/replicate)
#as the R2C (conditional rsquared) value is low (0.07) then the remaining 
#variance must be explained by the dependant variables!
null.model <- lmer(growthrate ~ 1 + (1|isolate/replicate), 
                   REML = TRUE, data = glm.tempdata, na.action = na.exclude)
r.squaredGLMM(null.model)



