library(tidyverse)

head(combined.tempdata)

#move into seperate dataframe for initial stats
alltempdata <- combined.tempdata
#turn "max" string into max plate area so can turn into numeric variable
pi * ((135/2) ^ 2)
alltempdata$totalgrowth.mm <- gsub("max", "14313.88", alltempdata$totalgrowth.mm) 

#check all values of "max" has been changed into max plate area
filter(alltempdata, totalgrowth.mm == "max") %>% summarise(n())
filter(alltempdata, totalgrowth.mm == "14313.88") %>% summarise(n())
#sanity check if all "max" in original df have been replaced with pi r ^ 2 value
filter(combined25degrees, totalgrowth.mm == "max") %>% summarise(n()) == 
  filter(c25dfstats, totalgrowth.mm == "14313.88") %>% summarise(n())

#turn totalgrowth.mm into numeric vector for futher analysis
alltempdata$totalgrowth.mm <- as.numeric(alltempdata$totalgrowth.mm)

#check for number of results above max plate size and then replace with max
#plate size
filter(alltempdata, totalgrowth.mm > 14313.88) %>%
  summarise(n())

alltempdata$totalgrowth.mm[alltempdata$totalgrowth.mm > 14313.88] <- 14313.88

#check number of totalgrowth.mm variables that are above max plate area
if (filter(alltempdata, totalgrowth.mm > 14313.88) %>% 
    summarise(n())  > 0){
  filter(alltempdata, totalgrowth.mm > 14313.88)
} else {
  print("No variables above max plate size")
}

#check number of NA values - try to find these if possible!
sum(is.na(alltempdata$totalgrowth.mm))
filter(c25dfstats, is.na(c25dfstats$totalgrowth.mm))
