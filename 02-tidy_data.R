library(tidyverse)
getwd()
  
combined.tempdata <- read.csv("combined data\\combined.tempdata.csv")
head(combined.tempdata)

#move into seperate dataframe for initial stats
cleaned.tempdata <- combined.tempdata
#turn "max" string into max plate area so can turn into numeric variable
pi * ((135/2) ^ 2)
cleaned.tempdata$totalgrowth.mm <- gsub("max", "14313.88", 
                                        cleaned.tempdata$totalgrowth.mm) 

#check all values of "max" has been changed into max plate area
filter(cleaned.tempdata, totalgrowth.mm == "max") %>% summarise(n())
filter(cleaned.tempdata, totalgrowth.mm == "14313.88") %>% summarise(n())
#sanity check if all "max" in original df have been replaced with pi r ^ 2 value
filter(combined.tempdata, totalgrowth.mm == "max") %>% summarise(n()) == 
  filter(cleaned.tempdata, totalgrowth.mm == "14313.88") %>% summarise(n())

#turn totalgrowth.mm into numeric vector for futher analysis
cleaned.tempdata$totalgrowth.mm <- as.numeric(cleaned.tempdata$totalgrowth.mm)

#check for number of results above max plate size and then replace with max
#plate size
filter(cleaned.tempdata, totalgrowth.mm > 14313.88) %>%
  summarise(n())

cleaned.tempdata$totalgrowth.mm[cleaned.tempdata$totalgrowth.mm > 14313.88] <- 14313.88

#check number of totalgrowth.mm variables that are above max plate area
if (filter(cleaned.tempdata, totalgrowth.mm > 14313.88) %>% 
    summarise(n())  > 0){
  filter(cleaned.tempdata, totalgrowth.mm > 14313.88)
} else {
  print("No variables above max plate size")
}

#check number of NA values - try to find these if possible!
sum(is.na(cleaned.tempdata$totalgrowth.mm))
filter(cleaned.tempdata, is.na(cleaned.tempdata$totalgrowth.mm))

write.csv(cleaned.tempdata, 
          file = "cleaned data\\cleaned.tempdata.csv", row.names = FALSE)

