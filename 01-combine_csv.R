library(tidyverse) 
getwd()

home <- getwd()
####does not work for 25
###but works for 30 
##dont know why
##arrr
###25 Degrees Section
folder25dir <- "raw data\\25degrees"
setwd(folder25dir)
temperaturefiles.25 <- list.files(pattern = '*.csv')

#get isolate names
isolate.names <- gsub(".csv", "", temperaturefiles.25) %>%
  strsplit("25degrees") %>%
  sapply("[[", 2)

listof25degrees <- lapply(temperaturefiles.25, read.csv, header = TRUE)
names(listof25degrees) <- isolate.names

combined25degrees <-  do.call(rbind, listof25degrees) %>%
  as.tibble() %>%
  mutate(temperature = 25)

head(combined25degrees)

setwd(home)

write.csv(combined25degrees, 
          file = "combined data\\25degreescombined.csv", row.names = FALSE)

###30 Degrees Section
folder30dir <- "raw data\\30degrees"
setwd(folder30dir)
temperaturefiles.30 <- list.files(pattern = '*.csv')

#get isolate names
isolate.names <- gsub(".csv", "", temperaturefiles.30) %>%
  strsplit("30degrees") %>%
  sapply("[[", 2)

listof30degrees <- lapply(temperaturefiles.30, read.csv, header = TRUE)
names(listof30degrees) <- isolate.names

combined30degrees <-  do.call(rbind, listof30degrees) %>%
  as.tibble() %>%
  mutate(temperature = 30)

head(combined30degrees)

write.csv(combined30degrees, 
          file = "combined data\\30degreescombined.csv",
          row.names = FALSE)

###15 Degrees Section
folder15dir <- "raw data\\15degrees"
setwd(folder15dir)
temperaturefiles.15 <- list.files(pattern = '*.csv')

#get isolate names
isolate.names <- gsub(".csv", "", temperaturefiles.15) %>%
  strsplit("15degrees") %>%
  sapply("[[", 2)

listof15degrees <- lapply(temperaturefiles.15, read.csv, header = TRUE)
names(listof15degrees) <- isolate.names

combined15degrees <-  do.call(rbind, listof15degrees) %>%
  as.tibble() %>%
  mutate(temperature = 15)

write.csv(combined15degrees, 
          file = "15degreescombined.csv",
          row.names = FALSE)

###get isolate names
combined25degrees$sample<- gsub("25", ".", combined25degrees$sample)
combined25degrees <- separate(combined25degrees, sample, into = c("isolate", "replicate"))

combined30degrees$sample<- gsub("30", ".", combined30degrees$sample)
combined30degrees <- separate(combined30degrees, sample, into = c("isolate", "replicate"))

combined15degrees$sample<- gsub("15", ".", combined15degrees$sample)
combined15degrees <- separate(combined15degrees, sample, into = c("isolate", "replicate"))

###combine all three datasets
fifteen <- combined15degrees
twentyfive <- combined25degrees
thirty <- combined30degrees

#need to change columns!
combined.tempdata <- rbind(twentyfive, thirty, fifteen) 
as.tibble(combined.tempdata)

write.csv(combined.data, 
          file = "combined data\\combined.data.csv",
          row.names = FALSE)
  
head(combined.tempdata)  
