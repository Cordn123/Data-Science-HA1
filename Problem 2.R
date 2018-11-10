rm(list=ls())

library("dplyr")
library("readr")

unzip("hw1p2.zip") #Decided to just unzip the bloody thing, sorry not sorry 

<<<<<<< HEAD
Data <- list.files("hw1p2", full.names = TRUE) %>% #Using piping to create a dataframe 
lapply(read_csv) %>%
bind_cols #Dataframe for a dataset created, but id column duplictes still remain, fix pending
=======
Data <- list.files("hw1p2", full.names = TRUE) %>%
lapply(read_csv) %>%
bind_cols 
#Dataset created, but id column duplictes still remain, fix pending
>>>>>>> e737c96edd6c1314b9458d41daf31db375468c85


