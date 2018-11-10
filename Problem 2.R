rm(list=ls())

library("dplyr") #The sexy Queen of data manipulation. 
library("readr") #Just generally better at reading files

unzip("hw1p2.zip") #Decided to just unzip the bloody thing, sorry not sorry 

Data <- list.files("hw1p2", full.names = TRUE) %>% #Using piping to create a dataframe 
lapply(read_csv) %>%
bind_cols #Dataframe for a dataset created, but id column duplictes still remain, fix pending

#Without dplyr and readr the code could look like this (for example):

# Filenames <- list.files("hw1p2", full.names = TRUE)
# Data <- lapply(Filenames, function(i){
#  read.csv(i, header=TRUE)
# })
# DataFrame <- cbind.data.frame(Data)

#Which is extremely cumbersome and boring, so we'll stick with dplyr

