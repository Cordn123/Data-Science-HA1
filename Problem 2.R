rm(list=ls())

library("dplyr")
library("readr")

unzip("hw1p2.zip") #Decided to just unzip the bloody thing, sorry not sorry 

Data <- list.files("hw1p2", full.names = TRUE) %>%
lapply(read_csv) %>%
bind_cols 
#Dataset created, but id column duplictes still remain, fix pending


