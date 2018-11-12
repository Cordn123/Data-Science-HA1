rm(list=ls())

library(tidyverse) 

# "readr" (generally better at reading things), 
# "magrittr" (AKA the steampunk piping machine), and 
# "dplyr" (the sexy Quenn of data manipulation) will be used 

# It's easier to call them up through "tidyverse" (One package to rule them all)

unzip("hw1p2.zip") #Decided to just unzip the bloody thing, sorry not sorry 

Data <- list.files("hw1p2", full.names = TRUE) %>% #Using piping to create a dataframe 
lapply(read_csv) %>%
bind_cols() #Dataframe for a dataset created

#Without dplyr and readr the code could look like this (for example):

# Filenames <- list.files("hw1p2", full.names = TRUE)
# Data <- lapply(Filenames, function(i){
#  read.csv(i, header=TRUE)
# })
# DataFrame <- cbind.data.frame(Data)

#Which is extremely cumbersome and boring, so we'll stick with dplyr

library(broom) # "broom" allows for a smooth transition from lm output to an orginised dataframe 

Data %>%
  select(starts_with("x")) %>% 
  names() %>% 
  assign("X", . , pos = 1) #Extracted varible column names and stored them in "X" 
 
# Running the regression analysis via piping: 

# 1) Creating a list of equations ( via "lapply") for all combinations of regressors 
#through recursive use of the "paste0" function 

# 2) Simplifying to create a vector of regression equations using the "unlist" function 

# 3) Running the regression and outputing a dataframe with summary statistics ("bind_rows" and "glance" functions) for all 70 models 
#and matching to correpsonding equations. Then storing the dataframe in "models" via the "assign" function 

lapply(4, function(n) combn(X , n, FUN = function(row) paste0("y ~ ", paste0(row, collapse = "+")))) %>% 
 unlist() %>%
  lapply(. ,function(y) { 
    models = glance(lm(y, Data))
    models$y = y
    return(models) }) %>% 
    bind_rows() %>% 
    assign("models", . , pos = 1)

filter(models, r.squared == max(r.squared)) #filtering to find the model with the highest R^2
    








  




 
  
  

  

  

  



  
  







