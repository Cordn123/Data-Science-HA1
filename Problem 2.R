rm(list=ls())

library(tidyverse) 

#Packages used:
# "readr" 
# "magrittr" 
# "dplyr" 

# It's easier to call them up through the "tidyverse" 

#Creating a list of names of files in the archive
list_files<-unzip("hw1p2.zip",list=TRUE)[,1]
#Locating the .csv files
list_files<-list_files[str_detect(list_files,".csv")]
#Establishing the connection to the archive with the "unz" function and looping through the files to store the data in "Data"
Data<-list("vector")
for (i in 1:length(list_files)) {
  conn<-unz("hw1p2.zip", list_files[i])
  Data[[i]]<-(read_csv(conn))
} 
#Binding the columns in the 9 resulting dataframes to fuse them into a single dataframe
Data <- bind_cols(Data)

library(broom) # "broom" allows for a smooth transition from lm output to an orginised dataframe 

Data %>%
  select(starts_with("x")) %>% 
  names() %>% 
  assign("X", . , pos = 1) #Extracted varible column names and stored them in "X" 

# Running the regression analysis via piping: 

# 1) Creating a list of equations ( via "lapply") for all combinations of regressors 
  # through recursive use of the "paste0" function. 

lapply(4, function(n) combn(X , n, FUN = function(row) paste0("y ~ ", paste0(row, collapse = "+")))) %>% 

  # The result are 70 combinations of 4 x[i]s with "+" pasted inbetween and "y~" pasted to the left.

# 2) Simplifying to create a vector of regression equations using the "unlist" function 
   
  unlist() %>%

# 3) Running the regression and outputing a dataframe with summary statistics ("bind_rows" and "glance" functions) 
  # for all 70 models and matching to correpsonding equations. 
  lapply(. ,function(y) { 
    models = glance(lm(y, Data)) # "glance" outputs a dataframe with summary statistics for a given regression
    models$y = y #attaching a column with the corresponding model equation to individual summaries
    return(models) }) %>% 
  bind_rows() %>% #Binding all summaries by rows to create a dataframe 
  
# 4) Storing the dataframe in "models" via the "assign" function with "pos" set to 1 for global environment
  assign("models", . , pos = 1)

filter(models, r.squared == max(r.squared)) #filtering to find the model with the highest R^2