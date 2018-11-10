#Problem 3
#3.1
library(data.table)
setwd("/Users/PatratskyAlexander/Desktop/ICEF\ 3rd\ Year/Data\ Science/HA/HA1")
data <- read.csv('hw1p3.csv', stringsAsFactors = FALSE) #also possible to use fread
summary(data)
View(data)

#using loops
vec_of_na <- c(length=length(data)) #правда в этом случае не подписаны переменные. Можно попробовать лист, 
#но там тоже придется переименовывать элементы
for (i in 1:ncol(data)){
  vec_of_na[i] <- sum(is.na(data[, i]))
}
vec_of_na

#using apply functions
count_na <- sapply(data, function(x) sum(is.na(x))) 
count_na

#using data.table
colSums(is.na(data_1))

#3.2
data$year_of_birth <- substr(data$date_of_birth, 0, 4) #created new column showing only the year of birth
vec_year_of_birth <- unique(data$year_of_birth) #vec with only unique years of birth
vec_of_meanxp <- c(lenght=length(vec_year_of_birth))

for (i in 1:length(unique(data$year_of_birth))){
  data_aux <- data[data$year_of_birth == vec_year_of_birth[i], ]
  vec_of_meanxp[i] <- mean(data_aux$exp_by_1996)
} 
vec_of_meanxp

#аналог
vec_pid <- unique(data_1$pers_id)
vec_means_id <- vector(length = length(vec_pid))
for (i in 1:length(vec_pid)){
  data_aux <- data_1[data_1$pers_id == vec_pid[i], ]
  vec_means_id[i] <- mean(data_aux$wage)
}

#Workings 3.2
length(data$d199601)

#здесь еще нужен while для того чтобы по годам сортировать. И использовать if для гендера
vec_means_exp <- vector(length = 100000)
for (i in 1:length(data$d199601)){
  #data_aux <- data[data$pers_id == vec_pid[i], ]
  vec_means_xp[i] <- mean(data$exp_by_1996)
}
vec_means_exp

