#Problem 3
#3.1
library(data.table)
data <- fread('hw1p3.csv') #also possible to use read.csv('hw1p3.csv'), but fread is much faster
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
colSums(is.na(data))

#3.2
#Using loops
#Preparing data
data$year_of_birth <- substr(data$date_of_birth, 0, 4) #created new column showing only the year of birth
vec_year_of_birth <- unique(data$year_of_birth) #vec with only unique years of birth
vec_of_meanxp <- vector(length=length(vec_year_of_birth)) #creating vector to store required data later on

data$exp_by_1996 <- as.numeric(data$exp_by_1996) #convert all elements to numbers, strings to NA 
data <- data[!is.na(data$exp_by_1996),] #reshaping the data

#By experience before 1996
for (i in 1:length(vec_year_of_birth)){
  data_aux <- data[data$year_of_birth == vec_year_of_birth[i], ]
  vec_of_meanxp[i] <- mean(data_aux$exp_by_1996)
} 
vec_of_meanxp

#By gender
vec_gender <- unique(data$gender) #vec with only unique gender, i.e only two values M, F
vec_genxp <- vector(length=length(vec_gender)) #creating vector to store required data later on

for (i in 1:length(vec_gender)){
  data_aux <- data[data$gender == vec_gender[i], ]
  vec_genxp[i] <- mean(data_aux$exp_by_1996)
} 
vec_genxp

#---------- ENTERING PROBLEM AREA
#using apply functions
#хз как сделать апплаем
#for year of birth
sapply(data, function(columns) mean(data$exp_by_1996, na.rm = TRUE)) #na.rm indicates whether NAs should be dropped

#for gender
sapply()

#-------- LEAVING PROBLEM AREA

#using data.table
#for year of birth
data$year_of_birth <- substr(data$date_of_birth, 0, 4)
data[,year_of_birth:=as.numeric(year_of_birth)]
data <- data[!is.na(year_of_birth)]

data_mean_wxp <- data[, .(mean_wxp=mean(exp_by_1996)), by = c('year_of_birth')]
data_mean_wxp

#for gender
data_mean_genxp <- data[, .(mean_wxp=mean(exp_by_1996)), by = c('gender')]
data_mean_genxp


#3.3
#This will work as soon as data.table will be fixed in the ex. above
library(ggplot2)

#also could have used filter() in dplyr
data_male <- data[ which(data$gender =='M')] #separating men observations 
data_male
#calculating mean specifically for men in respect to year of birth
data_mean_malexp <- data_male[, .(mean_wxp=mean(exp_by_1996)), by = c('year_of_birth')] 
data_mean_malexp

data_female <- data[ which(data$gender =='F')] #separating women observations
data_female
#calculating mean specifically for men in respect to year of birth
data_mean_femalexp <- data_female[,mean_wxp:=mean(exp_by_1996), by = c('year_of_birth')]
data_mean_femalexp

#plotting this mean for both men and women separately as a function of year of birth
#Example just for men 

#ggplot(data_mean_malexp,
#       aes(x = year_of_birth, y = mean_wxp)) + geom_line(colour='orange')

#This works, can't change colors though
ggplot(data_mean_malexp, aes(year_of_birth, mean_wxp)) + 
  geom_line(aes(color="Male"))+
  geom_line(data=data_mean_femalexp, aes(color="Female"))+
  labs(color="Legend") +
  xlab('Year of Birth') +
  ylab('Mean work exp. in years until 1996')





