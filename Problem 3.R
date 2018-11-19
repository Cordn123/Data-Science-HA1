#Problem 3
rm(list=ls())

#NOTE:
#Several loops take a bit long to run, thank you for your patience:

#Also, useful tips and tricks:
#For paralell computing try (speeds up the coputation):
#install.packages("doMC")
#library(doMC)
#registerDoMC(cores=n)

#3.1
library(data.table)
data <- fread('hw1p3.csv') #also possible to use read.csv('hw1p3.csv'), but fread is much faster
#just to understand the data
summary(data)
head(data)

#3.1
#using loops
vec_of_nac <- c(length=length(data)) 
for (i in 1:ncol(data)){
  vec_of_nac[i] <- sum(is.na(data[, ..i])) #"..i" structure is special for data.table to operate properly
}
vec_of_nac

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
data <- data[!is.na(data$exp_by_1996),] #reshaping the data, removing all NA

#Calculating By experience before 1996
for (i in 1:length(vec_year_of_birth)){
  data_aux <- data[data$year_of_birth == vec_year_of_birth[i], ]
  vec_of_meanxp[i] <- mean(data_aux$exp_by_1996)
} 
vec_of_meanxp

#Calculating By gender
vec_gender <- unique(data$gender) #vec with only unique gender, i.e only two values M, F
vec_genxp <- vector(length=length(vec_gender)) #creating vector to store required data later on

for (i in 1:length(vec_gender)){
  data_aux <- data[data$gender == vec_gender[i], ]
  vec_genxp[i] <- mean(data_aux$exp_by_1996)
} 
vec_genxp


#Using tapply() function to calculate grouped means by year of birth and gender:

tapply(data$exp_by_1996, data$year_of_birth, FUN = mean) #years of birth

tapply(data$exp_by_1996, data$gender, FUN = mean) #gender

#using data.table
#for year of birth
#Once again reshaping the data just in case
data$year_of_birth <- substr(data$date_of_birth, 0, 4)
data[,year_of_birth:=as.numeric(year_of_birth)]
data <- data[!is.na(year_of_birth)]


data_mean_wxp <- data[, .(mean_wxp=mean(exp_by_1996)), by = c('year_of_birth')]
data_mean_wxp

#for gender
data_mean_genxp <- data[, .(mean_wxp=mean(exp_by_1996)), by = c('gender')]
data_mean_genxp


#3.3
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
ggplot(data_mean_malexp, aes(year_of_birth, mean_wxp)) + 
  geom_line(aes(color="Male"))+
  geom_line(data=data_mean_femalexp, aes(color="Female"))+
  labs(color="Legend") +
  xlab('Year of Birth') +
  ylab('Mean work exp. in years until 1996')


#3.4 
#creating data containing just dummy variables for easy further calculations
data_aux345 <- data[, c('date_of_birth', 'gender', 'exp_by_1996', 'year_of_birth'):=NULL]

#This takes time :)
vec_of_nar <- c(length=length(data_aux345)) 
for (i in 1:nrow(data_aux345)){
  vec_of_nar[i] <- sum(is.na(data_aux345[i, ]))
}
vec_of_nar

#using apply functions
count_nar <- apply(data_aux345, 1, function(x) sum(is.na(x)))
count_nar

#using data.table
rowSums(is.na(data_aux345))

#3.5
rowSums(data_aux345, na.rm = FALSE) #na.rm set to FALSE includes all NAs into the calculations 

#3.6
data_36 <- data_aux345 #creating the brand new data specificslly for this problem
xp_id_time <- function(idd, n){
  m = n+1 #including column n
  data_36 = data_36[, (m:ncol(data_36)):= NULL] #droping all columns we are not interested in
  if (is.na(rowSums(data_36[idd], na.rm = FALSE)) == TRUE){ #in case we have NA, return NA
    return(NA)
  } else {
    return(rowSums(data_36[idd], na.rm = FALSE)) #in case we are good, summing up all rows, i.e for each person (idd)
  }
}
xp_id_time(1, 200) #testing

#3.7
total_xp_age <- function(idd, age) { #creating a function two variables 
  #more specifically we make a function of an individual for whom all dummies are taking into account
  #and calculating this by a certain age
  as.integer(age * 100 + data[idd,1]) %>% #multipliation by 100 allows to bring the data in line with the dates used in 'data'
    toString(x = .) %>% #then use our favorite piping to plug in data from one func to another
    paste0('d', .) %>% #pasting a d to match the format column dates in 'data'
    match(., names(data)) %>% 
    assign("n", . , pos=1)
  if (is.na(n) == FALSE) { #if we are all set just calculate row sums 
    return(rowSums(data[idd, 3:n])) #starting from 3 to exclude unnecessary columns 
  }
  else if (as.integer(age * 100 + data[idd,1]) < 201412) { #considering specific case 
    return(rowSums(data[input_row, 3:ncol(data)])) 
  }
  else {print('FIELD_NOT_FOUND')} #as specified in the problem text 
}
total_xp_age(idd = 768, age = 34)

#3.8
#Calculate for all individuals total work experience in months reached by 201112.
cumulative_xp_per_id <- vector()
data_38 <- data_aux345 #creating new data
data_38 <- data_38[, (1511:ncol(data_36)):= NULL] #once again, drop all columns we do not need, in our case, all columns after d201112
for (i in 1:nrow(data_aux345)){
  cumulative_xp_per_id[i] <- rowSums(data_aux345[i], na.rm = TRUE) #filling vector with each row sum 
}
sum(cumulative_xp_per_id) #summing up all the elements of the vector, so that we arrive to the final result

#3.9
vec_xp60 <- vector() 
for (i in 1:nrow(data)){
  vec_xp60[i] <- work_experience(i, 60) #just applying our function from 3.7 to each individual and prespecifying the required age 60
}
sum(vec_xp60, na.rm = TRUE) #summing all elements if the vector to get the final result
