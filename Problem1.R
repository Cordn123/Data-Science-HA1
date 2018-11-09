#Problem 1.1
R_squared <- vector(length=1000) #creating vector to store R_2 of predefined length
RSS <- vector(length=1000) #creating vector to store RSS of predefined length

for(i in 1:1000){ #running loop
  x <- runif(1000, min=0, max=6) #generating uniform x
  e <- rnorm(1000, 0, x) #generating normal residuals with mean=0 and sd=x
  y <- 1+2*x+e #generating y
  model <- lm(y~x) #regressing
  R_squared[i] <- summary(model)$r.squared #plugging value of R_2 to vector of R_2 for particular loop 
  RSS[i] <- sum(resid(model)^2) 
}

hist(R_squared) #plotting hists
hist(RSS)

#Problem 1.2
f_hist<-function(n, N, e_sd,...){ #creating function with described parameters, "..." stands for histogram parameters
  R_squared <- vector(length=N) #dublicating from 1.1 and expressing parameters in general form
  RSS <- vector(length=N)
  
  for(i in 1:N){
    x <- runif(n, min=0, max=6)
    e <- rnorm(n, 0, e_sd)
    y <- 1+2*x+e
    model <- lm(y~x)
    R_squared[i] <- summary(model)$r.squared
    RSS[i] <- sum(resid(model)^2)
  }
    
  hist(R_squared,...) #also allowing for additional parameters
  hist(RSS,...)

}
f_hist(1000,1000,x, xlab="a") #trying the function out
