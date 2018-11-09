#Problem1.1
#Problem #1.1
R_squared <- vector(length=1000)
RSS <- vector(length=1000)

for(i in 1:1000){
  x <- runif(1000, min=0, max=6)
  e <- rnorm(1000, 0, x)
  y <- 1+2*x+e
  model <- lm(y~x)
  R_squared[i] <- summary(model)$r.squared
  RSS[i] <- sum(resid(model)^2)
}

hist(R_squared)
hist(RSS)

#1.2
f_hist<-function(x){
  x
}