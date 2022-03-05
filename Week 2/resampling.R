## Week 2

## Resampling Methods

## Question 4

set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y,pch=21)

# Data = data frame with the data set
Data <- data.frame(x,y)

SQLoss=function(theta,Data){
  # theta = coefficients of the polynomial model (as a matrix)
  #  process 'Data' dataframe
  y <- Data$y;
  p <- length(theta)-1; n <- length(y);
  theta <- matrix(theta, length(theta), 1)
  
  # create design matrix
  X.hat <- matrix(0, n, p+1)
  for (i in 0:p){
    X.hat[,i+1] <- matrix(Data$x^(i), n, 1)
  }
  
  MSE <- (1/n) * t((y - X.hat %*% theta)) %*%  (y - X.hat %*% theta)
  return(MSE)
}

## Implement LOOCV
## Model 1

MSE.vec <- rep(0, length(y))
for (i in 1:length(y)){
  train <- Data[-i,]
  fit.i <- optim(par=c(1,1), fn=SQLoss, Data=train)
  y.hat <- fit.i$par %*% c(1,Data$x[i])
  MSE.vec[i] <- (Data$y[i]-y.hat)^2
}

M1 <- mean(MSE.vec)

## Model 2

MSE.vec <- rep(0, length(y))
for (i in 1:length(y)){
  train <- Data[-i,]
  fit.i <- optim(par=c(1,1,1), fn=SQLoss, Data=train)
  y.hat <- fit.i$par %*% c(1,Data$x[i], (Data$x[i])^2)
  MSE.vec[i] <- (Data$y[i]-y.hat)^2
}

M2 <- mean(MSE.vec)

## Model 3

MSE.vec <- rep(0, length(y))
for (i in 1:length(y)){
  train <- Data[-i,]
  fit.i <- optim(par=rep(1,4), fn=SQLoss, Data=train)
  y.hat <- fit.i$par %*% c(1,Data$x[i], (Data$x[i])^2, (Data$x[i])^3)
  MSE.vec[i] <- (Data$y[i]-y.hat)^2
}

M3 <- mean(MSE.vec)

## Model 4

MSE.vec <- rep(0, length(y))
for (i in 1:length(y)){
  train <- Data[-i,]
  fit.i <- optim(par=rep(1,5), fn=SQLoss, Data=train)
  y.hat <- fit.i$par %*% c(1, Data$x[i], (Data$x[i])^2, 
                           (Data$x[i])^3, (Data$x[i])^4)
  MSE.vec[i] <- (Data$y[i]-y.hat)^2
}

M4 <- mean(MSE.vec)

## Bootstrap, standard error of coefficients
nboots <- 100
theta0 <- rep(0, nboots)
theta1 <- rep(0, nboots)
theta2 <- rep(0, nboots)

for (i in 1:nboots){
  I=sample(1:length(y), replace = TRUE)
  boot <- Data[I,]
  fit.i <- optim(par=rep(1,3), fn=SQLoss, Data=boot)
  theta0[i] <- fit.i$par[1]
  theta1[i] <- fit.i$par[2]
  theta2[i] <- fit.i$par[3]
}

sd(theta0)
sd(theta1)
sd(theta2)
