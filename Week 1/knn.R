# Primer using KNN
setwd("~/Desktop/2021/Code/ml_finance/Week 1")
rm(list = ls())

## Exercise 1 
## Question 4

## create dataset
set.seed(100) 
x1=rnorm(1000,0,1)*0.4
x2=0.5*x1 + 0.5*rnorm(1000,0,1)
x3=runif(1000,0,1)
sigma=0.2
Y=0.5*x1^2 +1*x2  - x3 +sigma*rnorm(1000,0,1)
X <- as.matrix(cbind(x1,x2,x3))

## plot
Y3 <- rep(Y, times=3)
par(mfrow=c(2,2))
plot(x1,Y)
plot(x2,Y)
plot(x3,Y)
plot(c(x1,x2,x3), Y3)

## knn to minimise square loss

KNN <- function(X,Y,k,X.hat){
  # X is the feature matrix. Each row contains one observation
  # Y are the target variables
  # X.hat matrix with observations for which we want to determine the yhat value
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  n.hat=dim(X.hat)[1]
  y.hat <- c()
  for (i in 1:n.hat){
    x <- X.hat[i,]
    DD <- sqrt(rowSums((X-matrix(1,n,1)%*%x)^2)) # distance
    S <- sort(DD, index.return=TRUE)
    I <- S$ix[1:k]
    y.hat[i] <- mean(Y[I])
  }
  return(y.hat)
}


Y.hat=KNN(X,Y,20,X) #the X.hat matrix is equal to X, since we determine a training MSE

## investigate the fit
par(mfrow=c(2,2))
plot(x1,Y)
points(x1,Y.hat,pch=20, col="blue")

plot(x2,Y)
points(x2,Y.hat,pch=20, col="blue")

plot(x3,Y)
points(x3,Y.hat,pch=20, col="blue")

plot(Y,Y.hat,pch=20, col="blue")

## training MSE
MSE= mean((Y-Y.hat)^2)
MSE

## MSE for varying K
k.vector <- seq(1,100,5)
MSE.vec <- c()
for (j in 1:length(k.vector)){
  Y.pred <- KNN(X,Y,k.vector[j],X)
  MSE.vec[j] <- mean((Y-Y.pred)^2)
}

par(mfrow=c(1,1))
plot(k.vector, MSE.vec, pch=20, col="royalblue")

## simulate validation set
set.seed(101) 
x1=rnorm(1000,0,1)*0.4
x2=0.5*x1 + 0.5*rnorm(1000,0,1)
x3=runif(1000,0,1)
Y.test <- 0.5*x1^2 +1*x2  - x3 +sigma*rnorm(1000,0,1)
X.test <- as.matrix(cbind(x1,x2,x3))

## test MSE for varying K
k.vector <- seq(1,60,5)
MSE.test<- c()
for (j in 1:length(k.vector)){
  Y.pred <- KNN(X,Y,k.vector[j],X.test)
  MSE.test[j] <- mean((Y.test-Y.pred)^2)
}

idx <- which.min(MSE.test)
solution <- k.vector[idx]

plot(k.vector, MSE.test, pch=20, col="forestgreen")
plot(X.test,Y.test)

### Variance of the point estimate

## new data
set.seed(17)
sigma <- 0.2
x=as.matrix(cbind(0,0.1,0.5))
nSim <- 1000
y_hat200=c()

for (i in 1:nSim){
  x1 <- rnorm(1000,0,1)*0.4
  x2 <- 0.5*x1 + 0.5*rnorm(1000,0,1)
  x3 <- runif(1000,0,1)
  X <- as.matrix(cbind(x1,x2,x3))
  Y <- 0.5*x1^2 +1*x2  - x3 +sigma*rnorm(1000,0,1)
  y_hat200[i] <- KNN(X,Y,200,x)
}

TrueValue=0.5*x[1]^2 +1*x[2]  - x[3]
Bias.hat=TrueValue-mean(y_hat200)
SE.hat=sd(y_hat200)
hist(y_hat200,100)
abline(v=TrueValue, col="red", lwd=2)

SE.hat
Bias.hat

## Bivariate Normal Simulation
n.obs <- 1000
mu.x <- 0; mu.y<-0; sigma.x<-1; sigma.y<-1;
X <- rnorm(n.obs, mu.x, sigma.x)
V <- rnorm(n.obs)
Y <- 0.5*X + (1-(0.5)^2)*V
plot(X,Y,pch=21,xlim=c(-4,4))

x <- seq(-4,4,0.1)
ycond <- mu.y + 0.5*(sigma.y/sigma.x)*(x-mu.x)
lines(x,ycond,col="forestgreen",lwd=2)

X.mat <- as.matrix(X)
X.hat <- as.matrix(x)

y.hat <- KNN(X.mat,Y,80,X.hat)
points(x,y.hat, col="red")
