rm(list=ls())
library(glmnet)
library(lmvar)

set.seed(201)
k=40
n.obs=50
X=matrix(0,n.obs,k)
Z=rnorm(n.obs)
rho=0.9
for(i in 1:k){
X[,i]=sqrt(1-rho^2)*rnorm(n.obs)+rho*Z
}

Y=1+2*X[,1]  +rnorm(n.obs)*0.8

plot(X[,1],Y, pch=20, col="blue", cex=1.5)

# Fit a linear model
LS.Model=lm(Y~X, y=TRUE, x=TRUE)
summary(LS.Model)
sqrt(sum(LS.Model$coefficients[-1]^2)) #L2 norm of the regression coefficients

plot(predict(LS.Model), Y, pch=20, col="blue", cex=1.5) #nearly perfect fit
abline(a=0, b=1)

plot(X[,1],Y, cex=1.5) # plot the observations and the model predictions in function of a feature
points(X[,1],predict(LS.Model), pch=20, col="blue", cex=1.5)

sqrt(mean((predict(LS.Model)-Y)^2)) #Training Error 

# Test the linear model using a validation error
cv.lm(LS.Model, k=5)

# create a new test set
n.obs.test=50
X.test=X
Z=rnorm(n.obs)
rho=0.9
for(i in 1:k){
  X.test[,i]=sqrt(1-rho^2)*rnorm(n.obs)+rho*Z
}

Y.test=1+2*X.test[,1] +0.8*rnorm(n.obs.test)  

Y.hat.test=X.test%*%matrix(LS.Model$coefficients[-1],k,1) + LS.Model$coefficients[1]

plot(X.test[,20],Y.test) # plot the observations and the model predictions in function of a feature
points(X.test[,20],Y.hat.test, pch=20, col="blue")

plot(Y.hat.test, Y.test) # compare the predictions and the observations

sqrt(mean((Y.hat.test-Y.test)^2)) #Test Error 

# Fit a ridge regression
Ridge.Model=glmnet(X,Y, alpha=0, lambda=seq(0,100,0.1))

LL=100 #pick the lambda you want to check
Ridge.Model$lambda[LL]
coef(Ridge.Model)[,LL]
LS.Model
sqrt(sum(coef(Ridge.Model)[-1,LL]^2))

# Plot the coefficients in function of lambda
beta.1=coef(Ridge.Model)[2,]
plot(Ridge.Model$lambda, beta.1, ylim=c(-0.2,1),xlim=c(0,15), type="l", lwd=2, col=1, xlab="lambda", ylab="Regression coefficients")
for(i in 2:k){
  lines(Ridge.Model$lambda,coef(Ridge.Model)[(i+1),], col=i, type="l" , lwd=2)}
legend("topright",c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10"), lty=1, lwd=2, col=seq(1,10,1))

# The norm (L2) of the regression coefficients
plot(Ridge.Model$lambda,sqrt(colSums(coef(Ridge.Model)^2)), type="l", lwd=2, col="blue", xlim=c(0,100))
abline(h=0, col="red")

# do cross validation 
cv.out=cv.glmnet(X,Y,alpha=0)
plot(cv.out)
cv.out$lambda.min

# fit the best model
Ridge.Model=glmnet(X,Y, alpha=0, lambda=cv.out$lambda.min)
y.hat.test.ridge=predict(Ridge.Model, newx=X.test)

plot(y.hat.test.ridge,Y,pch=20)

plot(X[,1],Y, cex=1.5)
points(X[,1], y.hat.test.ridge,pch=20, col="blue", cex=1.5)
abline(a=1,b=2)
points(X.test[,1],Y.hat.test, pch=20, col="green", cex=1.5)

sqrt(mean((y.hat.test.ridge-Y.test)^2))


# Do Lasso regression. First, do a cross validation

cv.out=cv.glmnet(X,Y,alpha=1)
plot(cv.out)
cv.out$lambda.min

# fit the best model
Lasso.Model=glmnet(X,Y, alpha=1, lambda=cv.out$lambda.min)
y.hat.test.lasso=predict(Lasso.Model, newx=X.test)


plot(X[,1],Y, cex=1.5)
points(X[,1], y.hat.test.ridge,pch=20, col="blue", cex=1.5)
abline(a=1,b=2)
points(X.test[,1],y.hat.test.lasso, pch=20, col="green", cex=1.5)


sqrt(mean((y.hat.test.ridge-Y.test)^2))
sqrt(mean((y.hat.test.lasso-Y.test)^2))


# plot the coefficients in function of the lambda in Lasso regression
Lasso.Model=glmnet(X,Y, alpha=1, lambda=seq(0,1,0.0001))

# Plot the coefficients in function of lambda
beta.1=coef(Lasso.Model)[2,]
plot(Lasso.Model$lambda, beta.1, ylim=c(-0.2,1.4),xlim=c(0,0.25), type="l", lwd=2, col=1, xlab="lambda", ylab="Regression coefficients")
for(i in 2:9){
  lines(Lasso.Model$lambda,coef(Lasso.Model)[(i+1),], col=i, type="l" , lwd=2)}



################################################################################################
### Bias-variance of the ridge regression
###################################################################################################

# training set
n.obs=200
k=10
alpha=rnorm(k)
X=matrix(runif(k*n.obs)*6, n.obs,k)


lambda=seq(0,50,1)

E=c() #expected prediction
V=c()
Validation=c()
X.test=matrix(4, 1,k)
Y.test=sum(X.test*alpha)+rnorm(1)
for(i in 1:length(lambda)){
  y.hat.test=c()
  Validation.i=c()
  for(j in 1:1000){
    Y=X%*%matrix(alpha,k,1)+rnorm(n.obs)
    Ridge=glmnet(X,Y, alpha=0, lambda=lambda[i])
    y.hat.test[j]=predict(Ridge, newx=X.test)
    Validation.j[j]=(y.hat.test[j]-Y.test)^2
  }
  V[i]=sd(y.hat.test)
  E[i]=(mean(y.hat.test)-sum(X.test*alpha))^2
  Validation[i]=mean(Validation.j)
}

par(mfrow=c(1,2))
plot(lambda, E)
plot(lambda,V)

plot(lambda, E,  type="l", lwd=2, col="green")
par(new = TRUE)
plot(lambda,V, type="l", axes=FALSE, xlab="", ylab="", col="blue", lwd=2)
par(new = TRUE)
plot(lambda, Validation, lwd=2, col="red", type="l")
legend("topleft", c("Bias Squared", "Variance", "Validation error"), col=c("green", "blue", "red"), lwd=2, lty=1, cex=1.5)

