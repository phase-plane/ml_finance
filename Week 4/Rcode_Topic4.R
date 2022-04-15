## Correlated normal distributions
rho=0.7
X1=rnorm(1000,mean=0,sd=1)
X2=rho*X1+ sqrt(1-rho^2)*rnorm(1000,mean=0, sd=1)
par(mfrow=c(1,2))

plot(X1,X2, pch=20, col="blue")
abline(v=2, col="red", lwd=2)
abline(v=-2, col="red", lwd=2, lty=2)
abline(b=rho,a=0, col="green", lty=2, lwd=2)


rho=-0.7
X1=rnorm(1000,mean=0,sd=1)
X2=rho*X1+ sqrt(1-rho^2)*rnorm(1000,mean=0, sd=1)
plot(X1,X2, pch=20, col="blue")
abline(v=2, col="red", lwd=2)
abline(v=-2, col="red", lwd=2, lty=2)
abline(b=rho,a=0, col="green", lty=2, lwd=2)

  
######################################################  
# Assume we have no data set available to predict Y
######################################################
x1 <- 1
par(mfrow=c(1,1))
mu.Y1=0
var.Y1=1

  
Y1=rnorm(40,mean=mu.Y1, sd=sqrt(var.Y1))
plot(rep(x1, length(Y1)), Y1, pch=20, col="blue", 
     xlab="features", ylab="prediction")
abline(h=0, lwd=2, col="red")
  
######################################################  
# Assume we have the observation (x=0.8, y=1)
######################################################  
# Prediction the target for x=1
x1=0.8
y1=1
x=1
sigma.error=0.1
  
theta.1=1
theta.2=1
kernel=theta.1*exp(-1/(2*theta.2)*(x-x1)^2)

mu.c=kernel/(1+sigma.error^2)*y1
sigma.c=sqrt(1-kernel^2/(1+sigma.error^2))

Y.c=rnorm(20,mean=mu.c, sd=sigma.c)

plot(rep(x, length(Y.c)), Y.c, pch=20, col="blue", 
     xlab="features", ylab="prediction")
abline(h=mu.c, lwd=2, col="red")
points(x1,y1, pch=20, col="red")

######################################################  
# Prediction the target for different values of x, given (x1, y1)
######################################################  

x1=0.8
y1=1
SimPrediction = function(x, x1,y1){
  sigma.error=0.1
  theta.1=1
  theta.2=1
  kernel=theta.1*exp(-1/(2*theta.2)*(x-x1)^2)
  mu.c=kernel/(1+sigma.error^2)*y1
  sigma.c=sqrt(1-kernel^2/(1+sigma.error^2))
  
  return(rnorm(500,mean=mu.c, sd=sigma.c))
}

x=c(-1,0,0.5,0.6,0.7, 0.75,0.82,0.85,0.9,1,1.1, 1.2, 1.3,1.4,2,3)
Y.c=matrix(1,length(x),500)

Y.c[1,]=SimPrediction(x[1], x1,y1)
plot(rep(x[1], length(Y.c[1,])), Y.c[1,], pch=20, col="blue", 
     xlab="Feature", ylab="Prediction", ylim=c(-2,3), xlim=c(-1.2,3.2))
points(x1,y1, pch=20, col="red")
abline(h=0, col="black")
mu.c=c()
mu.c[1]=mean(Y.c[1,])
points(x[1], mu.c[1], pch=20, col="green", cex=1.4)

for(i in 2:length(x)){
Y.c[i,]=SimPrediction(x[i], x1,y1)
mu.c[i]=mean(Y.c[i,])
points(rep(x[i], length(Y.c[i,])), Y.c[i,], pch=20, col="blue", 
       xlab="features", ylab="prediction")
points(x[i], mu.c[i], pch=20, col="green", cex=1.4)
}

########################################################
# The squared exponential kernel function
######################################################
# effect of theta1
x1=0.8
x=seq(-3,5,0.1)

kernel11=exp(-1/(2)*(x-x1)^2)
kernel21=1.5*exp(-1/(2)*(x-x1)^2)
kernel31=2*exp(-1/(2)*(x-x1)^2)
kernel01=0.5*exp(-1/(2)*(x-x1)^2)

plot(x, kernel11, type="l", col="blue", lwd=2, xlim=c(-3,4), ylim=c(0,2))
lines(x, kernel21, type="l", col="red", lwd=2, xlim=c(-3,4), ylim=c(0,2))
lines(x, kernel31, type="l", col="green", lwd=2, xlim=c(-3,4), ylim=c(0,2))
lines(x, kernel01, type="l", col="purple", lwd=2, xlim=c(-3,4), ylim=c(0,2))
legend('topleft',c("theta.1=0.5","theta.1=1","theta.1=1.5", "theta.1=2"),
       col=c("purple","blue", "red", "green"), lty=1, lwd=2)

# The squared exponential kernel function
# effect of theta2
x1=0.8
x=seq(-3,5,0.1)

kernel11=exp(-1/(2*0.5)*(x-x1)^2)
kernel21=exp(-1/(2)*(x-x1)^2)
kernel31=exp(-1/(2*1.5)*(x-x1)^2)
kernel01=exp(-1/(2*2)*(x-x1)^2)

plot(x, kernel11, type="l", col="blue", lwd=2, xlim=c(-3,4), ylim=c(0,1))
lines(x, kernel21, type="l", col="red", lwd=2, xlim=c(-3,4), ylim=c(0,1))
lines(x, kernel31, type="l", col="green", lwd=2, xlim=c(-3,4), ylim=c(0,1))
lines(x, kernel01, type="l", col="purple", lwd=2, xlim=c(-3,4), ylim=c(0,1))
legend('topleft',c("theta.2=0.5","theta.2=1","theta.2=1.5", "theta.2=2"),
       col=c("purple","blue", "red", "green"), lty=1, lwd=2)


#############################################################
# multivariate Gaussian random variables
#############################################################

n=40 #dimension = number of predictions we need to make
mu=rep(0,n)
sigma=rep(1,n)
Z=rnorm(n, mean=mu, sd=sigma)

x=sort(runif(n,0,10)) # feature variables
theta.1=1
theta.2=1
A=matrix(1,n,1)%*%x
Distance=(A-t(A))^2
kernel=theta.1*exp(-(1/(2*theta.2))*Distance)
diag(kernel)=1+0.000001
L=chol(kernel)  #cholesky. L is upper triangle such that L^TL=kernel

Sim=4
Z=matrix(rnorm(n*Sim, mean=mu, sd=sigma),ncol=n, byrow=TRUE)
Y=Z%*%L


plot(x,Y[1,], col=1, type="l", ylim=c(min(Y), max(Y)), lwd=2, 
     ylab="Realizations")
points(x,Y[1,], pch=20, col=1)
for(i in 2:Sim){
  lines(x,Y[i,], col=i, type="l", lwd=2)
  points(x,Y[i,], pch=20, col=i)
}

##########################################################
# Gauss process regression
########################################################
# multiple observations: create the data set
set.seed(100)
n.obs=10 # number of observations
x.data=sort(runif(n.obs,0,10))
sigma.error=0.1
y.data= 5*sin(x.data)+rnorm(n.obs)*0.1
y.data=(y.data-mean(y.data))/sd(y.data)
plot(x.data,y.data, pch=21, col="black", lwd=1.5)
  

# predict new values
library(Matrix) #We use the nearPD function

x.new=seq(0,10,0.05)
xx=c(x.data,x.new)
nn=length(xx)
theta.1=1
theta.2=1
A=matrix(1,nn,1)%*%xx
Distance=(A-t(A))^2
KK=theta.1*exp(-(1/(2*theta.2))*Distance)
diag(KK)=diag(KK)
KK=nearPD(KK)$mat

K.data=KK[1:n.obs,1:n.obs]
K.star=KK[1:n.obs, (n.obs+1):nn]
K.new=KK[(n.obs+1):nn,(n.obs+1):nn]

mean.c=t(K.star)%*%solve(K.data+sigma.error^2*diag(n.obs))%*%(y.data)
var.c=pmax(K.new-t(K.star)%*%solve(K.data+sigma.error^2*diag(n.obs))%*%K.star,0)

Sim=10
Z=matrix(rnorm(length(x.new)*Sim,0,1),ncol=length(x.new),byrow=TRUE)
y.prediction <- matrix(mean.c, ncol=length(x.new), 
                       nrow=Sim, byrow=TRUE) + Z%*%chol(nearPD(var.c)$mat)

#points(x.new,y.prediction, col="red")
plot(x.data,y.data, pch=21, col="black", lwd=1.5,
     ylim=c(min(y.prediction), max(y.prediction)), 
     xlab="x vlalues", ylab="y values")
for(i in 1:Sim){
lines(x.new,y.prediction[i,], col=i, lwd=1)
}
points(x.data,y.data, pch=20, col="black", lwd=4, 
       ylim=c(min(y.prediction), max(y.prediction)))
lines(x.new, mean.c, lwd=2, lty=2, col="blue")

# Add confidence bounds based on 2 standard deviations

U=as.vector(mean.c)+2*sqrt(diag(var.c))
L=as.vector(mean.c)-2*sqrt(diag(var.c))
plot(x.new, U, lty=2, col="grey", lwd=2, type="l", ylim=c(min(L), max(U)))
polygon(c(x.new,rev(x.new)),c(U,rev(L)),col="grey")
lines(x.new, mean.c, lwd=2, lty=2, col="blue")
lines(x.new, L, lty=2, col="grey", lwd=2)
lines(x.new, U, lty=2, col="grey", lwd=2)

## Compare results with the GauPro package
library(GauPro)

GPR=GauPro(x.data, y.data)
y.new.GPR=GPR$pred(x.new)

plot(x.data,y.data, pch=21, col="black", lwd=1.5)
lines(x.new, mean.c, lwd=2, lty=2, col="blue")
lines(x.new, y.new.GPR)

# results with the mlegp package
library(mlegp)
GPR.mle=mlegp(x.data, y.data)
yhat.mle=predict(GPR.mle, newData=matrix(x.new, length(x.new),1))
lines(x.new, yhat.mle, col="red")

############################################
## GPR and KNN ####
############################################
rm(list=ls())
# generate data: one feature X and a quadratic target variable Y
set.seed(200)
sigma=0.1
X=sort(2*runif(100)-1)
Y=2*X^2+sigma*rnorm(100)
plot(X,Y,pch=20,col="blue",cex=0.6)

GPR=GauPro(X, Y, nugget=0.01)

GPR.fit=GPR$pred(X, se=T)
yhat=GPR.fit$mean
SE=GPR.fit$se

plot(X,Y)
points(X,yhat, pch=20, col="blue")
lines(X,yhat+2*SE, type="l", lty=2, col="red")
lines(X,yhat-2*SE, type="l", lty=2, col="red")

GPR=GauPro(X, Y)
y.hat=GPR$pred(X)
lines(X, y.hat, col="green")

############################################
## GPR and linear regression
############################################
# generate the data
x.data=runif(100, 0, 20)
y.data=1+2*x.data + 2*rnorm(100) + 0.1*x^2
plot(x.data, y.data)

###################################################
# Kernel ridge regression
###################################################
# multiple observations
set.seed(100)
n.obs=100 # number of observations
x.data=sort(runif(n.obs,0,10))
sigma.error=1
y.data= 5*sin(x.data)+rnorm(n.obs)*sigma.error
y.data=(y.data-mean(y.data))/sd(y.data)
plot(x.data,y.data, pch=21, col="black", lwd=1.5)

# determine the kernel matrix
x.new=seq(0,10,0.05)
xx=c(x.data,x.new)
nn=length(xx)
theta.1=1
theta.2=1
A=matrix(1,nn,1)%*%xx
Distance=(A-t(A))^2
KK=theta.1*exp(-(1/(2*theta.2))*Distance)

K.data=KK[1:n.obs,1:n.obs]
K.star=KK[1:n.obs, (n.obs+1):nn]

# prediction
lambda=1E-10
A=solve(K.data+lambda*diag(n.obs))
y.hat=t(K.star)%*%A%*%y.data

plot(x.data,y.data, pch=21, col="black", lwd=1.5)
points(x.new, y.hat, pch=20, col="blue")