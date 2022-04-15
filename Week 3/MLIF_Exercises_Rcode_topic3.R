############################
## Question 4
###########################
set.seed(101)
# the feature variables
X1=rnorm(200)
X2=rnorm(200)
X3=rnorm(200)+X1+X2
X4=X1+2*X3
# the target variable 
Y=X1+X2+X3*0.6 + X4 + 0.5*rnorm(200)
# define the feature matrix
X=matrix(c(rep(1,200),X1,X2,X3, X4),200, 5, byrow=FALSE)
# the least squares regression coefficients
beta=solve(t(X)%*%X)%*%t(X)%*%matrix(Y,200,1)

lambda=0.01
beta=solve(t(X)%*%X + diag(lambda,5))%*%t(X)%*%matrix(Y,200,1)
beta

############################
## Question 5
###########################3
library(MASS)

# simulation of the stock prices at maturity
Mat=1
r=0.01 # risk free rate
mu=c(0.05, 0.06, 0.07, 0.02, 0.03)
sigma=c(0.1, 0.2, 0.3, 0.1, 0.2)
n=length(sigma)
rr=0.5 #correlation between the logreturns
rho=matrix(rep(rr,25),n,n)
diag(rho)=1
NSim=10000
E=mvrnorm(NSim,rep(0, n),rho)

# simulate the time T stock prices
S=matrix(rep(1,NSim*n), NSim, n)
for(i in 1:n){
  S[,i]=exp((mu[i]-0.5*sigma[i]^2)*Mat+sqrt(Mat)*sigma[i]*E[,i])
}

## payoff of the derivative
K=5 # strike price
Index=rowSums(S)
Y=pmax(Index-K,0) # payoff

## Regression
Model=lm(Y~S)
weights=as.vector(Model$coefficients)
summary(Model)
# check that the mean of the hedging strategy is the same as the mean of the derivative
sum(weights[2:(n+1)]*exp(mu*Mat))+weights[1]
mean(Y)
mean(predict(Model))

## plot residuals
Residuals=Model$residuals
hist(Residuals,100)
quantile(Residuals,0.75)

sum(Residuals>0)/length(Residuals) # probability of a hedging loss

plot(Index,Y, col="blue", pch=20)
points(Index, Model$fitted.values, col="red", pch=20)


## assume options are traded on the index
Y=2*Index^2
Strikes=seq(1,6,1)
Options=matrix(rep(1,NSim*length(Strikes)), NSim, length(Strikes))
for(i in 1:length(Strikes)){
  Options[,i]=pmax(Index-Strikes[i],0)
}
Model_Options=lm(Y~Options)
summary(Model_Options)

plot(Index,Y, col="blue", pch=20)
points(Index, Model_Options$fitted.values, col="red")

