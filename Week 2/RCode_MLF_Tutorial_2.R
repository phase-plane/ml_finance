
####################################################################################
# question 3: Probability that a given observation is not in the bootstrap sample
####################################################################################
n=5000  # the number of observations in the original data set
j= 5 # we determine the probability that observation j is not in the data set
N.boot= 10000 # number of bootstrap samples we will generate
In=c()
for(i in 1:N.boot){
  I= sample(1:n, replace = TRUE)
  In[i]=sum(I==j)>0
}

1-mean(In)

####################################################################################
# question 4: Implementing LOOCV
####################################################################################

# create the data set
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y, pch=20, col="blue")

Data=data.frame(x=x, y=y)

SQLoss=function(theta,Data){
  # theta = coefficients (as a matrix)
  # Data = data frame with the data set
  
  p = length(theta)-1   # p = order to the polynomial model 
  n=length(Data$x)      # (-1 because we have an intercept)
  
  theta  = matrix(theta, 1, length(theta))        
  X.hat=matrix(1,n,(p+1))
  for( i in 0:p){
    X.hat[,(i+1)]=Data$x^i
  }
  
  Y.hat=X.hat%*%t(theta)
  MSE = mean((Y.hat - Data$y)^2)
  return(MSE)
}

#LOOCV for Model 1 (linear model)
MSE.M1.i=c()
for(i in 1:length(Data$x)){
  D.i=data.frame(x=Data$x[-i],y=Data$y[-i])  
  Fit.i=optim(par=c(1,1), fn=SQLoss, Data=D.i)
  y.hat = sum(Fit.i$par*c(1, Data$x[i]))
  MSE.M1.i[i] = (y.hat - Data$y[i])^2
}
MSE.M1 = mean(MSE.M1.i)


#LOOCV for Model 2 (quadratic model)
MSE.M2.i=c()
for(i in 1:length(Data$x)){
  D.i=data.frame(x=Data$x[-i],y=Data$y[-i])  
  Fit.i=optim(par=c(1,1,1), fn=SQLoss, Data=D.i)
  y.hat = sum(Fit.i$par*c(1, Data$x[i],  Data$x[i]^2))
  MSE.M2.i[i] = (y.hat - Data$y[i])^2
}
MSE.M2 = mean(MSE.M2.i)

#LOOCV for Model 3 (cubic model)
MSE.M3.i=c()
for(i in 1:length(Data$x)){
  D.i=data.frame(x=Data$x[-i],y=Data$y[-i])  
  Fit.i=optim(par=c(1,1,1,1), fn=SQLoss, Data=D.i)
  y.hat =sum(Fit.i$par*c(1, Data$x[i],  Data$x[i]^2,  Data$x[i]^3))
  MSE.M3.i[i] = (y.hat - Data$y[i])^2
}
MSE.M3 = mean(MSE.M3.i)

#LOOCV for Model 4 
MSE.M4.i=c()
for(i in 1:length(Data$x)){
  D.i=data.frame(x=Data$x[-i],y=Data$y[-i])  
  Fit.i=optim(par=c(1,1,1,1, 1), fn=SQLoss, Data=D.i)
  y.hat = sum(Fit.i$par*c(1, Data$x[i],  Data$x[i]^2,  Data$x[i]^3,  Data$x[i]^4))
  MSE.M4.i[i] = (y.hat - Data$y[i])^2
}
MSE.M4 = mean(MSE.M4.i)

c(MSE.M1, MSE.M2, MSE.M3,MSE.M4)

# The standard error of the coefficients of model M2 (we use all the data now)

N.boot=1000
Coef.0=c()
Coef.1=c()
Coef.2=c()
for(i in 1:N.boot){
  I=sample(1:100, replace=TRUE)
  D=data.frame(x=Data$x[I],y=Data$y[I]) 
  Fit=optim(par=c(1,1,1), fn=SQLoss, Data=D)
  Coef.0[i]=Fit$par[1]
  Coef.1[i]=Fit$par[2]
  Coef.2[i]=Fit$par[3]
}
sd(Coef.0)
sd(Coef.1)
sd(Coef.2)



