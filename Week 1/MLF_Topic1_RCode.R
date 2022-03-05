rm(list=ls())
# generate data: one feature X and a quadratic target variable Y
set.seed(100)
sigma=0.1
X=2*runif(1000)-1
Y=2*X^2+sigma*rnorm(1000)
plot(X,Y,pch=20,col="blue",cex=0.6)

## Approximation 1: find the data that corresponds with our x value
x=0.5
sum(X==x)

## Apprpximation 2: K-nearest neighbor
mean(Y) # Average of all data points

k=50 # number of neighbors
DD=sqrt((X-x)^2) # distance
S=sort(DD, index.return=TRUE)
I=S$ix[1:k]
y_hat=mean(Y[I])
y_hat

# make a plot of the K-nearest observations
Z=c()
Z[I]=1 
Z[-I]=0
Z=as.factor(Z) # Z is 1 if it is one of the neighbors
plot(X,Y,pch=20,col=c("blue","red")[Z],cex=0.6)

# Determine the K-nearest neighbor estimator for different values of k. 
k_vector=seq(1,1000,1)
Estimate=c()
for(i in 1:length(k_vector)){
  k=k_vector[i]
  DD=sqrt((X-x)^2)
  S=sort(DD, index.return=TRUE)
  I=S$ix[1:k]
  Estimate[i]=mean(Y[I])
}
plot(k_vector,Estimate, ylim=c(0.48,0.7), type="l", col="blue")
lines(k_vector,rep(0.5,length(k_vector)), col="red")

### Numerical optimization with a squared loss function
SqLossFunction=function(yyhat,yy){
  L=mean((yy-yyhat)^2)
  return(L)
  }
k=50
DD=sqrt((X-x)^2)
S=sort(DD, index.return=TRUE)
I=S$ix[1:k]
optimize(SqLossFunction,yy=Y[I], interval = c(0,2))
y_hat

################################
### Median versus the average
################################
 ## We add an outlier to the data set
XX=c(X,0.501)   
YY=c(Y,2)     
plot(XX,YY,pch=20,col="blue",cex=0.8, xlim=c(-1,1), ylim=c(-0.3,3))
x=0.5
k=50
DD=sqrt((XX-x)^2)
S=sort(DD, index.return=TRUE)
I=S$ix[1:k]
y_hat_Sq=mean(YY[I])
y_hat_Sq

## The absolute value: we use numerical optimization to determine KNN 
## with an absolute value loss function.
MLossFunction=function(yyhat,yy){
  L=mean(abs(yy-yyhat))
  return(L)
}
k=50
DD=sqrt((XX-x)^2)
S=sort(DD, index.return=TRUE)
I=S$ix[1:k]
optimize(MLossFunction,yy=YY[I], interval = c(0,2))

y_hat_M=median(YY[I])
y_hat_M

plot(XX,YY,pch=20,col="blue",cex=0.4, xlim=c(-1,1), ylim=c(-0.3,2))
par(new=TRUE)
plot(x,y_hat_Sq, xlim=c(-1,1), ylim=c(-0.3,2), 
     col="red", cex=1, pch=20, xlab="", ylab="")
par(new=TRUE)
plot(x,y_hat_M, xlim=c(-1,1), ylim=c(-0.3,2), 
     col="black", cex=1, pch=20, xlab="", ylab="")

## Compare the squared loss and absolute value loss functions.
## Minimizing the absolute value is not behaving as nice as minimizing the 
## squared loss function. 
xvalues=seq(0.46,0.6,0.0001)
yvalues_sq=c()
yvalues_M=c()
for(ii in 1:length(xvalues)){
  yvalues_sq[ii]=SqLossFunction(xvalues[ii],YY[I])
  yvalues_M[ii]=MLossFunction(xvalues[ii],YY[I])
}
par(mfrow=c(1,2))
plot(xvalues,yvalues_sq,pch=20, cex=0.5, col="blue", 
     xlim=c(0.5,0.54), ylim=c(0.0556, 0.056), xlab="x", ylab="MSE", main="Squared error")
plot(xvalues,yvalues_M, pch=20, cex=0.5, col="blue", 
     xlim=c(0.46,0.5), ylim=c(0.118,0.120), xlab="x", ylab="MAE", 
     main="absolute value error")


###########################################
### assessing the quality of the model
###########################################
## Model fit using the data
## we determine the estimated target variable for each data point. 
x=X
y_hat=c()
k=50
for(i in 1:length(x)){
  DD=sqrt((X-x[i])^2)
  S=sort(DD, index.return=TRUE)
  I=S$ix[1:k]
  y_hat[i]=mean(Y[I])
}
plot(X,Y,pch=20,col="blue",cex=1, xlim=c(-1,1), ylim=c(-0.3,2), 
     main='Data and Fitted values', xlab='x', ylab="y")
par(new=TRUE)
plot(X,y_hat, xlim=c(-1,1), ylim=c(-0.3,2), col="red", 
     cex=1, pch=20, xlab="", ylab="")
legend(0.5,0.02,legend=c("Data","Fitted values"), 
       pch=c(20,20), col=c("blue","red"), cex=1.3)

MSE=mean((y_hat-Y)^2)
MSE*100

# residual plot
plot(X,y_hat-Y, col="blue", cex=1, pch=20, xlab="x", ylab="Residual", 
     main="Residual plot")
# Compare the  variance of the prediction error and the variance of the random source
var(y_hat-Y) # variance of the residuals 
sigma^2  # the theoretical variable of the residuals, based on the 'read' model.

# plot the MSE in function of k
## the effect of k
x=X
k_vector=seq(5,50,1)
MSE_vector=c()
for(kk in 1:length(k_vector)){
  y_hat=c()
  k=k_vector[kk]
  for(i in 1:length(x)){
    DD=sqrt((X-x[i])^2)
    S=sort(DD, index.return=TRUE)
    I=S$ix[1:k]
    y_hat[i]=mean(Y[I])
  }
  MSE_vector[kk]=mean((y_hat-Y)^2)*100
}
plot(k_vector,MSE_vector,cex=1.2, ylim=c(0.85,1.1), xlim=c(5,50),pch=20, 
     col="blue", xlab="k", ylab="MSE", main="MSE for different values of k")
par(new=TRUE)
plot(k_vector,rep(1,length(k_vector)),cex=1.2, ylim=c(0.85,1.1), 
     xlim=c(5,50),type="l", col="red",xlab="",ylab="")

###################################################################################


###################################################################################
## Training, Validation and Test set
set.seed(101)
Train=sample(1000,800)
Train_X=X[Train]
Train_Y=Y[Train]
y_hat=c()
x=Train_X
k=2
for(i in 1:length(Train)){
  DD=sqrt((Train_X-x[i])^2)
  S=sort(DD, index.return=TRUE)
  I=S$ix[1:k]
  y_hat[i]=mean(Train_Y[I])
}
MSE_Train=mean((y_hat-Train_Y)^2)
MSE_Train*100
plot(Train_X,Train_Y,pch=20,col="blue",cex=0.8, xlim=c(-1,1), ylim=c(-0.3,2))
par(new=TRUE)
plot(Train_X,y_hat,pch=20,col="red",cex=0.8, xlim=c(-1,1), ylim=c(-0.3,2))

#Validation/Test set
Test_X=X[-Train]
Test_Y=Y[-Train]
y_hat_test=c()
x=Test_X
for(i in 1:length(Test_X)){
  DD=sqrt((Train_X-x[i])^2)
  S=sort(DD, index.return=TRUE)
  I=S$ix[1:k]
  y_hat_test[i]=mean(Train_Y[I])
}
MSE_Test=mean((y_hat_test-Test_Y)^2)
MSE_Test*100

plot(Test_X,Test_Y,pch=20,col="blue",cex=1, xlim=c(-1,1), ylim=c(-0.3,2), 
     main="Predicted values on the Valiation")
par(new=TRUE)
plot(Test_X,y_hat_test,pch=20,col="red",cex=1, xlim=c(-1,1), 
     ylim=c(-0.3,2), xlab="", ylab="")

### Validation MSE in function of k
k_vector=seq(2,80,1)
Test_X=X[-Train]
Test_Y=Y[-Train]
x=Test_X
MSE_Test=c()
for(kk in 1:length(k_vector)){
y_hat_test=c()
k=k_vector[kk]
for(i in 1:length(Test_X)){
  DD=sqrt((Train_X-x[i])^2)
  S=sort(DD, index.return=TRUE)
  I=S$ix[1:k]
  y_hat_test[i]=mean(Train_Y[I])
}
MSE_Test[kk]=mean((y_hat_test-Test_Y)^2)*100
}
plot(k_vector,MSE_Test, pch=20, col="blue",cex=1.2,xlab="k", ylab="MSE", 
     main="The validation MSE")


##################################
# Bias and Variance of a model
##################################

### Variance of the point estimate
set.seed(102)
k1=5
k2=200
y_hat1=c()
y_hat2=c()
x=0.5

for(i in 1:1000){
  XX=X
  YY=2*X^2+sigma*rnorm(1000)
  D=sqrt((XX-x)^2)
  S=sort(D, index.return=TRUE)
  I1=S$ix[1:k1]
  I2=S$ix[1:k2]
  y_hat1[i]=mean(YY[I1])
  y_hat2[i]=mean(YY[I2])
}
par(mfrow=c(1,2))
hist(y_hat1,100, freq=FALSE, xlim=c(0.35,0.65), ylim=c(0,20),
     main='Histogram with k=5', xlab='Predicted value f(0.5)')
hist(y_hat2,100, freq=FALSE, xlim=c(0.35,0.65), ylim=c(0,60),
     main='Histogram with k=200',xlab='Predicted value f(0.5)')

#Mean
mean(y_hat1)
mean(y_hat2)
#Variance
var(y_hat1)*100
var(y_hat2)*100
#Bias
fx=2*x^2
fx-mean(y_hat1)
fx-mean(y_hat2)


## plot the standard deviation of the estimate in function of the k in KNN
k_vector=seq(1,500,10)
x=0.5
sigma=0.1
fx=2*x^2
y_hat_std=c()
Bias=c()

for(kk in 1:length(k_vector)){
  y_hat=c()
  k=k_vector[kk]
    for(i in 1:10000){
    #II=sample(1000,1000,replace=TRUE)
    #XX=X[II]
    #YY=Y[II]
    XX=X
    YY=2*X^2+sigma*rnorm(1000)
    DD=sqrt((XX-x)^2)
    S=sort(DD, index.return=TRUE)
    I=S$ix[1:k]
    y_hat[i]=mean(YY[I])
  }
  y_hat_std[kk]=sqrt(var(y_hat))
  Bias[kk]=fx-mean(y_hat)
}
par(mfrow=c(1,2))
plot(k_vector,y_hat_std, pch=20, col="blue", cex=2, xlab="k", ylab="Variance", 
     main="Variance")
plot(k_vector,(Bias)^2,  pch=20, col="blue", cex=2, xlab="k", ylab="Bias", 
     main="Bias")

## use the R function knn.reg from the FNN package
library("FNN") # if this does not work, do first install.packages(FNN)
knn.reg(X,c(0.5),Y,k=50)


## Use Linear model to approximate the conditional expectation
x=0.5
XX=X^2
LM=lm(Y~XX)
yhat_LM_x= LM$coefficients[1]+ LM$coefficients[2]*x^2

plot(X,predict(LM))


