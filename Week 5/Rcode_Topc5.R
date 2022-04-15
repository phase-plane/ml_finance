#################################
### Sales and Population Data
#################################
rm(list=ls())
library(tree)

# We create the data set
set.seed(20)
n=100
Pop=runif(n,280,40000)
Sales=c()
Colors=c()
Means=c()
for(i in 1:n){
  if(Pop[i]<11598){
    if(Pop[i]<4405){  Sales[i]=1111+rnorm(1,0,1000)
    Colors[i]=1
    Means[i]=1111
    }   else{    Sales[i]=5605+rnorm(1,0,1000)
    Colors[i]=2
    Means[i]=5605
    }
  }
  if(Pop[i]>11598){
    if(Pop[i]<29433)
    {
      Sales[i]=13040+rnorm(1,0,1000)
      Colors[i]=3
      Means[i]=13040
    }
    else
    {
      Sales[i]=23590+rnorm(1,0,1000)
      Colors[i]=4
      Means[i]=23590
    }
  }
}

## Determine the mean values in each group
Means=c()
for(i in 1:4){
  Means[i]=mean(Sales[Colors==i])
}

# Fit a linear model
plot(Pop,Sales, main="Lottery sales data", xlab="Population",
     ylab="", pch=Colors+19, col=Colors, lwd=2, cex=1.5)
abline(a=lm(Sales~Pop)$coefficients[1], 
       b=lm(Sales~Pop)$coefficients[2], col="red", lwd=2)


summary(lm(Sales~Pop))
plot(Pop,lm(Sales~Pop)$residuals, 
     main="Residuals of a Linear Regression", xlab="Population", 
     ylab="", pch=20, col="blue", cex=1.5)


# Fit a tree model
tree.Sales=tree(Sales~Pop)
summary(tree.Sales)
plot(tree.Sales)
text(tree.Sales,pretty=0, cex=2)

ResidualsTree=Sales- predict(tree.Sales)
plot(Pop, ResidualsTree,pch=20, xlab="Population", ylab="", 
     main="Residuals of decision tree", lwd=2.5, col="blue")
 
## Goodness of split
c.vector=seq(1, 40000, 1)
SS=c()
for(i in 1:length(c.vector)){
  c=c.vector[i]
  I=Pop<c
  R0=Sales[Pop<c]
  R1=Sales[Pop>=c]
  lambda0=mean(R0)
  lambda1=mean(R1)
  SS[i]= sqrt(sum((R0-lambda0)^2)+sum((R1-lambda1)^2))
}
plot(c.vector, SS, type="l", col="blue", lwd=2, xlab="c", 
     ylab="Root mean Sum of Squares", cex.lab=1.5)

# find out which values for c give the minimum RMSE
I=which(SS==min(SS), arr.ind=TRUE)
c.vector[I]


# RMSE before and after the optimal split
Before = sqrt(sum((Sales - mean(Sales))^2))
R0=Sales[Pop<11361]
R1=Sales[Pop>=11361]
lambda0=mean(R0)
lambda1=mean(R1)
After= sqrt(sum((R0-lambda0)^2)+sum((R1-lambda1)^2))

Before
After


## Build a tree 
  SumSquares=function(c,X,Y){
    I=which(X<=c)
    lambda.0=mean(Y[I])
    lambda.1=mean(Y[-I])
    SS.1=sum((Y[I]-lambda.0)^2)
    SS.2=sum((Y[-I]-lambda.1)^2)
    SS=SS.1+SS.2
    return(SS)
  }

# find the first split
R=optimize(SumSquares,c(min(Pop), max(Pop)), X=Pop, Y=Sales)
R0=R$minimum
R0

I=which(Pop<=R0)
Pop.10=Pop[I]
Sales.10=Sales[I]
D.10=sum((Sales.10-mean(Sales.10))^2)
Pop.11=Pop[-I]
Sales.11=Sales[-I]
D.11=sum((Sales.11-mean(Sales.11))^2)

# find the second split
R1=optimize(SumSquares,c(min(Pop.10), max(Pop.10)), X=Pop.10, Y=Sales.10)
R2=optimize(SumSquares,c(min(Pop.11), max(Pop.11)), X=Pop.11, Y=Sales.11)

R10=R1$minimum
R11=R2$minimum

c(R0, R10, R11)

c=seq(500,40000,10)
y=c()
for(i in 1:length(c)){
  y[i]=SumSquares(c[i],Pop,Sales)
}
plot(c,y)


## grow a tree with a different number of leaves
T0=tree(Sales ~ Pop)
summary(T0)

control=tree.control(nobs=length(Pop),minsize=1, mindev=0.0000001)
T1=tree(Sales ~ Pop,  control=control)
summary(T1)

## Variance of a tree
# first run the function GenData
NSim=1000
y0=c()
y1=c()
for(i in 1:NSim){
  Data=GenData()
  Pop=Data$Pop
  Sales=Data$Sales
  
  T0=tree(Sales ~ Pop)
  y0[i]=predict(T0, newdata=data.frame(Pop=25000))
  
  control=tree.control(nobs=length(Pop),minsize=1, mindev=0.0000001)
  T1=tree(Sales ~ Pop,  control=control)
  y1[i]=predict(T1, newdata=data.frame(Pop=25000))

}
par(mfrow=c(1,2))
hist(y0,100, xlim=c(10000,16500), main="Prediction: Small tree")
hist(y1,100, xlim=c(10000,16500), main="Prediction: Large tree")

par(mfrow=c(1,2))
plot(y0, type="l", ylim=c(10000, 15000), col="blue")
plot(y1, type="l", ylim=c(10000, 15000), col="blue")

# fit the predicted values for the large tree
# training set
y.hat.1=predict(T1)
plot(Pop,Sales, main="Goodness-of-fit: Training data set")
points(Pop,y.hat.1, pch=20, col="red")

# test set
Data.test=GenData()
y.hat.test.1=predict(T1, newdata=data.frame(Pop=Data.test$Pop))
plot(Data.test$Pop, Data.test$Sales, main="Goodness-of-fit: Training data set")
points(Data.test$Pop,y.hat.test.1, pch=20, col="red")

# Training error in function of the size of the tree
c=seq(0.00001,0.05,0.00001)
Size=c()
Data.test=GenData()
Test.error=c()
Training.error=c()

for(i in 1:length(c)){
control=tree.control(nobs=length(Pop),minsize=1, mindev=c[i])
Model=tree(Sales ~ Pop,  control=control)
Size[i]=summary(Model)$size
y.hat=predict(Model)
Training.error[i]=sqrt(mean((Sales - y.hat)^2))
y.hat.test=predict(Model, newdata=data.frame(Pop=Data.test$Pop))
Test.error[i]=sqrt(mean((Data.test$Sales - y.hat.test)^2))
}
par(mfrow=c(1,2))
plot(Size, Test.error, type="l", col="blue", lwd=2, main="Test error")
plot(Size, Training.error, type="l", col="red", lwd=2, main="Training error")


# Generate data
GenData=function(){
  n=100
  Pop=runif(n,280,40000)
  Sales=c()
  Colors=c()
  Means=c()
  for(i in 1:n){
    if(Pop[i]<11598){
      if(Pop[i]<4405){  Sales[i]=1111+rnorm(1,0,1000)
      Colors[i]=1
      Means[i]=1111
      }   else{    Sales[i]=5605+rnorm(1,0,1000)
      Colors[i]=2
      Means[i]=5605
      }
    }
    if(Pop[i]>11598){
      if(Pop[i]<29433)
      {
        Sales[i]=13040+rnorm(1,0,1000)
        Colors[i]=3
        Means[i]=13040
      }
      else
      {
        Sales[i]=23590+rnorm(1,0,1000)
        Colors[i]=4
        Means[i]=23590
      }
    }
  }
  return(data.frame(Pop=Pop, Sales=Sales))
}

# Cost-complexity tree pruning
control=tree.control(nobs=length(Pop),minsize=1, mindev=0.0005)
T1=tree(Sales ~ Pop,  control=control)
summary(T1)
par(mfrow=c(1,2))
plot(T1)
text(T1,pretty=0)

Pruned.tree=prune.tree(T1, best=3)
Pruned.tree
plot(Pruned.tree)
text(Pruned.tree,pretty=1)


cv.Model=cv.tree(T1, FUN=prune.tree, K=10)
cv.Model
plot(cv.Model$k, cv.Model$dev)

#################################################
# Bagging
################################################

# generate a data set
set.seed(25)
n.obs=100 # number of observations
X=sort(runif(n.obs,0,10))
sigma.error=1
y.data= 5*sin(X)+rnorm(n.obs)*sigma.error
Y=(y.data-mean(y.data))/sd(y.data)

# generate a test set
n.obs=100 # number of observations
X.test=sort(runif(n.obs,0,10))
sigma.error=1
y.data= 5*sin(X.test)+rnorm(n.obs)*sigma.error
Y.test=(y.data-mean(y.data))/sd(y.data)

control=tree.control(nobs=length(X),minsize=1, mindev=0.0000001)

# determine the RMSE of a test observation using a single tree
f1=(predict(tree(Y~X, control=control), newdata=data.frame(X=X.test)))
s1=sqrt(mean((f1-Y.test)^2)) # Test RMSE
s1
# plot of the test set 
plot(X.test, Y.test)
points(X.test, f1, pch=20, col="blue", cex=1.5)
# plot of the training set 
plot(X, Y, cex=2, col="red")
points(X, predict(tree(Y~X, control=control)), pch=20, col="blue", cex=1)

# use a bagged decision tree
B=2000
f.i=matrix(1,B,length(X.test))
# each row contains the predictions for a given bootstrap sample
f.i.training =matrix(1,B,length(X))
# each row contains the predictions for a given bootstrap sample
for(i in 1:B){
  D.i=sample(100, replace=TRUE)
  Y.i=Y[D.i] #D.i[1:80]
  X.i=X[D.i]
  f.i[i,]=(predict(tree(Y.i~X.i, control=control), newdata=data.frame(X.i=X.test)))
  f.i.training[i,]=(predict(tree(Y.i~X.i, control=control), newdata=data.frame(X.i=X)))
}
ff=colMeans(f.i)
ff.training=colMeans(f.i.training)
s=sqrt(mean((ff-Y.test)^2))
s

# plot the results for the test data set
plot(X.test, Y.test)
points(X.test, ff, pch=20, col="blue")
points(X.test,f1, pch=20, col="red")

# plot the results for the training data set
plot(X, Y, xlim=c(0,10), cex=1.3)
points(X, ff.training, pch=20, col="blue",cex=1.5)

# Determine the RMSE in function of the number of bags
ff.cumsum=apply(f.i,2,cumsum)/(matrix(rep(seq(1,B,1), length(X.test)),nrow= B, 
                                      byrow=FALSE))
s.cum=sqrt(rowMeans((ff.cumsum- (matrix(rep(Y.test, B),nrow= B, byrow=TRUE)))^2))
plot(seq(1,B), s.cum, type="l", col="blue", lwd=2, xlim=c(0,400))

# inspect the smoothness of the prediction function
XX=seq(0,10,0.1)
B=10000
f.i=matrix(1,B,length(XX)) 
# each row contains the predictions for a given bootstrap sample
for(i in 1:B){
  D.i=sample(100, replace=TRUE)
  Y.i=Y[D.i[1:20]] #D.i[1:80]
  X.i=X[D.i[1:20]]
  f.i[i,]=(predict(tree(Y.i~X.i, control=control), newdata=data.frame(X.i=XX)))
}
ff=colMeans(f.i)

plot(X, Y)
lines(X.test, f1, pch=20, col="blue")
lines(XX, ff, pch=20, col="red", lwd=2)
lines(XX, (5*sin(XX)-mean(5*sin(XX)))/sd(5*sin(XX)), col="green")


points(seq(0,10,0.1), 
       (predict(tree(Sales.i~Pop.i, control=control), 
                newdata=data.frame(Pop.i=seq(0,10,0.1)))), pch=20, col="green")
XX=seq(0,10,0.1)

##############################################
# Boosting
##############################################
rm(list=ls())
# generate a data set
set.seed(25)
n.obs=100 # number of observations
X=sort(runif(n.obs,0,10))
sigma.error=1
y.data= 5*sin(X)+rnorm(n.obs)*sigma.error
Y=(y.data-mean(y.data))/sd(y.data)

# generate a test set
n.obs=100 # number of observations
X.test=sort(runif(n.obs,0,10))
sigma.error=1
y.data= 5*sin(X.test)+rnorm(n.obs)*sigma.error
Y.test=(y.data-mean(y.data))/sd(y.data)

control=tree.control(nobs=100, mindev=0.001)

B=1000
f.i=rep(0, length(X))
r=Y
lambda=1
for( i in 1:B){
  f.b=predict(tree(r~X, control= control))
  f.i= f.i + lambda* f.b
  r=r-lambda*f.b
}

par(mfrow=c(1,2))
plot(X,Y)
points(X, f.i, pch=20)

plot(r)

# investigate the effect on a test data set

control=tree.control(nobs=length(X),mindev=0.00001)

B=2000
f.i.training=rep(0, length(X))
f.i.test=rep(0, length(X.test))
r=Y
lambda=0.05
RMSE=c()
for( i in 1:B){
  D.i=sample(100, replace=TRUE)
  M=tree(r~X, control= control)
  f.b.training=predict(M)
  f.b.test=predict(M, newdata = data.frame(X=X.test))
  
  f.i.training= f.i.training + lambda* f.b.training
  f.i.test= f.i.test + lambda* f.b.test
  r=r-lambda*f.b.training
  
  RMSE[i]= sqrt(mean((f.i.test-Y.test)^2))
  
  
}
plot(seq(1,B,1), RMSE, type="l", col="blue",  ylim=c(0.4, 0.5), lwd=2)

plot(X.test, f.i.test, type="l")
