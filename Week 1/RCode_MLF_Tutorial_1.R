
#################################################################################
## Question 4: make a plot of the target variable agains the feature variables.
#################################################################################

# Create the data set
set.seed(100)
x1=rnorm(1000,0,1)*0.4
x2=0.5*x1 + 0.5*rnorm(1000,0,1)
x3=runif(1000,0,1)

sigma=0.2
Y=0.5*x1^2 +1*x2  - x3 +sigma*rnorm(1000,0,1)

#################################################################################
## Question a: make a plot of the target variable agains the feature variables.
#################################################################################
par(mfrow=c(2,2))
plot(x1,Y)
plot(x2,Y)
plot(x3,Y)

#################################################################################
## Question b: write a KNN function
#################################################################################

KNN = function(X,Y,k,X.hat){
  # X is the feature matrix. Each row contains on observation
  # Y are the target variables
  # X.hat matrix with observations for which we want to determine the yhat value
  n=dim(X)[1]
  p=dim(X)[2]
  
  n.hat=dim(X.hat)[1]

  y.hat=c()
  for(i in 1:n.hat){
    x=X.hat[i,]
    DD=sqrt(rowSums((X-matrix(1,n,1)%*%x)^2))
    S=sort(DD, index.return=TRUE)
    I=S$ix[1:k]
    y.hat[i]=mean(Y[I])
  }
  return(y.hat)
}

#################################################################################
## Question c:  Determine the training MSE 
#################################################################################

X=matrix(c(x1,x2,x3), length(x1), 3)
Y.hat=KNN(X,Y,50,X) #the X.hat matrix is equal to X, since we determine a training MSE

# investigate the fit
par(mfrow=c(2,2))
plot(x1,Y)
points(x1,Y.hat,pch=20, col="blue")

plot(x2,Y)
points(x2,Y.hat,pch=20, col="blue")

plot(x3,Y)
points(x3,Y.hat,pch=20, col="blue")

plot(Y,Y.hat,pch=20, col="blue")

MSE= mean((Y-Y.hat)^2)
MSE


#################################################################################
## Question d: # Investigate the training MSE in function of k
#################################################################################

k.vector= seq(1,100,5)
MSE=c()
for(i in 1:length(k.vector)){
  Y.hat=KNN(X,Y,k.vector[i],X)
  MSE[i]= mean((Y-Y.hat)^2)
}
plot(k.vector, MSE, pch=20, col="blue")

#################################################################################
## Question e: # Investigate the validation MSE in function of k
#################################################################################
# create a test set
set.seed(101)
x1=rnorm(1000,0,1)*0.4
x2=0.5*x1 + 0.5*rnorm(1000,0,1)
x3=runif(1000,0,1)

Y.test=0.5*x1^2 +1*x2  - x3 +sigma*rnorm(1000,0,1)
X.test=matrix(c(x1,x2,x3), length(x1), 3)



k.vector= seq(5,60,1)
MSE.test=c()
for(i in 1:length(k.vector)){
  Y.hat=KNN(X,Y,k.vector[i],X.test)
  MSE.test[i]= mean((Y.test-Y.hat)^2)
}
plot(k.vector, MSE.test, pch=20, col="blue")

# find the optimal k
I=sort(MSE.test, index=TRUE)$ix[1]
k.vector[I]


#################################################################################
## Question f: # The variance of the prediction 
#################################################################################
nSim=1000
Y.hat=c()
x=matrix(c(0,0.1,0.5), 1, 3)
for(i in 1:nSim){
  #Simulate a new data set
  x1=rnorm(1000,0,1)*0.4
  x2=0.5*x1 + 0.5*rnorm(1000,0,1)
  x3=runif(1000,0,1)
  Y=0.5*x1^2 +1*x2  - x3 +sigma*rnorm(1000,0,1)
  X=matrix(c(x1,x2,x3), length(x1), 3)
  Y.hat[i]=KNN(X,Y,200,x)
}


TrueValue=0.5*x[1]^2 +1*x[2]  - x[3]
Bias.hat=TrueValue-mean(Y.hat)
SE.hat=sd(Y.hat)
hist(Y.hat,100)
abline(v=TrueValue, col="red", lwd=2)

SE.hat
Bias.hat


#################################################################################
## Question 5: Assume a multivariate normal vector (X,Y)
#################################################################################

n.obs=2000
X=rnorm(n.obs, 0,1)
Y=0.5*X +(1-0.5^2)*rnorm(n.obs,0,1)
X=matrix(X, length(X),1)

plot(X,Y,pch=20, col="blue")
x=seq(-4,4,0.1)
f.c=0.5*x
lines(x,f.c, type="l", lwd=2, col="green")


y.hat=KNN(X,Y,80,matrix(x,length(x),1))
points(x,y.hat, pch=20, lwd=2, col="red")

# The red points, i.e. the KNN predictions, are an estimate for the `real` 
# conditional expectation. 
# In this particular example, we can determine the real conditional 
# expectation by using the bivariate normal distribution.