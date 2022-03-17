##################################################
## Classification with KNN
##################################################

rm(list=ls())
Data=read.csv("DowJones.csv")
head(Data)
TT=as.Date(as.numeric(Data$date),origin = "1899-12-31")
Dow=as.numeric(Data$value)
Return=c(0,diff(Dow)/(Dow[1:(length(Dow)-1)]))
plot(TT,Dow, type="l", col="blue", lwd=2, xlab="Time", ylab="Dow Jones")
plot(TT,Return, type="l", col="blue", lwd=2, xlab="Time")

# create the explanatory variables and the target variable
num.past=3 #at least 2
num.fut=1 

n.features=length(Dow)-num.fut-num.past+1
XX=matrix(1,n.features,num.past)
Y=c()
R=c()
for( i in 1:n.features){
  XX[i,]=Return[i:(i+num.past-1)]
  R[i]=(Dow[i+num.past+num.fut-1]-Dow[i+num.past-1])/Dow[i+num.past-1]
  Y[i]=(R[i]>0.0005)*1
}

par(mfrow=c(2,2))
plot(XX[,1],Y, pch=20, col="blue", cex=1, xlab="R(t-1)", cex.lab=1.5)
plot(XX[,2],Y, pch=20, col="blue", cex=1, xlab="R(t-2)", cex.lab=1.5)
plot(XX[,3],Y, pch=20, col="blue", cex=1,  xlab="R(t-3)", cex.lab=1.5)


summary(Return)
summary(Y)
table(Y)

## write a function for KNN
KNN=function(Training, Y, Test, k){
  min.x=function(y){
    return((y-x)^2)
  }
  y_hat=c()
  for(i in 1:dim(Test)[1]){
    x=Test[i,]
    if(dim(Test)[2]==1){DD=sqrt((Training-x[i,])^2)}
    else{
      DD=sqrt(rowSums(t(apply(Training,1,min.x)))) # distance
    }
    S=sort(DD, index.return=TRUE)
    I=S$ix[1:k]
    y_hat[i]=1*(mean(Y[I])>0.5)
    
  }
  return(y_hat)
}
# estimate the returns using only a training set
Y.hat=KNN(XX,Y,XX,100)

# misclassification error
MisClassification=length(Y)-sum((Y-Y.hat)==0)
MisClassification
MisClassification/length(Y)
table(Y,Y.hat)

plot(XX[,3],Y, col="blue", cex=1,  xlab="R(t-3)", cex.lab=1.5)
points(XX[,3], Y.hat, pch=20, col="red")
# Use k-fold cross validation to determine the optimal k

## k-fold cross validation
kfCV=function(X,Y,a){
  # a is the number of folds
  # The matrix Folds has in each column the index number for that particular fold
  min.x=function(y){
    return((y-x)^2)
  }
  n=length(Y)
  n.fold=floor(n/a)
  Folds=matrix(1,n.fold,a)
  A=seq(1,n,1)
  A0=sample(A, n.fold)
  Folds[,1]=A0
  for(i in 2:a){
    A1=sample(A[-A0], n.fold)
    Folds[,i]=A1
    A0=c(A0,A1)
  }
  KK=seq(5,800,10) # number of neighbors of the KNN
  
  MSE=c() # MSE for each k
  for(l in 1:length(KK)){
    k=KK[l]
    MSE.Fold=c() #MSE for each fold as validation set
    for(j in 1:a){
      MSE_Test.i=c() # MSE for each fold as validation set
      y.hat.i=c()
      Train.X=X[-Folds[,j],] # everything except fold j is the training set
      Train.Y=Y[-Folds[,j]]
      Test.X=X[Folds[,j],]  # Fold j is the validation set
      Test.Y=Y[Folds[,j]]
      for(i in 1:length(Test.Y)){
        x=Test.X[i,]
        DD=sqrt(rowSums(t(apply(Train.X,1,min.x))))
        S=sort(DD, index.return=TRUE)
        I=S$ix[1:k]
        y.hat.i[i]=1*(mean(Train.Y[I])>0.5)
      }
      MSE.Fold[j]=sum((y.hat.i!=Test.Y))
    }
    MSE[l]=mean(MSE.Fold)
  }
  opt.k=sort(MSE, index.return=TRUE)$ix[1]
  opt.MSE=sort(MSE, index.return=TRUE)$x[1]
  return(data.frame(K=KK, MSE=MSE))
}

Model.5fold=kfCV(XX,Y,5)
Model.10fold=kfCV(XX,Y,10)
par(mfrow=c(1,2))
plot(Model.5fold$K, Model.5fold$MSE, xlab="Number of neighbours", 
     ylab="Validation error", main = "5-fold cross validation", pch=20, col="blue")
plot(Model.10fold$K, Model.10fold$MSE, xlab="Number of neighbours", 
     ylab="Validation error", main = "10-fold cross validation", pch=20, col="blue")


# time series cross validation

TimeSeriesCV=function(X,Y,a){
  KK=seq(1,500,10)
  MSE=c()
  for(j in 1:length(KK)){
    k=KK[j]
  # we take 50% of the data set as training set
  n.obs=length(Y)
  Training.X=X[1:(0.5*n.obs),]
  Training.Y =Y[1:(0.5*n.obs)]
  n.Training=length(Training.Y)
  n.Test=n.obs-n.training
  
  n.Fold=floor(n.Test/a)
  if((n.Fold*a)<n.Test){a=a+1
  II=c(rep(n.Fold,(a-1)), n.Test-(n.Fold*(a-1)))  # we create an extra fold 
                                                  # to catch remaining observations
  } else {II =c(rep(n.Fold,a)) } ## II is a vector with the number of observations 
                                  # in each fold
  
  Start=n.Training+1
  MSE.i=c()
  for(i in 1:a){
    End=n.Training + II[i]
    x=XX[Start:End,]
    y.hat.i=  KNN( Training.X, Training.Y, x, k)
    MSE.i[i]= sum((y.hat.i!=Y[Start:End]))
  
    Start=End+1
    Training.X=X[1:End,]
    Training.Y=Y[1:End]
    n.Training=length(Training.Y)
  }
  MSE[j]=mean(MSE.i)
  }
  return(data.frame(K=KK, MSE=MSE))
}

AA=TimeSeriesCV(XX,Y,5)
plot(AA$K, AA$MSE, xlab="Number of neighbours", ylab="Validation error", 
     cex.lab=1.5, pch=20, col="blue")


## Build a trading strategy
k=100
n.obs=length(Y)
Training.X=XX[1:(0.5*n.obs),]
Training.Y =Y[1:(0.5*n.obs)]
n.Training=length(Training.Y)
n.Test=n.obs-n.Training
End=n.Training
y.hat.i=c()
Wealth=c()
Wealth[1]=100
for(i in 1:(n.Test-1)){
  x=matrix(XX[(n.Training+i),],1,3)
  y.hat.i[i]=  KNN( Training.X, Training.Y, x, k)
Ret.i=XX[(n.Training+i+1),3]
  Wealth[i+1]=Wealth[i]*(1+Ret.i*y.hat.i[i])
  Training.X=XX[1:(n.Training+i),]
  Training.Y=Y[1:(n.Training+i)]
}

par(mfrow=c(2,1))
plot(TT[(n.Training+1):length(Y)],Dow[(n.Training+1):length(Y)], 
     col=(y.hat.i+2), pch=20)
plot(Wealth)
