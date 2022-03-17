## Project 1

## VIXSP500
## Import the data
Data <- read.table(file="VIXSP500.csv", header = TRUE, sep = ",")
Data$Date <- as.Date(Data$Date, origin = "1899-12-31")
Data$VIX <- Data$VIX / 100

## exploratory plot (cover graphic)
par(mfrow=c(1,1))
plot(Data$Date, Data$Returns, type="l", col="red",
     xlab = "", ylab = "", xaxt='n', yaxt='n')
lines(Data$Date, Data$VIX, type = "l", col = "blue")

## include legends 
legend("topleft", c("VIX","S&P 500 Return"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"red"))

 
## Daily Log-return, given VIX (same day)

## create matrix of target
lr.sp500 <- na.omit(Data$Log.returns)
lr.sp500 <- matrix(lr.sp500, length(lr.sp500), 1)

## create matrix of predictor
vix <- Data$VIX[-1]
vix <- matrix(vix, length(vix), 1)

## knn function under square loss
KNN <- function(X,Y,k,X.hat){
  # X is the feature matrix. Each row contains one observation
  # Y are the target variables
  # X.hat matrix with observations for which we want to determine the yhat value
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  n.hat <- dim(X.hat)[1]
  y.hat <- rep(0, n.hat)
  for (i in 1:n.hat){
    x <- X.hat[i,]
    DD <- sqrt(rowSums((X-matrix(1,n,1)%*%x)^2)) # distance
    S <- sort(DD, index.return=TRUE)
    I <- S$ix[1:k]
    y.hat[i] <- mean(Y[I])
  }
  return(y.hat)
}

## sanity check
sanity <- KNN(vix,lr.sp500,1,vix)
plot(vix, lr.sp500, pch=20, col="red")
points(vix, sanity, pch=20, col="blue")
abline(h=0, lwd=1.2)
all.equal(as.numeric(lr.sp500), sanity, tolerance = 0.00001)

## Run KNN for a variety of k
X <- vix; Y <- lr.sp500;
k.vector <- seq.int(1,150,5)
MSE.train <- rep(0,length(k.vector))
for (j in 1:length(k.vector)){
  Y.pred <- KNN(X,Y,k.vector[j],X)
  MSE.train[j] <- mean((Y-Y.pred)^2)
}

plot(k.vector, MSE.train, type="l", col="blue", 
     main = "Training MSE for different choices of K", 
     xlab = "K", ylab = "MSE")

## a) Validation set approach
##    randomly split the data (80 test - 20 validation)
X <- vix; Y <- lr.sp500; split <- 0.8;
I <- sample(1:dim(X)[1], size=floor(split*(dim(X)[1])));
X.train <- matrix(X[I], length(X[I]), dim(X)[2]) ; 
Y.train <- matrix(Y[I], length(Y[I]), dim(Y)[2]);
X.val <- matrix(X[-I], length(X[-I]), dim(X)[2]); 
Y.val <- matrix(Y[-I], length(Y[-I]), dim(Y)[2]);
k.vector <- seq.int(1, 150, 5)
MSE.val<- rep(0, length(k.vector))
for (j in 1:length(k.vector)){
  Y.pred <- KNN(X.train,Y.train,k.vector[j], X.val)
  MSE.val[j] <- mean((Y.val-Y.pred)^2)
}

plot(k.vector, MSE.val, type="l", col="forestgreen",
     main = "Validation MSE for different choices of K",
     xlab = "K", ylab = "MSE" )
idx <- which.min(MSE.val)
k.vector[idx]

## Choice of k
## BEWARE of running time above 100 simulations
n.sims <- 50
k.vector <- seq.int(1, 500, 10)
choice <- rep(0, n.sims)
for (i in 1:n.sims){
  I <- sample(1:dim(X)[1], size=floor(split*(dim(X)[1])));
  X.train <- matrix(X[I], length(X[I]), dim(X)[2]) ; 
  Y.train <- matrix(Y[I], length((Y)[I]), dim(Y)[2]);
  X.val <- matrix(X[-I], length(X[-I]), dim(X)[2]); 
  Y.val <- matrix(Y[-I], length(Y[-I]), dim(Y)[2]);
  MSE.val<- rep(0, length(k.vector))
  for (j in 1:length(k.vector)){
    Y.pred <- KNN(X.train,Y.train,k.vector[j], X.val)
    MSE.val[j] <- mean((Y.val-Y.pred)^2)
  }
  idx <- which.min(MSE.val)
  choice[i] <- k.vector[idx]
}

hist(choice, xlab = "Choice of K", 
     main = "Choice of K for 1000 Training and Validation Splits")

## b) LOOCV
k.vector <- seq.int(5, 200, 10)
MSE.vec <- rep(0, length(k.vector))
LOOCV.mse <- rep(0, dim(Y)[1])
for (j in 1:length(k.vector)){
  k <- k.vector[j]
  for (i in 1:dim(Y)[1]){
    X.train <- matrix(X[-i], length(X[-i]), dim(X)[2]) ; 
    Y.train <- matrix(Y[-i], length((Y)[-i]), dim(Y)[2]);
    X.val <- matrix(X[i], length(X[i]), dim(X)[2]); 
    Y.val <- matrix(Y[i], length(Y[i]), dim(Y)[2]);
    Y.hat <- KNN(X.train,Y.train, k, X.val)
    LOOCV.mse[i] <- (Y.hat-Y.val)^2
  }
  MSE.vec[j] <- mean(LOOCV.mse)
}

plot(k.vector, MSE.vec, type ="l", col="blue", xlab = "K", ylab = "MSE",  
     main = "LOOCV MSE for different choices of K")
idx <- which.min(MSE.vec)
k.vector[idx]


## c) 5 & 10 fold CV

## recall
X <- vix; Y <- lr.sp500;

## k-fold cross validation
kfCV=function(X,Y,a){
  # a is the number of folds
  # X, Y matrices
  # Creates matrix Folds' (each column represents 1 fold)
  n <- dim(Y)[1]
  n.fold <- floor(n/a) # truncate to avoid remainder
  Folds <- matrix(1,n.fold,a)
  A <- seq.int(1,n,1)
  A0 <- sample(A, n.fold)
  Folds[,1] <- A0
  for(i in 2:a){
    A1 <- sample(A[-A0], n.fold)
    Folds[,i] <- A1
    A0 <- c(A0,A1)
  }
  KK <- seq(5,200,10) # number of neighbors of the KNN

  MSE <- rep(0, length(KK)) # MSE for each k
  for(l in 1:length(KK)){
    k <- KK[l]
    MSE.Fold <- rep(0,a) # MSE for each fold as validation set
    for(j in 1:a){
      Train.X <- as.matrix(X[-Folds[,j],]) # everything except fold j is the training set
      Train.Y <- as.matrix(Y[-Folds[,j]])
      Test.X <- as.matrix(X[Folds[,j],])  # Fold j is the validation set
      Test.Y <- as.matrix(Y[Folds[,j]])
      y.hat <- KNN(Train.X, Train.Y, k, Test.X)
      MSE.Fold[j] <- sum((y.hat-Test.Y)^2)
    }
    MSE[l] <- mean(MSE.Fold)
  }
  opt.k <- sort(MSE, index.return=TRUE)$ix[1]
  opt.MSE <- sort(MSE, index.return=TRUE)$x[1]
  return(data.frame(K=KK, MSE=MSE))
}

Model.5fold <- kfCV(X,Y,5)
Model.10fold <- kfCV(X,Y,10)
par(mfrow=c(1,2))
plot(Model.5fold$K, Model.5fold$MSE, xlab="Number of neighbours", 
     ylab="Validation error", main = "5-fold cross validation", pch=20, col="blue")
plot(Model.10fold$K, Model.10fold$MSE, xlab="Number of neighbours", 
     ylab="Validation error", main = "10-fold cross validation", pch=20, col="blue")

plot(k.vector, MSE.vec, xlab="Number of neighbours", 
     ylab="Validation error", main = "LOOCV cross validation", pch=20, col="blue")

idx <- which.min(Model.10fold$MSE)
Model.10fold$K[idx]

## visualising the KNN model
par(mfrow=c(1,1))
y.hat <- KNN(X,Y,100,X)
plot(X, Y, pch=20, col="blue", cex=0.8, xlim=c(0.05,.5), 
     ylim=c(min(Y), max(Y)), xlab ="VIX",
     ylab=expression('r'[t]), main = "Data vs Fitted Values")
points(X, y.hat, pch=20, col="red", cex=0.8, xlim=c(0.05,.5), ylab="")
abline(h=0.0005, lwd=1.5)

## include legends 
legend("topright", c("Data","Fitted Values"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"red"))

## Part II: Up-Down Classification, given VIX (same day)

## clear working environment
rm(list=ls())

## reimport the data
Data <- read.table(file="VIXSP500.csv", header = TRUE, sep = ",")
Data$Date <- as.Date(Data$Date, origin = "1899-12-31")
Data$VIX <- Data$VIX / 100

## create vector of target
returns <- na.omit(Data$Returns)
class.returns <- as.numeric(returns>0.005)

## create vector of predictor
vix <- Data$VIX[-1]

## KNN written for matrix arguments
X <- as.matrix(vix); Y <- as.matrix(class.returns)

## write a new function for KNN
KNN.classify <- function(Training, Y, k, Test){
  min.x=function(y){
    return((y-x)^2)
  }
  y_hat <- rep(0, dim(Test)[1])
  for (i in 1:dim(Test)[1]){
    x <- as.numeric(Test[i,])
    if(dim(Test)[2]==1){DD=sqrt((as.numeric(Training)-x)^2)}
    else{
      DD=sqrt(rowSums(t(apply(Training,1,min.x)))) # distance
    }
    S=sort(DD, index.return=TRUE)
    I=S$ix[1:k]
    y_hat[i]=1*(mean(Y[I])>0.5)
  }
  return(y_hat)
}

Y.hat <- KNN.classify(X,Y,125,X)

# misclassification error
MisClassification <- length(Y)-sum((Y-Y.hat)==0)
MisClassification
MisClassification/length(Y)
table(Y,Y.hat)

## k-fold cross validation
kfCV=function(X,Y,a){
  # a is the number of folds
  # X, Y matrices
  # Creates matrix Folds' (each column represents 1 fold)
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
  
  KK=seq.int(5,450,6) # number of neighbors of the KNN
  
  MSE <- rep(0,length(KK)) # MSE for each k
  for(l in 1:length(KK)){
    k=KK[l]
    MSE.Fold <- rep(0,a)  #MSE for each fold as validation set
    for(j in 1:a){
      Train.X <- as.matrix(X[-Folds[,j],]) # everything except fold j
      Train.Y <- as.matrix(Y[-Folds[,j]])
      Test.X <- as.matrix(X[Folds[,j],])  # Fold j is the validation set
      Test.Y <- as.matrix(Y[Folds[,j]])
      y.hat <- KNN.classify(Train.X, Train.Y, k, Test.X)
      MSE.Fold[j] <- sum(y.hat != as.numeric(Test.Y))
    }
    MSE[l]=mean(MSE.Fold)
  }
  opt.k=sort(MSE, index.return=TRUE)$ix[1]
  opt.MSE=sort(MSE, index.return=TRUE)$x[1]
  return(data.frame(K=KK, MSE=MSE))
}

## fitting the models
Model.5fold <- kfCV(X,Y,5)
Model.10fold <- kfCV(X,Y,10)

## plotting the results
par(mfrow=c(1,2))

plot(Model.5fold$K, Model.5fold$MSE, xlab="Number of neighbours", 
     ylab="Validation error", main = "5-fold cross validation", 
     pch=20, col="blue")

plot(Model.10fold$K, Model.10fold$MSE, xlab="Number of neighbours",
     ylab="Validation error", main = "10-fold cross validation", 
     pch=20, col="blue")

## identify optimal k
idx <- which.min(Model.5fold$MSE)
Model.5fold$K[idx]

idx <- which.min(Model.10fold$MSE)
Model.10fold$K[idx]

## Trading strategy / Movement Classifier
k <- 120
n.obs <- dim(Y)[1]

f.split <- floor(0.5*n.obs)
c.split <- ceiling(0.5*n.obs)

Wealth <- rep(0, c.split)

for (i in 1:c.split){
  X.train <- as.matrix(X[1:(f.split+i-1),])
  Y.train <- as.matrix(Y[1:(f.split+i-1),])
  y.hat <- KNN.classify(X.train, Y.train, 120, as.matrix(X[(f.split+i),]))
  if (i==1){
    if (y.hat==1){
      Wealth[i] <- 100*(1+returns[c.split])
    } else { 
      Wealth[i] <- 100
    }
  } else {
      if (y.hat==1){
        Wealth[i] <- Wealth[i-1]*(1+returns[f.split+i])
      } else { 
        Wealth[i] <- Wealth[i-1]
      }
  }
}

## market portfolio
Market <- rep(0, dim(X)[1])
Market[1] <- 100*(1+returns[1])
for (i in 2:length(Market)){
  Market[i] <- Market[i-1]*(1+returns[i])
}

## one gamble
Gamble <- rep(0, c.split)
n <- length(Gamble)
guess <- sample(c(0,1), n, replace = TRUE, prob = c(.5, .5))

for (i in 1:n){
  y.hat <- guess[i]
  if (i==1){
    if (y.hat==1){
      Gamble[i] <- 100*(1+returns[c.split])
    } else { 
      Gamble[i] <- 100
    }
  } else {
    if (y.hat==1){
      Gamble[i] <- Gamble[i-1]*(1+returns[f.split+i])
    } else { 
      Gamble[i] <- Gamble[i-1]
    }
  }
}

## plot 1 gamble
par(mfrow=c(1,1))
plot(Wealth, type ="l", col="blue", xlab ="Trading Day", 
     ylim=c(min(Market), max(Wealth)), main = "Portfolio Value")
lines(Market, col="red")
lines(Gamble,col="forestgreen")

## include legends 
legend("topleft", c("Strategy","Market", "50-50 Gamble"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"red", "forestgreen"))

## M gambles
m <- 100
Paths <- matrix(0, nrow = length(Gamble), ncol = m)
for (s in 1:m){
  Gamble <- rep(0, c.split)
  n <- length(Gamble)
  guess <- sample(c(0,1), n, replace = TRUE, prob = c(.5, .5))
  for (i in 1:n){
    y.hat <- guess[i]
    if (i==1){
      if (y.hat==1){
        Gamble[i] <- 100*(1+returns[c.split])
      } else { 
        Gamble[i] <- 100
      }
    } else {
      if (y.hat==1){
        Gamble[i] <- Gamble[i-1]*(1+returns[f.split+i])
      } else { 
        Gamble[i] <- Gamble[i-1]
      }
    }
  }
  Paths[,s] <- Gamble
}

plot(Wealth, type ="l", col="blue", xlab ="Trading Day", lwd=3, 
     ylim=c(min(Market), max(Paths, Wealth)), 
     main="Portfolio Value for 100 Gambling realisations")
for (i in 1:dim(Paths)[2]){
  lines(Paths[,i], col="forestgreen", lwd=1)
}

## include legends 
legend("topleft", c("Strategy", "50-50 Gamble"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"forestgreen"))

## Linear Regression
#Create squared and to the power of three values
Data$Squared = Data$Returns**2
Data$Three = Data$Returns**3

#Estimate regression models
lin_reg1 <- lm(Data$Log.returns~Data$Returns)
lin_reg2 <- lm(Data$Log.returns~Data$Squared)
lin_reg3 <- lm(Data$Log.returns~Data$Three)
lin_reg4 <- lm(Data$Log.returns~Data$Returns + Data$Squared)
lin_reg5 <- lm(Data$Log.returns~Data$Returns + Data$Three)
lin_reg6 <- lm(Data$Log.returns~Data$Squared + Data$Three)
lin_reg7 <- lm(Data$Log.returns~Data$Returns + Data$Squared + Data$Three)

cat("Model 1: R-squared:", summary(lin_reg1)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg1)$adj.r.squared)
cat("Model 2: R-squared:", summary(lin_reg2)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg2)$adj.r.squared)
cat("Model 3: R-squared:", summary(lin_reg3)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg3)$adj.r.squared)
cat("Model 4: R-squared:", summary(lin_reg4)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg4)$adj.r.squared)
cat("Model 5: R-squared:", summary(lin_reg5)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg5)$adj.r.squared)
cat("Model 6: R-squared:", summary(lin_reg6)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg6)$adj.r.squared)
cat("Model 7: R-squared:", summary(lin_reg7)$r.squared, "and adjusted R-squared:", 
    summary(lin_reg7)$adj.r.squared)