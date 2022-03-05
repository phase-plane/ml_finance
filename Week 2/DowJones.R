## Question 5

## Dow Jones
Data <- read.table(file="DowJones.csv", header = TRUE, sep = ",")
Data$date <- as.Date(Data$date, origin = "1899-12-31")
Dow <- as.numeric(Data$value)
plot(Data$date, Dow, type = "l", col="royalblue")

KNN <- function(X,Y,k,X.hat){
  # X.hat matrix with observations for which we want to determine the yhat value
  n <- dim(X)[1]
  p <- dim(X)[2]
  n.hat <- dim(X.hat)[1]
  y.hat <- rep(0,n.hat)
  for (i in 1:n.hat){
    x <- X.hat[i,]
    DD <- sqrt(rowSums((X-matrix(1,n,1)%*%x)^2)) # distance
    S <- sort(DD, index.return=TRUE)
    I <- S$ix[1:k]
    y.hat[i] <- mean(Y[I])
  }
  return(y.hat)
}