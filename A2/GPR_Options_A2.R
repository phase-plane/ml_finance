## Assignment 2

## Gauss Process Regression (GPR) for Option Pricing

## libraries
library(MASS)
library(GauPro)
library(ggplot2)

## a) Create the dataset of call option prices
##    Plain Vanilla Call
BLSPrice <- function(S, M, cap.T, sigma, r){
  ## M = K/S
  d1 <- (log(1/M) + (r + 0.5*sigma^2)*cap.T) / (sigma*sqrt(cap.T))
  d2 <- d1 - sigma*sqrt(cap.T)
  C <- S*pnorm(d1) - S*M*exp(-r*cap.T)*pnorm(d2)
  return(C)
}

## parameters for simulation
set.seed(1)
N <- 100
S <- runif(N, 80, 120)
M <- runif(N, .5, 1.5)
cap.T <- runif(N, 1/12, 1)
sigma <- runif(N, 0.1, 0.4)
r <- runif(N, 0, 0.05)

## dataset
vanilla.C <- BLSPrice(S, M, cap.T, sigma, r)

## target Y dim(n x 1)
Y <- vanilla.C

## feature matrix X dim(n x 5)
X <- cbind(S, M, cap.T, sigma, r)

## b) fit GPR using GauPro
GPR.call <- GauPro(X, Y)

## c) Use the fitted Gauss Process regression to determine 
##    the price of call options for different strike prices.
Np <- 100
Sp <- 100 + rep(0, Np)
Mp <- seq(0.5, 1.5, length.out = Np)
Tp <- 1/12 + rep(0, Np)
sigp <- 0.3 + rep(0, Np)
rp <- 0.01 + rep(0, Np)

## create new design matrix
Xp <- cbind(Sp, Mp, Tp, sigp, rp)

## predictions
Cp <- BLSPrice(Sp, Mp, Tp, sigp, rp)
Yp <- GPR.call$pred(Xp)

## Plot model predictions
ggplot() + 
  geom_line(aes(Mp, Cp, color = "C"), size = 1.5) + 
  geom_line(aes(Mp, Yp, color = "C.hat"), size = 1.5) +
  scale_color_manual("",
                     breaks = c("C", "C.hat"),
                     values = c("C" = "blue", "C.hat" = "red")) +
  labs(x = "M", y = "C") +
  theme(text = element_text(size = 10))

## DIY
par(mfrow=c(1,1))
plot(Mp, Cp, type="l", col="blue", xlab = "M", ylab = "C")
lines(Mp, Yp, type = "l", col = "red")

## include legends 
legend("topright", c("Black Scholes","Prediction"), 
       lty=c(1,1), lwd=c(2.5,2.5), col=c("blue" ,"red"))

## DIY2
U <- as.vector(GPR.call$pred(Xp_norm) + 2*sqrt(diag(GPR.call$Kchol)))
L <- as.vector(GPR.call$pred(Xp_norm)) - 2*sqrt(diag(GPR.call$Kchol))

plot(Mp, Cp, type="l", col="blue", xlab = "M", ylab = "C", 
     ylim=c(min(L), max(U)))
lines(Mp, Yp, type = "l", col = "red")
lines(Mp, U, lty=2, col="grey", lwd=2, type="l")
lines(Mp, Yp, lwd=2, lty=2, col="blue")
lines(Mp, L, lty=2, col="red", lwd=2)
lines(Mp, U, lty=2, col="green", lwd=2)