rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(0)

# Load libraries
library(ggplot2)
library(tensorflow)
library(keras)


##################################################
##### Question 1
##################################################

# Create data set
set.seed(1)
N = 100
X = 1*rnorm(N)
Y = 0.5 + 2*X + 0.5*rnorm(N)


##################################################
# (a)
##################################################

Z = matrix(c(rep(1,N),X), N, 2, byrow=FALSE)

beta.ols = solve(t(Z)%*%Z)%*%t(Z)%*%matrix(Y,N,1)
beta.ols


##################################################
# (b)
##################################################

# Define model
model1 = keras_model_sequential() %>% 
  layer_dense(units = 1)

# Compile model
model1 %>% 
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = "sgd"
  )

# Fit model
model1 %>% 
  fit(
    x = matrix(X), 
    y = matrix(Y),
    epochs = 100,
    validation_split = 0.0,
    verbose = 2
  )

summary(model1)

# Coefficients
# use model$weights for details
beta.nn = get_weights(model1)
beta.nn = t(t(c(beta.nn[[2]], beta.nn[[1]])))
beta.nn


##################################################
# (c)
##################################################

# Define model
model2 = keras_model_sequential() %>% 
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1)

# Compile model
model2 %>% 
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = "sgd"
  )

# Fit model
model2 %>% 
  fit(
    x = matrix(X), 
    y = matrix(Y),
    epochs = 100,
    validation_split = 0.0,
    verbose = 2
  )

summary(model2)


##################################################
# (d)
##################################################

# Predict model 1
Xp1 = seq(min(X), max(X), length.out = 200)
Yp1 = predict(model1, matrix(Xp1))

# Predict model 2
Xp2 = seq(min(X), max(X), length.out = 200)
Yp2 = predict(model2, matrix(Xp2))

# Plot
ggplot() + 
  geom_point(aes(X, Y), size = 2) + 
  geom_line(aes(Xp1, Yp1, color = "M1"), size = 1.5) + 
  geom_line(aes(Xp2, Yp2, color = "M2"), size = 1.5) +
  scale_color_manual("",
                     breaks = c("M1", "M2"),
                     values = c("M1" = "blue", "M2" = "red")) +
  labs(x = "M", y = "C") +
  theme(text = element_text(size = 20))


##################################################
##### Question 2
##################################################

# Generate data
set.seed(1)
N = 1000
X = 0.5*rnorm(N)

ProbFcns = function(X){
  f1 = 1.5 + 1.0*X
  f2 = 1.0 + 1.5*X
  f3 = 0.5 + 2.0*X
  
  P1 = exp(f1)/(exp(f1) + exp(f2) + exp(f3))
  P2 = exp(f2)/(exp(f1) + exp(f2) + exp(f3))
  P3 = exp(f3)/(exp(f1) + exp(f2) + exp(f3))
  
  P = cbind(P1,P2,P3)
  return(P)
}


##################################################
# (a)
##################################################

# Generate sample for Z
I = matrix(0, nrow = N, ncol = 3)
Z = matrix(0, nrow = N, ncol = 1)
for (i in 1:N) {
  I[i,] = rmultinom(1, 1, ProbFcns(X[i])) # 3x1 indicator function
  Z[i,] = sum(I[i,]*c(1,2,3)) # convert to category number
}

ggplot() +
  geom_point(aes(X, Z), size = 2, color = "blue")


##################################################
# (c)
##################################################

# Generate another sample for Z
It = matrix(0, nrow = N, ncol = 3)
Zt = matrix(0, nrow = N, ncol = 1)
for (i in 1:N) {
  U = runif(1)
  Q = cumsum(ProbFcns(X[i]))
  It[i,] = c(U <= Q[1], (U > Q[1])&(U <= Q[2]), U > Q[2])*1 # 3x1 indicator function
  Zt[i,] = sum(It[i,]*c(1,2,3)) # convert to category number
}

ggplot() +
  geom_point(aes(X, Z), size = 2, color = "blue") +
  geom_point(aes(X, Zt), size = 2, color = "red")

# Compare both approaches
# Run Monte Carlo simulation on fixed x grid
Np = 100
Nq = 10000
Xp = seq(-2, +2, length.out = Np)

Pp0 = ProbFcns(Xp)

Pp1 = matrix(0, nrow = Np, ncol = 3)
Pp2 = matrix(0, nrow = Np, ncol = 3)
for (i in 1:Np) {
  # Approach 1
  Pp1[i,] = rmultinom(1, Nq, ProbFcns(Xp[i]))/Nq
  # Approach 2
  U = runif(Nq)
  Q = cumsum(ProbFcns(Xp[i]))
  Pp2[i,] = colMeans(cbind(U <= Q[1], (U > Q[1])&(U <= Q[2]), U > Q[2])*1)
}

ggplot() + 
  geom_point(aes(Xp, Pp0[,1]), size = 2.5, color = "blue") +
  geom_point(aes(Xp, Pp0[,1] + Pp0[,2]), size = 2.5, color = "blue") +
  geom_point(aes(Xp, Pp1[,1]), size = 1.5, color = "red") +
  geom_point(aes(Xp, Pp1[,1] + Pp1[,2]), size = 1.5, color = "red") +
  geom_point(aes(Xp, Pp2[,1]), size = 1.5, color = "orange") +
  geom_point(aes(Xp, Pp2[,1] + Pp2[,2]), size = 1.5, color = "orange") +
  labs(x = "x", y = "Prob")


##################################################
# (e)
##################################################

# Define model
model = keras_model_sequential() %>% 
  layer_dense(units = 3, activation = "softmax")

# Compile model
model %>% 
  compile(
    loss = "categorical_crossentropy",
    metrics = "categorical_accuracy",
    optimizer = "sgd"
  )

# Fit model
model %>% 
  fit(
    x = matrix(X), 
    y = matrix(I),
    epochs = 250,
    validation_split = 0.0,
    verbose = 2
  )

alpha = get_weights(model)[[2]]
beta  = get_weights(model)[[1]]
alpha
beta

alpha_t = alpha + (1.5 - alpha[1])
beta_t  = beta + (1.0 - beta[1])
alpha_t
beta_t


##################################################
##### Question 4
##################################################

# Sample
set.seed(1)
N = 10

S = runif(N, 40, 60)
M = runif(N, 0.5, 1.5)
T = runif(N, 3/12, 2)
sig = runif(N, 0.1, 0.4)
r = runif(N, 0, 0.05)

X = cbind(S, M, T, sig, r) # features

# Data normalization
DataNorm = function(X){
  # normalize to [-1,+1]
  X[,1] = 2*(X[,1] - 50)/(60 - 40) # S
  X[,2] = 2*(X[,2] - 1)/(1.5 - 0.5) # M
  X[,3] = 2*(X[,3] - 1.125)/(2 - 3/12) # T
  X[,4] = 2*(X[,4] - 0.25)/(0.4 - 0.1) # sig
  X[,5] = 2*(X[,5] - 0.025)/(0.05 - 0) # r
  return(X)
}

X_norm = DataNorm(X)


##################################################
# (a)
##################################################

# Black-Scholes formula (binary)
BlackScholesBin = function(S, M, T, sig, r){
  # M = K/S
  
  d1 = (log(1/M) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  
  B = 50*exp(-r*T)*pnorm(d2)
  return(B)
}

B = matrix(BlackScholesBin(S, M, T, sig, r))

# Define model
model = keras_model_sequential() %>% 
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1)

# Compile model
model %>% 
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = "sgd"
  )

# Fit model
history = model %>% 
  fit(
    x = X_norm, 
    y = B,
    epochs = 100,
    validation_split = 0.0,
    verbose = 2
  )

summary(model)

# Plot model predictions
Np = 100

Sp = 50 + rep(0, Np)
Mp = seq(0.5, 1.5, length.out = Np)
Tp = 0.5 + rep(0, Np)
sigp = 0.2 + rep(0, Np)
rp = 0.02 + rep(0, Np)

Xp = cbind(Sp, Mp, Tp, sigp, rp)
Xp_norm = DataNorm(Xp)

Bp = BlackScholesBin(Sp, Mp, Tp, sigp, rp)
Yp = predict(model, Xp_norm)

ggplot() + 
  geom_line(aes(Mp, Bp, color = "C"), size = 1.5) + 
  geom_line(aes(Mp, Yp, color = "Chat"), size = 1.5) +
  scale_color_manual("",
                     breaks = c("C", "Chat"),
                     values = c("C" = "blue", "Chat" = "red")) +
  labs(x = "M", y = "C") +
  theme(text = element_text(size = 20))


##################################################
# (b)
##################################################

# Black-Scholes formula
BlackScholes = function(S, M, T, sig, r){
  # M = K/S
  
  d1 = (log(1/M) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  
  C = S*pnorm(d1) - S*M*exp(-r*T)*pnorm(d2)
  return(C)
}

C = BlackScholes(S, M, T, sig, r)

# Define model
model = keras_model_sequential() %>% 
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1)

# Compile model
model %>% 
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = "sgd"
  )

# Fit model
history = model %>% 
  fit(
    x = X_norm, 
    y = C,
    epochs = 100,
    validation_split = 0.0,
    verbose = 2
  )

summary(model)

# Plot model predictions
Np = 100

Sp = 50 + rep(0, Np)
Mp = seq(0.5, 1.5, length.out = Np)
Tp = 0.5 + rep(0, Np)
sigp = 0.2 + rep(0, Np)
rp = 0.02 + rep(0, Np)

Xp = cbind(Sp, Mp, Tp, sigp, rp)
Xp_norm = DataNorm(Xp)

Cp = BlackScholes(Sp, Mp, Tp, sigp, rp)
Yp = predict(model, Xp_norm)

ggplot() + 
  geom_line(aes(Mp, Cp, color = "C"), size = 1.5) + 
  geom_line(aes(Mp, Yp, color = "Chat"), size = 1.5) +
  scale_color_manual("",
                     breaks = c("C", "Chat"),
                     values = c("C" = "blue", "Chat" = "red")) +
  labs(x = "M", y = "C") +
  theme(text = element_text(size = 20))


##################################################
##### Question 5
##################################################

# Settings
N = 10000

gamma = 1
sigma = 0.5

x1 = rnorm(N)
x2 = gamma*x1 + sigma*rnorm(N)

beta = c(10,5,20,50,125)

f = beta[1]*x1 + beta[2]*x2 + beta[3]*x1^2 + beta[4]*x2^2 + beta[5]*x1*x2
f1 = beta[1] + 2*beta[3]*x1 + beta[5]*x2


##################################################
# (e)
##################################################

# Plot marginal effects
Ng = 101
x1g = seq(-3, +3, length.out=Ng)

f1g_PD = beta[1] + 2*beta[3]*x1g + beta[5]*gamma*0
f1g_MD = beta[1] + beta[2]*gamma + (2*beta[3] + 2*beta[4]*gamma^2 + 2*beta[5]*gamma)*x1g
f1g_LE = beta[1] + (2*beta[3] + beta[5]*gamma)*x1g

ggplot() + 
  geom_point(aes(x1,f1)) +
  geom_line(aes(x1g, f1g_PD, color = "PD"), size = 1.5) +
  geom_line(aes(x1g, f1g_MD, color = "MD"), size = 1.5) + 
  geom_line(aes(x1g, f1g_LE, color = "LE"), size = 1.5) + 
  xlim(-3, +3) + 
  scale_color_manual("",
                     breaks = c("PD", "MD", "LE"),
                     values = c("PD" = "blue", "MD" = "red", "LE" = "orange")) +
  labs(x = "x1", y = "df/dx1") +
  theme(text = element_text(size = 20))


# Plot accumulated effects
f0g_PD = beta[1]*x1g + 2*beta[3]*x1g*0.5*x1g + beta[5]*gamma*0*x1g
f0g_MD = beta[1]*x1g + beta[2]*gamma*x1g + 
  (2*beta[3] + 2*beta[4]*gamma^2 + 2*beta[5]*gamma)*x1g*0.5*x1g
f0g_LE = beta[1]*x1g + (2*beta[3] + beta[5]*gamma)*x1g*0.5*x1g

ggplot() + 
  geom_point(aes(x1,f)) +
  geom_line(aes(x1g, f0g_PD, color = "PD"), size = 1.5) +
  geom_line(aes(x1g, f0g_MD, color = "MD"), size = 1.5) + 
  geom_line(aes(x1g, f0g_LE, color = "LE"), size = 1.5) + 
  xlim(-3, +3) + 
  scale_color_manual("",
                     breaks = c("PD", "MD", "LE"),
                     values = c("PD" = "blue", "MD" = "red", "LE" = "orange")) +
  labs(x = "x1", y = "f") +
  theme(text = element_text(size = 20))


##################################################
# (f)
##################################################

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

# Plot marginal effects
Ng = 101
x1g = seq(-3, +3, length.out=Ng)

h = 0.1 # finite difference step size

f1g_PD = c()
f1g_MD = c()
f1g_LE = c()
for (i in 1:Ng) {
  f1g_PD[i] = (mean(beta[1]*(x1g[i]+h) + beta[2]*x2 + beta[3]*(x1g[i]+h)^2
                    + beta[4]*x2^2 + beta[5]*(x1g[i]+h)*x2) +
                 - mean(beta[1]*(x1g[i]-h) + beta[2]*x2 +
                          beta[3]*(x1g[i]-h)^2 + beta[4]*x2^2 + 
                          beta[5]*(x1g[i]-h)*x2))/(2*h)
  f1g_MD[i] = (KNN(matrix(x1), matrix(f), 100, matrix(x1g[i]+h)) +
                 - KNN(matrix(x1), matrix(f), 100, matrix(x1g[i]-h)))/(2*h)
  f1g_LE[i] = KNN(matrix(x1), matrix(f1), 100, matrix(x1g[i]))
}

ggplot() + 
  geom_point(aes(x1,f1)) +
  geom_line(aes(x1g, f1g_PD, color = "PD"), size = 1.5) +
  geom_line(aes(x1g, f1g_MD, color = "MD"), size = 1.5) + 
  geom_line(aes(x1g, f1g_LE, color = "LE"), size = 1.5) + 
  xlim(-3, +3) + 
  scale_color_manual("",
                     breaks = c("PD", "MD", "LE"),
                     values = c("PD" = "blue", "MD" = "red", "LE" = "orange")) +
  labs(x = "x1", y = "df/dx1") +
  theme(text = element_text(size = 20))

# Plot accumulated effects
f0g_PD = c()
f0g_MD = c()
f0g_LE = c()
for (i in 1:Ng) {
  f0g_PD[i] = mean(beta[1]*x1g[i] + beta[2]*x2 + beta[3]*x1g[i]^2 +
                     beta[4]*x2^2 + beta[5]*x1g[i]*x2)
  f0g_MD[i] = KNN(matrix(x1), matrix(f), 100, matrix(x1g[i]))
  f0g_LE[i] = sum(KNN(matrix(x1), matrix(f1), 100,
                      matrix(x1g[1:i]))*(c(0,diff(x1g[1:i])) 
                                         + c(diff(x1g[1:i]),0))/2)
}
f0g_LE = f0g_LE - f0g_LE[round(length(f0g_LE)/2)]

ggplot() + 
  geom_point(aes(x1,f)) +
  geom_line(aes(x1g, f0g_PD, color = "PD"), size = 1.5) +
  geom_line(aes(x1g, f0g_MD, color = "MD"), size = 1.5) + 
  geom_line(aes(x1g, f0g_LE, color = "LE"), size = 1.5) + 
  xlim(-3, +3) + 
  scale_color_manual("",
                     breaks = c("PD", "MD", "LE"),
                     values = c("PD" = "blue", "MD" = "red", 
                                "LE" = "orange")) +
  labs(x = "x1", y = "f") +
  theme(text = element_text(size = 20))
