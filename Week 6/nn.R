## Week 6
## Neural Networks and Deep Learning
library(keras)

set.seed(1)
N <- 100
X <- rnorm(N)
Y <- 0.5 + 2*X + 0.5*rnorm(N)

model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

model %>% 
  compile(
    loss = "mse",
    metrics = "mae",
    optimizer = "sgd"
  )

model %>% 
  fit(
    x = X,
    y = Y,
    epochs = N
  )

model <-keras_model_sequential()%>%
  layer_dense(units = 64, activation = "relu", input_shape =c(3))%>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 13, activation = "linear")

model%>%
  compile(optimizer = "rmsprop", loss = "mse")

x.test <- matrix(runif(3*100), ncol = 3)
y.test <- matrix(runif(13*100), ncol = 13)

history <- model%>%
  fit(x.test, y.test, epochs = 20, batch_size = 8, validation_split = 0.2)