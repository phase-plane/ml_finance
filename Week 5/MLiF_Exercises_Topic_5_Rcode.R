rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(0)


##################################################
##### Question 1(d)    
##################################################

library("ggplot2")

p1 = seq(0, 1, length.out = 100)
E  = 1 - pmax(p1, 1-p1) # E(p1, 1-p1)

ggplot() + 
  geom_line(aes(p1, E), size = 1.5) +
  geom_point(aes(0.5, 1 - 1/2), size = 5, color = "orange") +
  geom_label(aes(x = 0.5+0.2, y = 1 - 1/2, label = "E = 1 - 1/K = 0.5"), 
             color = "orange", size = 5, fontface = "bold" )


##################################################
##### Question 2(d)    
##################################################

library("ggplot2")

p1 = seq(0, 1, length.out = 100)
G  = 1 - (p1^2 + (1-p1)^2) # G(p1, 1-p1)

ggplot() + 
  geom_line(aes(p1, G), size = 1.5) +
  geom_point(aes(0.5, 1 - 1/2), size = 5, color = "orange") +
  geom_label(aes(x = 0.5, y = 1 - 1/2 - 0.1, label = "G = 1 - 1/K = 0.5"),
             color = "orange", size = 5, fontface = "bold" )


##################################################
##### Question 3(e)    
##################################################

library("ggplot2")

p1 = seq(0, 1, length.out = 100)
D  = -(p1*log(p1) + (1-p1)*log(1-p1)) # D(p1, 1-p1)

ggplot() + 
  geom_line(aes(p1, D), size = 1.5) +
  geom_point(aes(0.5, log(2)), size = 5, color = "orange") +
  geom_label(aes(x = 0.5, y = log(2) - 0.1, 
                 label = sprintf("G = log(K) = %f", log(2))),
             color = "orange", size = 5, fontface = "bold" )


##################################################
##### Question 4
##################################################

library("rpart")
library("rpart.plot")
library("readxl")
library("gridExtra")

# Load data
# Variables: OverallQual, GrLivArea, SalePrice
data_train = read_excel("IOWA_Training_Data.xlsx")
data_test  = read_excel("IOWA_Test_Data.xlsx")

summary(data_train)
summary(data_test)


##################################################
# (a)
##################################################

p1 = ggplot() + geom_point(data = data_train, aes(OverallQual, SalePrice))
p2 = ggplot() + geom_point(data = data_train, aes(GrLivArea, SalePrice))

grid.arrange(p1,p2, nrow = 1)


##################################################
# (b)
##################################################

# Train tree
tree_house = rpart(formula = SalePrice ~ OverallQual, 
                   data = data_train, 
                   method = "anova",
                   control = rpart.control(maxdepth = 10, minsplit = 1, 
                                           minbucket = 1, xval = 5, cp = 0.01))

rpart.plot(tree_house)

yhat_train1 = predict(tree_house, data_train, type = "vector")
yhat_test1  = predict(tree_house, data_test, type = "vector")

p1 = ggplot() +
  geom_point(data = data_train, aes(OverallQual, SalePrice), color = "blue") +
  geom_point(data = data_train, aes(OverallQual, yhat_train1), color = "red")


# Train tree
tree_house = rpart(formula = SalePrice ~ GrLivArea, 
                   data = data_train, 
                   method = "anova",
                   control = rpart.control(maxdepth = 3, minsplit = 1, 
                                           minbucket = 1, xval = 5, cp = 0))

rpart.plot(tree_house)

yhat_train2 = predict(tree_house, data_train, type = "vector")
yhat_test2  = predict(tree_house, data_test, type = "vector")

p2 = ggplot() +
  geom_point(data = data_train, aes(GrLivArea, SalePrice), color = "blue") +
  geom_point(data = data_train, aes(GrLivArea, yhat_train2), color = "red")

grid.arrange(p1,p2, nrow = 1)


##################################################
# (c)
##################################################

# Train tree
tree_house = rpart(formula = SalePrice ~ OverallQual + GrLivArea, 
                   data = data_train, 
                   method = "anova",
                   control = rpart.control(maxdepth = 10, minsplit = 1, 
                                           minbucket = 1, xval = 5, cp = 0.01))

rpart.plot(tree_house)

yhat_train = predict(tree_house, data_train, type = "vector")
yhat_test  = predict(tree_house, data_test, type = "vector")

p1 = ggplot() +
  geom_point(data = data_train, aes(OverallQual, SalePrice), color = "blue") +
  geom_point(data = data_train, aes(OverallQual, yhat_train), color = "red")
p2 = ggplot() +
  geom_point(data = data_train, aes(GrLivArea, SalePrice), color = "blue") +
  geom_point(data = data_train, aes(GrLivArea, yhat_train), color = "red")

grid.arrange(p1,p2, nrow = 1)


##################################################
##### Question 5    
##################################################

#library ("tree")
library("rpart")
library("rpart.plot")
library("readxl")
library("gridExtra")

# Load data
# Variables: home_ownership, income, dti, fico_low, loan_status
data_train = read_excel("lendingclub_traindata.xlsx")
data_test  = read_excel("lendingclub_testdata.xlsx")

summary(data_train)
summary(data_test)


##################################################
# (a)
##################################################

p1 = ggplot() + geom_count(data = data_train, aes(home_ownership, loan_status))
p2 = ggplot() + geom_count(data = data_train, aes(log(income), loan_status))
p3 = ggplot() + geom_count(data = data_train, aes(log(1+dti), loan_status))
p4 = ggplot() + geom_count(data = data_train, aes(fico_low, loan_status))

grid.arrange(p1,p2,p3,p4, nrow = 2)


##################################################
# (b)
##################################################

# Train tree
tree_lending = rpart(formula = loan_status ~ fico_low,
                     data = data_train,
                     method = "anova",
                     control = rpart.control(maxdepth = 4, minsplit = 1, 
                                             minbucket = 10, xval = 1, cp = 0))

rpart.plot(tree_lending)

yhat_train1 = predict(tree_lending, data_train, type = "vector")
yhat_test1  = predict(tree_lending, data_test, type = "vector")

p1 = ggplot() +
  geom_point(data = data_train, aes(fico_low, loan_status), color = "blue") +
  geom_point(data = data_train, aes(fico_low, yhat_train1), color = "red")


# Train tree
tree_lending = rpart(formula = loan_status ~ dti,
                     data = data_train,
                     method = "anova",
                     control = rpart.control(maxdepth = 4, minsplit = 1, 
                                             minbucket = 10, xval = 1, cp = 0))

rpart.plot(tree_lending)

yhat_train2 = predict(tree_lending, data_train, type = "vector")
yhat_test2  = predict(tree_lending, data_test, type = "vector")

p2 = ggplot() +
  geom_point(data = data_train, aes(log(1+dti), loan_status), color = "blue") +
  geom_point(data = data_train, aes(log(1+dti), yhat_train2), color = "red")

grid.arrange(p1,p2, nrow = 1)


# Train tree
tree_lending = rpart(formula = loan_status ~ home_ownership + income + dti + fico_low, 
                     data = data_train, 
                     method = "anova",
                     control = rpart.control(maxdepth = 4, minsplit = 1,
                                             minbucket = 10, xval = 1, cp = 0))

rpart.plot(tree_lending)

yhat_train = predict(tree_lending, data_train, type = "vector")
yhat_test  = predict(tree_lending, data_test, type = "vector")

p1 = ggplot() +
  geom_point(data = data_train, aes(fico_low, loan_status), color = "blue") +
  geom_point(data = data_train, aes(fico_low, yhat_train), color = "red")
p2 = ggplot() +
  geom_point(data = data_train, aes(log(1+dti), loan_status), color = "blue") +
  geom_point(data = data_train, aes(log(1+dti), yhat_train), color = "red")

grid.arrange(p1,p2, nrow = 1)


##################################################
# (c)
##################################################

# Train tree
tree_lending = rpart(formula = loan_status ~ fico_low,
                     data = data_train,
                     method = "class",
                     control = rpart.control(maxdepth = 4, minsplit = 1, 
                                             minbucket = 10, xval = 1, cp = 0))

rpart.plot(tree_lending)

yhat_train1 = predict(tree_lending, data_train, type = "class")
yhat_test1  = predict(tree_lending, data_test, type = "class")

p1 = ggplot() +
  geom_point(data = data_train, aes(fico_low, loan_status), color = "blue") +
  geom_point(data = data_train, aes(fico_low, as.numeric(paste(yhat_train1))-0.005), 
             color = "red")


# Train tree
tree_lending = rpart(formula = loan_status ~ dti,
                     data = data_train,
                     method = "class",
                     control = rpart.control(maxdepth = 4, minsplit = 1,
                                             minbucket = 10, xval = 1, cp = 0))

rpart.plot(tree_lending)

yhat_train2 = predict(tree_lending, data_train, type = "class")
yhat_test2  = predict(tree_lending, data_test, type = "class")

p2 = ggplot() +
  geom_point(data = data_train, aes(log(1+dti), loan_status), color = "blue") +
  geom_point(data = data_train, aes(log(1+dti), 
                                    as.numeric(paste(yhat_train2))-0.005), 
             color = "red")

grid.arrange(p1,p2, nrow = 1)


# Train tree
tree_lending = rpart(formula = loan_status ~ home_ownership + income + dti + fico_low, 
                     data = data_train, 
                     method = "class",
                     control = rpart.control(maxdepth = 4, minsplit = 1, 
                                             minbucket = 10, xval = 1, cp = 0))

rpart.plot(tree_lending)

yhat_train = predict(tree_lending, data_train, type = "class")
yhat_test  = predict(tree_lending, data_test, type = "class")

p1 = ggplot() +
  geom_point(data = data_train, aes(fico_low, loan_status), color = "blue") +
  geom_point(data = data_train, aes(fico_low, as.numeric(paste(yhat_train))-0.005), 
             color = "red")
p2 = ggplot() +
  geom_point(data = data_train, aes(log(1+dti), loan_status), color = "blue") +
  geom_point(data = data_train, aes(log(1+dti), as.numeric(paste(yhat_train))-0.005),
             color = "red")

grid.arrange(p1,p2, nrow = 1)

