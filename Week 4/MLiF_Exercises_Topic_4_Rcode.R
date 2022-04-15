## Exercises Week 4
##
rm(list=ls())
library("scatterplot3d")
library(glmnet)

n.obs=300
X1=runif(n.obs)*5
X2=runif(n.obs)*5
Y=5*X1^2 + X1*X2 +rnorm(n.obs)
X=matrix(c(X1,X2), nrow=n.obs, byrow=FALSE)

##################################################
##### Question a    
##################################################
scatterplot3d(x=X1, y=X2, z=Y, pch=20, color="blue", angle=50) 
# this scatterplot shows the data Y is not linear in X1 and X2.

##################################################
##### Question b  
##################################################
scatterplot3d(x=X1^2, y=sqrt(2)*X2*X2, z=Y, pch=20, color="blue", angle=70)
# if we transform the variables, we find a linear relationship 
## between Y and the transformed features


##################################################
##### Question c
##################################################
# transformation from X to Z
Z1=X1^2
Z2=X2^2
Z3=sqrt(2)*X1*X2
Z=matrix(c(Z1,Z2,Z3), nrow=length(Z1),byrow=FALSE)

lambda=1
A=solve(t(Z)%*%Z+lambda*diag(3))
beta.ridge=(A%*%t(Z))%*%Y
beta.ridge

##################################################
##### Question d
##################################################
# Kernel trick
KK=(X%*%t(X))^2
# prediction
lambda=1
A=solve(KK+lambda*diag(n.obs))
alpha=A%*%Y

# transform the alpha values bak to beta values
beta1=sum(alpha*X1^2)
beta2=sum(alpha*X2^2)
beta3=sum(sqrt(2)*alpha*X1*X2)

# check that both problems yield the same beta values.
c(beta1, beta2, beta3)
beta.ridge