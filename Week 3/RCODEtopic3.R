# Draw hyperplane
rm(list=ls())
library("scatterplot3d") # load
n=500
x1=seq(0,10,0.5)
x2=seq(0,10,0.5)
beta0=0
beta1=2
beta2=4
y=matrix(,length(x1),length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    y[i,j]=beta0+beta1*x1[i]+beta2*x2[j]
  }
}
persp(x1,x2,y,theta = 40, phi = 20)

### Term Life Insurance
rm(list=ls())
Data_full=read.csv("TermLife.csv")
head(Data_full)
dim(Data_full)
I=Data_full$FACE>0
Data=Data_full[I,]
Data$LNFACE=log(Data$FACE)
Data$LNINCOME=log(Data$INCOME)


# Density of FACE and LNFACE
par(mfrow=c(1,2))
plot(density(Data$FACE)$x,density(Data$FACE)$y, type='l',col="blue")
plot(density(Data$LNFACE)$x,density(Data$LNFACE)$y, type='l',col="blue")

# Relation of LNFACE with INCOME and LNINCOME
par(mfrow=c(1,2))
plot(Data$INCOME, Data$LNFACE, col="blue", pch=20)
plot(Data$LNINCOME, Data$LNFACE,col="blue", pch=20)


# Investigate dependencies between the variables
library("scatterplot3d") # load
library("akima")
pairs(~NUMHH + EDUCATION + LNINCOME + LNFACE, Data, upper.panel=NULL)
cor(data.frame(Data$NUMHH, Data$EDUCATION, Data$LNINCOME, Data$LNFACE ))
scatterplot3d(Data$EDUCATION, Data$NUMHH, Data$LNFACE)

I2=Data$NUMHH==2
I6=Data$NUMHH==6
I4=Data$NUMHH==4
par(mfrow=c(2,2))
plot(Data$EDUCATION[I2],Data$LNFACE[I2],xlim=c(8,20), ylim=c(5,20), main="NUMHH=2")
plot(Data$EDUCATION[I6],Data$LNFACE[I6],xlim=c(8,20), ylim=c(5,20),main="NUMHH=6")
plot(Data$EDUCATION[I4],Data$LNFACE[I4],xlim=c(8,20), ylim=c(5,20),main="NUMHH=4")
plot(Data$EDUCATION,Data$LNFACE,xlim=c(8,20), ylim=c(5,20),main="Full Data")

# Correlations, given NUMHH
cor(Data$EDUCATION[I2],Data$LNFACE[I2])
cor(Data$EDUCATION[I6],Data$LNFACE[I6])
cor(Data$EDUCATION[I4],Data$LNFACE[I4])

# linear regression model
TermLife.LS=lm(LNFACE~NUMHH + EDUCATION + LNINCOME, Data)
summary(TermLife.LS)


# effect of Education on the Face value
x_2=seq(8,16.5,0.1)
n_test=length(x_2)
TestData=data.frame(NUMHH= c(mean(Data$NUMHH)*rep(1,n_test)),EDUCATION=x_2,LNINCOME=c(mean(Data$LNINCOME)*rep(1,n_test))) 
yhat=predict(TermLife.LS,TestData)
Face=exp(yhat)
#Percentage change
diff(Face)/Face[1:(n_test-1)]

# beta coefficient of EDUCATION
TermLife.LS$coefficients[3]
par(mfrow=c(1,2))
plot(x_2,yhat,type='l', xlab="EDUCATION", ylab="LNFACE", col="blue")
plot(x_2,Face,type='l', xlab="EDUCATION", ylab="FACE", col="blue")

# Adding a useless response variable
set.seed(1)
Data$U1=rnorm(length(Data$LNFACE))
Data$U2=rnorm(length(Data$LNFACE))
Data$U3=rnorm(length(Data$LNFACE))
Data$U4=rnorm(length(Data$LNFACE))
Data$U5=rnorm(length(Data$LNFACE))
Data$U6=rnorm(length(Data$LNFACE))
Data$U7=rnorm(length(Data$LNFACE))
Data$U8=rnorm(length(Data$LNFACE))
Data$U9=rnorm(length(Data$LNFACE))
Data$U10=rnorm(length(Data$LNFACE))

TermLife.LS_Useless=lm(LNFACE~NUMHH + EDUCATION + LNINCOME+ U1
                        + U2 + U3 + U4+ U5
                       + U6 + U7 + U8+ U9 +U10, Data)
summary(TermLife.LS_Useless)

anova(TermLife.LS)



# Refrigerator example
rm(list=ls())
Data=read.csv("Refrigerator.csv")
head(Data)
summary(Data$PRICE)
summary(Data$ECOST)
summary(Data$RSIZE)
summary(Data$FSIZE)
summary(Data$SHELVES)
summary(Data$FEATURES)
n=length(Data$PRICE)
# Pairwise Correlations
cor(matrix(c(Data$ECOST,Data$RSIZE,Data$FSIZE,Data$SHELVES,Data$FEATURES, Data$PRICE),n,6)) 

# effect of ECOST on PRICE
plot(Data$ECOST,Data$PRICE)
cor(Data$ECOST,Data$PRICE)

# added variable plot
PRICE_1=lm(PRICE~RSIZE + FSIZE + SHELVES + FEATURES, Data)
E1=summary(PRICE_1)$residuals
ECOST_1=lm(ECOST ~ RSIZE + FSIZE + SHELVES + FEATURES, Data)
E2=summary(ECOST_1)$residuals
plot(E1,E2)
cor(E1,E2)


# Linear Model
summary(lm(PRICE~RSIZE + FSIZE + SHELVES + FEATURES + ECOST, Data))


# categorical variables
rm(list=ls())

Data_full=read.csv("TermLife.csv")
I=Data_full$FACE>0
Data=Data_full[I,]
Data$LNFACE=log(Data$FACE)
Data$LNINCOME=log(Data$INCOME)

# SINGLE as a numeric variable 
Data$SINGLE=as.numeric(Data$MARSTAT==0)
TermLife.Single.LS=lm(LNFACE ~ LNINCOME + SINGLE, Data)
yhat=predict(TermLife.Single.LS)
plot(Data$LNINCOME,yhat)


# MARStat as a quantitative variable
TermLife.MarStat_Q.LS=lm(LNFACE ~ LNINCOME + MARSTAT, Data)
yhat=predict(TermLife.MarStat_Q.LS)
plot(Data$LNINCOME,yhat)


# MARStat as a factor variable
Data$MARSTAT_C=as.factor(Data$MARSTAT)
TermLife.MarStat_C.LS=lm(LNFACE ~ LNINCOME + MARSTAT_C, Data)
yhat=predict(TermLife.MarStat_C.LS)
plot(Data$LNINCOME,yhat, col=c("red","blue", "green")[Data$MARSTAT_C])



