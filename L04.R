# 단순선형회귀모형(1)
# Homoskedasticity Only SE
var <- vcov(fit); se <- sqrt(diag(var))
round(cbind(bhat, se), digit=3)
se[2]

# Robust SE
library(sandwich)
v <- vcovHC(fit)
rse <- sqrt(diag(v))
round(cbind(bhat,rse), digit=3)

# 단순선형회귀모형(2)
## [실증 4.1] CSLRM 가정
data <- read.table("data2020.csv", header=T, sep=",")
attach(data)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

fit <- lm(income~educ)
summary(fit)

c(22.722-1.96*se[2], 22.722+1.96*se[2])

## [실증 4.2] CSLRM 가정
r = as.numeric(regular==1)
fit <- lm(income~r)
summary(fit)

## [실증 4.1] SLRM 가정
educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

y <- income; x<- educ
fit <- lm(y~x)

library(sandwich); library(lmtest)
# 동분산 only SE (CSLRM)
round(coeftest(fit, v=vcovHC(fit, type="const")), digits = 3)
# Robust SE (SLRM)
round(coeftest(fit,v=vcovHC(fit)),digits = 3)

## [실증 4.3] SLRM 가정
data1 <- subset(data, data$ind=="C" & data$age < 30 & data$sex==1)
attach(data1)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

y <- income; x <- educ
fit <- lm(y~x)

# Scatterplot & SLR
plot(x, y, pch=16, xlab = "", ylab = "")
abline(fit, col="red", lwd=2)

# Robust SE
library(sandwich); library(lmtest)
round(coeftest(fit,v=vcovHC(fit)),digits = 3)

# 단순선형회귀모형(3)
## 예1)
data <- read.table("data2020.csv", header=T, sep=","); attach(data)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

y <- log(income); x <- educ
fit <- lm(y~x)
plot(x,y,pch=16,xlab="",ylab="");abline(fit,col="red",lwd=2)

## 예2)
data1 <- subset(data, data$age<30); attach(data1)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

y <- log(income); x <- educ
fit <- lm(y~x)
library(sandwich); v <- vcovHC(fit)
library(lmtest); round(coeftest(fit,v=v),digit=3)

## 예1)
data <- read.table("data2020.csv", header=T, sep=",")

y <- income; x <- as.numeric(edu>3)
fit <- lm(y~x)

library(sandwich); library(lmtest)
round(coeftest(fit,v=vcovHC(fit)),digit=3)

y1 = y[which(x==1)]; y0 = y[which(x==0)]
theta = mean(y1)-mean(y0); se1 = sqrt((sd(y1)/sqrt(length(y1)))^2+(sd(y0)/sqrt(length(y0)))^2)
t = theta/se1
round(c(theta, se1, t), digit=3)

t.test(y~x, var.equal=F)

## 예2)
data1 <- subset(data, data$ind=="C"); attach(data1)

se=function(x) sd(x)/sqrt(length(x))
x=as.numeric(edu>3); y=income

fit <- lm(y~x)
library(sandwich); library(lmtest)
round(coeftest(fit,v=vcovHC(fit)),digit=3)

y1 = y[which(x==1)]; y0=y[which(x==0)]
theta = mean(y1)-mean(y0); se1 = sqrt(se(y1)^2+se(y0)^2)
t=theta/se1
round(c(theta,se1,t),digit=3)

## 예3
data <- read.table("data2020.csv", header=T, sep=",")
attach(data)

x <- as.numeric(regular==2); y <- income
t.test(y~x, var.equal=F)

library(sandwich); library(lmtest)
fit <- lm(y~x)
round(coeftest(fit, v=vcovHC(fit, type="HC1")), digit=3)