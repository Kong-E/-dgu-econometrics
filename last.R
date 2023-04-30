rm(list = ls())

data <- read.table("data2020.csv", header=T, sep=",")
mydata <- subset(data, data$age<30 & data$edu==3 & data$ind=="C")
attach(mydata)
library(sandwich); library(lmtest); library(car)

Y <- income
X <- age
X2 <- age^2
R <- as.numeric(regular==1)
M <- as.numeric(sex==1)
RM <- R*M
RX <- R*X
RX2 <- R*X2
MX <- M*X
MX2 <- M*X2

# (A)
fit <- lm(Y~X+X2+R+M)
v = vcovHC(fit)
b <- round(coef(fit),3)
b[0]+b[1]*30+b[2]*(30^2)
round(coeftest(fit,v=v), digit=3)
summary(fit)
round(sqrt(diag(v)),3)
# linearHypothesis(fit,v=v,c("male=0"))

# (B)
fit <- lm(Y~X+X2+R+M+RM)
v = vcovHC(fit)
round(coeftest(fit,v=v), digit=3)
summary(fit)
round(sqrt(diag(v)),3)

# (C)
fit <- lm(Y~X+X2+R+M+RM+RX+RX2+MX+MX2)
v = vcovHC(fit)
round(coeftest(fit,v=v), digit=3)
summary(fit)
round(sqrt(diag(v)),3)
