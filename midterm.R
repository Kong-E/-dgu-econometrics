data <- read.table("data2020.csv", header=T, sep=",")

data1 <- subset(data, data$ind=="F" & data$sex==1)
attach(data1)

## 1
m1 = as.numeric(income<=200)
p <- mean(m1)
se1 <- sqrt(p*(1-p)/length(m1))
c(p-1.96*se1, p+1.96*se1)

y = subset(data1, regular==2)
m2 = as.numeric(y$income<=200)
p2 <- mean(m2)
se2 <- sqrt(p2*(1-p2)/length(m2))
c(p2-1.96*se2, p2+1.96*se2)

## 2
# (A)
educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

y <- log(income); x <- educ
fit <- lm(y~x)
summary(fit)
library(sandwich); library(lmtest)
round(coeftest(fit,v=vcovHC(fit)),digit=3)

# (B)
col=as.numeric(edu>3)
x <- col
y <- income
fit <- lm(y~x)
summary(fit)
library(sandwich); library(lmtest)
round(coeftest(fit,v=vcovHC(fit)),digit=3)
