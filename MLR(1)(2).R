## (1)

data <- read.table("data2020.csv", header=T, sep=",")
attach(data)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

exper = age-educ-6
y <- log(income)
x1 <- educ; x2 <- exper
round(cor(data.frame(y,x1,x2)), digit = 3)

# Cross-tabulation approach
y <- log(income[which(exper==15)])
x <- educ[which(exper==15)]
length(y)
fit <- lm(y~x)
library(sandwich); library(lmtest)
round(coeftest(fit, v=vcovHC(fit)), digit=3)

# 다중선형회귀모형
y <- log(income)
x1 <- educ; x2 <- exper
fitM <- lm(y~x1+x2); fit <- lm(y~x1)
library(sandwich); library(lmtest)
round(coeftest(fitM, v=vcovHC(fitM)), digit=3)
round(coeftest(fit, v=vcovHC(fit)), digit = 3)

## (2)
library(scatterplot3d)
scatterplot3d(x1,x2,y,pch=1)

s3d <- scatterplot3d(x1,x2,y,pch=1)
s3d$plane3d(fitM, lty="dotted", col="blue")
