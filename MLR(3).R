rm(list = ls())

## (3)

data <- read.table("data2020.csv", header=T, sep=",")
data1 <- subset(data, data$ind=="C" & data$age<30 & data$sex==1)
attach(data1)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

exper <- age-educ-6
y <- log(income); x1 <- educ; x2 <- exper; x3 <- x2^2
n <- length(y)

fit <- lm(y~x1+x2+x3)
library(sandwich); library(lmtest)
round(coeftest(fit, v=vcovHC(fit)), digit=3)

sum <- summary(fit) 
R2 <- sum$r.squared; AdjR2 <- sum$adj.r.squared; ser <- sum$sigma
round(c(R2, AdjR2, ser), digit=3)

## (4)
# Short regression
fit <- lm(y~x1)
round(coeftest(fit, v=vcovHC(fit)), digit=3)

# Long regression
fitM <- lm(y~x1+x2)
round(coeftest(fitM,v=vcovHC(fitM)), digit=3)

# Omitted variable bias
round(cor(data.frame(y,x1,x2)), digit=3)

aug <- lm(x2~x1)
round(coeftest(aug,v=vcovHC(aug)), digit=3)

# VIF
library(car)
vif(fitM) # Variance Inflation Factor

r2 <- summary(aug)$r.squared
r2
1/(1-r2)
