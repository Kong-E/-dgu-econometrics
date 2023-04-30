data <- read.table("data2020.csv", header=T, sep=",")
mydata <- subset(data, data$ind=="C" & data$sex==2); attach(mydata)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

x <- educ; y <- income
n <- length(y); fit <- lm(y~x)
plot(x,y)

abline(h=mean(y), col="blue", lty=2)
abline(fit, col="red", lty=1)

ehat <- y-mean(y)
yhat <- fitted(fit) # b[1]+b[2]*x 와이햇
uhat <- resid(fit) # y-yhat

TSS <- crossprod(ehat)
RSS <- crossprod(uhat)
ESS <- crossprod(yhat-mean(y))
c(TSS, RSS, ESS)

SD <- sqrt(TSS/(n-1))
SER <- sqrt(RSS/(n-2)) # summary(fit)$sigma
R2 <- 1-RSS/TSS # summary(fit)$r.squared
round(c(SD,SER,R2),digit=3)

library(ggplot2)
g1 <- ggplot(data, aes(x=educ, y=income))+theme_bw()

# scatterplot
g1+geom_point(color='black', shape=16)+coord_cartesian(ylim=c(0,1000))

# BLP by OLS
g1+geom_point(color='black', shape=16)+
  geom_smooth(method=lm, se=F, color='red')+
  coord_cartesian(ylim=c(0,1000))

library(sandwich)
v <- vcovHC(fit); v
rse <- sqrt(diag(v)); rse
