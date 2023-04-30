data <- read.table(file.choose(), header=T, sep=",")
data1 <- subset(data, data$ind=="C"); attach(data1)
## 1

se <- function(x) sd(x)/sqrt(length(x))
x <- as.numeric(edu>3);

# (1)
y <- income
y1 <- y[which(x==1)]; y0 <- y[which(x==0)]
theta <- mean(y1)-mean(y0); setheta <- sqrt(se(y1)^2+se(y0)^2)
t <- (theta-80)/setheta; p <- pnorm(t, lower.tail=F)
round(c(t,p), digit=3)

# (2-1)
y <- as.numeric(regular==1)
y1 <- y[which(x==1)]; y0 <- y[which(x==0)]
p1 <- mean(y1); p0 <- mean(y0)
theta <- p1-p0
setheta <- sqrt(p1*(1-p1)/length(y1)+p0*(1-p0)/length(y0))
round(c(theta, setheta, theta-1.96*setheta, theta+1.96*setheta), digit=3)

# (2-2)
se0 <- sqrt(mean(y)*(1-mean(y)))*sqrt(1/length(y1)+1/length(y0)) # se for homoskedasticity
t <- theta/se0; p <- 2*pnorm(abs(t), lower.tail=F)
round(c(t,p), digit=3)

# (3)
y <- income
y1m <- y[which(x==1 & sex==1)]; y0m <- y[which(x==0 & sex==1)]
thetam <- mean(y1m)-mean(y0m); sem=sqrt(se(y1m)^2+se(y0m)^2)
y1f <- y[which(x==1 & sex==2)]; y0f=y[which(x==0 & sex==2)]
thetaf <- mean(y1f)-mean(y0f); sef=sqrt(se(y1f)^2+se(y0f)^2)
t <- (thetam-thetaf)/sqrt(sem^2+sef^2); 
p <- 2*pnorm(abs(t), lower.tail=F)
round(c(t,p), digit=3)

# (4)
y <- as.numeric(regular==1)
y1m <- y[which(x==1 & sex==1)]; p1m <- mean(y1m)
y0m <- y[which(x==0 & sex==1)]; p0m <- mean(y0m)
thetam <- p1m-p0m; sem <- sqrt(p1m*(1-p1m)/length(y1m)+p0m*(1-p0m)/length(y0m))

y1f <- y[which(x==1 & sex==2)]; p1f <- mean(y1f); v1f <-
  y0f <- y[which(x==0 & sex==2)]; p0f <- mean(y0f); v0f <-
  thetaf <- p1f-p0f; sef <- sqrt(p1f*(1-p1f)/length(y1f)+p0f*(1-p0f)/length(y0f))
t <- (thetam-thetaf)/sqrt(sem^2+sef^2); p <- 2*pnorm(abs(t), lower.tail=F)
round(c(t,p), digit=3)


## 2
educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6; educ[edu==2] <- 9; 
educ[edu==3] <- 12; educ[edu==4]<- 14; educ[edu==5] <- 16; educ[edu==6] <- 18; educ[edu==7] <- 21

# (1)
plot(educ,income)

# (2)
fit <- lm(income~educ)
bhat <- coef(fit)
R2 <- summary(fit)$r.squared
SER <- summary(fit)$sigma
round(c(bhat, SER, R2), digit=3)

# (3)
round(coef(lm(income~educ-1)), digit=3)