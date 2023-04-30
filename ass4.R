rm(list = ls())

data <- read.table("data2018.csv", header=T, sep=",")
mydata <- subset(data, data$age<30 & data$ind=="C" & data$regular==1)
attach(mydata)
library(sandwich); library(lmtest); library(car)

educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6;
educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16;
educ[edu==6] <- 18; educ[edu==7] <- 21

exper <- age-educ-6
Y <- log(income)
X1 <- educ; X2 <- exper; X3 <- exper^2
M <- as.numeric(sex==1); MX2 <- M*X2; MX3 <- M*X3
S5 <- as.numeric(size==5); S6 <- as.numeric(size==6)
MS5 <- M*S5; MS6 <- M*S6

# 모형 A
fit <- lm(Y~X1+X2+X3+M+MX2+MX3)
summary(fit)
b <- coef(fit); b

# (1) H1: 급여에 성별격차가 존재한다.
theta <- b[5]*1+b[6]*X2+b[7]*X3
v = vcovHC(fit)
linearHypothesis(fit,v=v,c("M=0","MX2=0", "MX3=0"))
# 13.055 > 2.60(=F0.05(3,∞)) 이므로 대립가설을 받아들인다.

# (2) H1: 근로자의 경력이 로그급여에 미치는 영향은 선형이 아니다.
linearHypothesis(fit,v=v,c("X3=0","MX2=0","MX3=0"))
# 6.308 > 2.60(=F0.05(3,∞)) 이므로 대립가설을 받아들인다.

# 모형 B
fit <- lm(Y~X1+X2+M+S5+S6+MS5+MS6)
v = vcovHC(fit)
b <- coef(fit)

# (1) H1: 급여에 성별격차가 존재한다.
theta <- b[4]*1+b[7]*1*S5+b[8]*1*S6
linearHypothesis(fit,v=v,c("M=0","MS5=0","MS6=0"))
# 8.602 > 2.60(=F0.05(3,∞)) 이므로 대립가설을 받아들인다.

# (2) H1: 직장의 규모(=종사자 수)에 따라 급여에 차이가 존재한다.

# m=0인 경우
thetaS5 = b[5]; thetaS6 = b[7] # H0: thetaS5 = thetaS6 = 0
linearHypothesis(fit,v=v,c("S5=0","S6=0"))
# 12.669 > 3.00(=F0.05(2,∞)) 이므로 대립가설을 받아들인다.

# m=1인 경우
thetaS5 = b[5]+b[7]; thetaS6 = b[6]+b[8] # H0: thetaS5 = thetaS6 = 0
linearHypothesis(fit,v=v,c("S5+MS5=0","S6+MS6=0"))
# 31.074 > 3.00()(=F0.05(2,∞)) 이므로 대립가설을 받아들인다.


# (3) 직장의 규모에 따라 급여에 성별격차가 존재한다.

# (S5,S6)=(0,0)
# E(Y|educ,exper,M=1,S5=0,S6=0)-E(Y|educ,exper,M=0,S5=0,S6=0)
linearHypothesis(fit,v=v,c("M=0"))
# 12.261 > 3.84이므로 size < 5일 때 급여에 성별격차가 존재한다.
se <- sqrt(diag(v));se
(b[4]/se[4])^2

# (S5,S6)=(1,0)
# E(Y|educ,exper,M=1,S5=1,S6=0)-E(Y|educ,exper,M=0,S5=1,S6=0)
linearHypothesis(fit,v=v,c("M+MS5=0"))
# 1.1914 < 3.84이므로 size = 5일 때 급여에 성별격차가 존재하지 않는다.
theta0 <- b[4]+b[7]
vtheta <- diag(v)[4]+diag(v)[7]+2*v[4,7]
(theta0/sqrt(vtheta))^2

# (S5,S6)=(0,1)
# E(Y|educ,exper,M=1,S5=0,S6=1)-E(Y|educ,exper,M=0,S5=0,S6=1)
linearHypothesis(fit,v=v,c("M+MS6=0"))
# 12.303 > 3.84이므로 size = 6일 때 급여에 성별격차가 존재한다.