data <- read.table("data2018.csv", header=T, sep=",")
mydata <- subset(data, data$age<30 & data$edu > 3); attach(mydata)
z1 <- qnorm(0.975); z2 <- qnorm(0.995)

# 1번문제
#1
m <- mean(income); se <- sd(income)/sqrt(length(income))
round(c(m-z1*se, m+z1*se), digit=3)
#2
t <- (m-180)/se; p <- pnorm(t, lower.tail=F); round(c(t,p), digit=3)
t.test(income, mu=180, alternative="greater", conf.level=0.95)
#3
t <- (m-190)/se; p <- pnorm(t, lower.tail=T); round(c(t,p), digit=3)
t.test(income, mu=190, alternative="less", conf.level=0.95)
#4
t <- (m-185)/se; p <- 2*pnorm(abs(t), lower.tail=F); round(c(t,p), digit=3)
t.test(income, mu=185, alternative="two.sided", conf.level=0.95)

# 2번문제
#1
income1 <- income[which(sex==1)]
m <- mean(income1); se <- sd(income1)/sqrt(length(income1))
round(c(m-z2*se, m+z2*se), digit=3)
#2
t <- (m-190)/se; p <- pnorm(t, lower.tail=F); round(c(t,p), digit=3)
t.test(income1, mu=190, alternative="greater", conf.level=0.99)
#3
p <- 2*pnorm(abs(t), lower.tail=F); round(c(t,p), digit=3)
t.test(income1, mu=190, alternative="two.sided", conf.level=0.99)

# 3번문제
#1
income2 <- income[which(sex==2)]
m <- mean(income2); se <- sd(income2)/sqrt(length(income2))
round(c(m-z1*se, m+z1*se), digit=3)
#2
t <- (m-180)/se; p <- pnorm(t, lower.tail=T); round(c(t,p), digit=3)
t.test(income2, mu=180, alternative="less", conf.level=0.95)
#3
p <- 2*pnorm(abs(t), lower.tail=F); round(c(t,p), digit=3)
t.test(income2, mu=180, alternative="two.sided", conf.level=0.95)

# 4번문제
#1
r2 <- as.numeric(regular==2);r2
phat <- mean(r2); se <- sqrt(phat*(1-phat)/length(r2))
round(c(phat-z1*se, phat+z1*se), digit=3)
#2
t <- (phat-0.35)/se; p <- pnorm(t, lower.tail=F); round(c(t,p), digit=3)
t.test(r2, mu=0.35, alternative="greater", conf.level=0.95)
#3
p <- 2*pnorm(abs(t), lower.tail=F); round(c(t,p), digit=3)
t.test(r2, mu=0.35, alternative="two.sided", conf.level=0.95)