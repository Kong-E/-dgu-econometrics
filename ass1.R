data <- read.table(file.choose(), header=T, sep=",")
mydata <- subset(data, data$age<30 & data$edu > 3); attach(mydata)
z1 <- qnorm(0.975); z2 <- qnorm(0.995)

# 1
m <- mean(income); se <- sd(income)/sqrt(length(income))
round(c(m-z1*se, m+z1*se), digit=3) # (1)
t <- (m-180)/se; p <- pnorm(t, lower.tail=F); 
round(c(t,p), digit=3) # (2)
t.test(income, mu=180, alternative="greater", 
       conf.level=0.95)
t <- (m-190)/se; p <- pnorm(t, lower.tail=T); 
round(c(t,p), digit=3) # (3)
t.test(income, mu=190, alternative="less", conf.level=0.95)
t <- (m-185)/se; p <- 2*pnorm(abs(t), lower.tail=F); 
round(c(t,p), digit=3) # (4)
t.test(income, mu=185, alternative="two.sided", 
       conf.level=0.95)

# 2
income1 <- income[which(sex==1)]
m <- mean(income1); se <- sd(income1)/sqrt(length(income1))
round(c(m-z2*se, m+z2*se), digit=3) # (1)
t <- (m-190)/se; p <- pnorm(t, lower.tail=F); 
round(c(t,p), digit=3) # (2)
t.test(income1, mu=190, alternative="greater", 
       conf.level=0.99)
p <- 2*pnorm(abs(t), lower.tail=F); 
round(c(t,p), digit=3) # (3)
t.test(income1, mu=190, alternative="two.sided", conf.level=0.99)

# 3
income2 <- income[which(sex==2)]
m <- mean(income2); se <- sd(income2)/sqrt(length(income2))
round(c(m-z1*se, m+z1*se), digit=3) # (1)
t <- (m-180)/se; p <- pnorm(t, lower.tail=T); 
round(c(t,p), digit=3) # (2)
t.test(income2, mu=180, alternative="less", conf.level=0.95)
p <- 2*pnorm(abs(t), lower.tail=F); 
round(c(t,p), digit=3) # (3)
t.test(income2, mu=180, alternative="two.sided", 
       conf.level=0.95)

# 4
ir <- as.numeric(regular==2)
phat <- mean(ir); se <- sqrt(phat*(1-phat)/length(ir))
round(c(phat-z1*se, phat+z1*se), digit=3) # (1)
t <- (phat-0.35)/se; p <- pnorm(t, lower.tail=F); 
round(c(t,p), digit=3) # (2)
t.test(ir, mu=0.35, alternative="greater", conf.level=0.95)
p <- 2*pnorm(abs(t), lower.tail=F); 
round(c(t,p), digit=3) # (3)
t.test(ir, mu=0.35, alternative="two.sided", conf.level=0.95)
