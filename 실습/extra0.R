data <- read.table('data2020.csv', header=T, sep=",");
data1 <- subset(data, data$ind=="C"); attach(data1)

#2-1
y <- income
n <- length(y)
m <- mean(y)
sem <- sd(y)/sqrt(n)
round(c(m-1.96*sem, m+1.96*sem), digit=2)
t.test(y, mu=250, alternative = "greater") #귀무가설 기각

#2-2
educ <- NULL
educ[edu==0] <- 4; educ[edu==1] <- 6; educ[edu==2] <- 9; educ[edu==3] <- 12;
educ[edu==4] <- 14; educ[edu==5] <- 16; educ[edu==6] <- 18; educ[edu==7] <- 21;
exper <- age - educ - 6
mydata <- data.frame(data1,educ,exper); attach(mydata)

y <- exper
n <- length(y)
m <- mean(y)
sem <- sd(y)/sqrt(n)
round(c(m-2.58*sem, m+2.58*sem), digit=2)
t.test(y, mu=28, alternative = "less", conf.level = 0.99) #귀무가설 기각

#2-3
y <- as.numeric(regular==1)
n <- length(y)
m <- mean(y)
sem <- sd(y)/sqrt(n)
round(c(m-1.645*sem, m+1.645*sem), digit=2)
t.test(y, mu=0.6, alternative = "two.sided") #기각