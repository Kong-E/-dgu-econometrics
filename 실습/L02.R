# 02 - 인과효과

data <- read.table('data2018.csv', header=T, sep=","); attach(data)

# (1) No Control
m1 <- income[which(regular==1)]; n1 <- length(m1)
m0 <- income[which(regular==2)]; n0 <- length(m0)
theta <- mean(m1)-mean(m0)
round(theta, digit=2); c(n0,n1)

# (2) Control: edu
m1 <- income[which(regular==1 & edu==3)]; n1 <- length(m1)
m0 <- income[which(regular==2 & edu==3)]; n0 <- length(m0)
theta <- mean(m1)-mean(m0)
round(theta, digit=2); c(n0,n1)

# (3) Control: (edu, ind, size)
m1 <- income[which(regular==1 & edu==3 & ind=="C" & size==6)]; n1 <- length(m1)
m0 <- income[which(regular==2 & edu==3 & ind=="C" & size==6)]; n1 <- length(m0)
theta <- mean(m1)-mean(m0)
round(theta, digit=2); c(n0,n1)