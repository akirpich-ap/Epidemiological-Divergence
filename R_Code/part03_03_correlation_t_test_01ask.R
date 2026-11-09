options(scipen = 999)

respiratory <- c(-0.27, -0.31, -0.29, -0.21, -0.21, -0.24, -0.24, -0.14)
hist(respiratory)
qqnorm(respiratory, main='Normal')
t.test(respiratory)



intestinal <- c(-0.03, 0.32, 0.52, 0.15, 0.28, 0.57, 0.09, 0.56)
hist(intestinal)
qqnorm(intestinal, main='Normal')
t.test(intestinal)
