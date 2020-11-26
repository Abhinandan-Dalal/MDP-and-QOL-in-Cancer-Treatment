set.seed(1729)
library(MASS)
library(mvtnorm)
library(nnet)
library(actuar)

n= 1000


### Covariate simulation

age <- rnorm(n, mean = 50, sd = 3)
bp <- rmvnorm(1, age, 0.7*diag(n))
bp <- as.vector(bp) + 60
rdexp = rbinom(n, 1, 0.1)
hrm = rnorm(n, 700, 20)
Theta <- 10000 + 1000000*rpareto(n, 100, 10)

X<-cbind(ipt=rep(1, n), age, bp, rdexp, hrm, Theta)
X1<- matrix(nrow = nrow(X), ncol = ncol(X))
X1[ , c(1, 4)]=X[ , c(1, 4)]
for(i in c(2, 3, 5, 6)){
  X1[ ,i] = (X[ ,i]-mean(X[ ,i]))/sd(X[ ,i])
}
X = as.data.frame(X)


### Below are 6 models trained 
### on the basis of  all possible 3 states x 2 actions

B1<-matrix(nrow = 2, ncol = 6)
B1[ ,1]<-c(-0.5, 0.5)
B1[ ,2]<-0.15
B1[ ,3]<-0.8
B1[ ,4]<-0.45
B1[ ,5]<-0.53
B1[ ,6]<- -0.6
A1<-antilogit(X1%*%t(B1))
P1<-cbind(A1[ ,1], A1[ ,2] - A1[ ,1], 1 - A1[ ,2])

y = numeric(n)
for(i in  1:n){
  y[i] = sample(3, size = 1, prob = P1[i,])
}
resp = as.factor(y)
data1 = as.data.frame(cbind(X,resp))
data1$resp = as.factor(data1$resp)
data1$rdexp = as.factor(data1$rdexp)
m1 <- polr(resp ~ ., data = data1[,-1], Hess=TRUE)


B2<-matrix(nrow = 2, ncol = 6)
B2[ ,1]<-c(-0.2, 0.7)
B2[ ,2]<-0.2
B2[ ,3]<-1
B2[ ,4]<-0.2
B2[ ,5]<-0.6
B2[ ,6]<- -0.7
A2<-antilogit(X1%*%t(B2))
P2<-cbind(A2[ ,1], A2[ ,2] - A2[ ,1], 1 - A2[ ,2])

resp = numeric(n)
for(i in  1:n){
  y[i] = sample(3, size = 1, prob = P2[i,])
}
data2 = as.data.frame(cbind(X,resp = as.factor(y)))
data2$resp = as.factor(data2$resp)
data2$rdexp = as.factor(data2$rdexp)
m2 <- polr(resp ~ ., data = data2[,-1], Hess=TRUE)

B3<-matrix(nrow = 2, ncol = 6)
B3[ ,1]<-c(-0.45, 0.45)
B3[ ,2]<-0.2
B3[ ,3]<-0.75
B3[ ,4]<-0.51
B3[ ,5]<-0.62
B3[ ,6]<- -0.6
A3<-antilogit(X1%*%t(B3))
P3<-cbind(A3[ ,1], A3[ ,2] - A3[ ,1], 1 - A3[ ,2])

resp = numeric(n)
for(i in  1:n){
  resp[i] = sample(3, size = 1, prob = P3[i,])
}
data3 = as.data.frame(cbind(X,resp = as.factor(resp)))
data3$resp = as.factor(data3$resp)
data3$rdexp = as.factor(data3$rdexp)
m3 <- polr(resp ~ ., data = data3[,-1], Hess=TRUE)

B4<-matrix(nrow = 2, ncol = 6)
B4[ ,1]<-c(-0.35, 0.45)
B4[ ,2]<-0.15
B4[ ,3]<-0.9
B4[ ,4]<-0.38
B4[ ,5]<-0.6
B4[ ,6]<- -0.8
#View(antilogit(X1%*%t(B1)))
A4<-antilogit(X1%*%t(B4))
P4<-cbind(A4[ ,1], A4[ ,2] - A4[ ,1], 1 - A4[ ,2])

resp = numeric(n)
for(i in  1:n){
  resp[i] = sample(3, size = 1, prob = P4[i,])
}
data4 = as.data.frame(cbind(X,resp = as.factor(resp)))
data4$resp = as.factor(data4$resp)
data4$rdexp = as.factor(data4$rdexp)
m4 <- polr(resp ~ ., data = data4[,-1], Hess=TRUE)

B5<-matrix(nrow = 2, ncol = 6)
B5[ ,1]<-c(-0.38, 0.5)
B5[ ,2]<-0.2
B5[ ,3]<-1
B5[ ,4]<-0.4
B5[ ,5]<-0.52
B5[ ,6]<- -0.85
A5<-antilogit(X1%*%t(B5))
P5<-cbind(A5[ ,1], A5[ ,2] - A5[ ,1], 1 - A5[ ,2])

resp = numeric(n)
for(i in  1:n){
  resp[i] = sample(3, size = 1, prob = P5[i,])
}
data5 = as.data.frame(cbind(X,resp = as.factor(resp)))
data5$resp = as.factor(data5$resp)
data5$rdexp = as.factor(data5$rdexp)
m5 <- polr(resp ~ ., data = data5[,-1], Hess=TRUE)

B6<-matrix(nrow = 2, ncol = 6)
B6[ ,1]<-c(-0.3, 0.6)
B6[ ,2]<-0.2
B6[ ,3]<-0.75
B6[ ,4]<- 0.57
B6[ ,5]<- 0.5
B6[ ,6]<- -0.55
A6<-antilogit(X1%*%t(B6))
P6<-cbind(A6[ ,1], A6[ ,2] - A6[ ,1], 1 - A6[ ,2])

resp = numeric(n)
for(i in  1:n){
  resp[i] = sample(3, size = 1, prob = P6[i,])
}
data6 = as.data.frame(cbind(X,resp = as.factor(resp)))
data6$resp = as.factor(data6$resp)
data6$rdexp = as.factor(data6$rdexp)
m6 <- polr(resp ~ ., data = data6[,-1], Hess=TRUE)