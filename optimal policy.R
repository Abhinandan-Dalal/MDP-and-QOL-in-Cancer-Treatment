#Number of States=3, No of Covariates=4
#Initial State: 1
#-------------------------

library(MASS)
library(mvtnorm)
library(nnet)

antilogit=function(x){
  return(1/(1+exp(-x)))
}

logit=function(x){
  return(log(x/(1-x)))
}

optact=function(M1,M2,N,theta,g){
  
  reward=function(s,i,t,g){
    R=g*(s-i)*(3-s^(theta))/((t+1)^2)
    return(log(1+abs(R))*sign(R))    #constant parameter is 3
  }
  
  #reward(4,5,12)#example
  
  final_reward=function(s,N,g){
    R=g*(3-s^(theta))/(N+1)^2
    return(log(1+abs(R))*sign(R))
  }
  #final_reward(3,N)#example
  exp_reward=function(P,s,t,g){
    sum=0
    for(i in 1:3)
      sum=sum+P[s,i]*reward(s,i,t,g)
    return(sum)
  }
  #exp_reward(M1,2,6)#example
  #M_t,j=u_t*(j) solved from Bellman's equations
  U=matrix(data=0,nrow=N,ncol=3)
  D=matrix(data=0,nrow=N-1,ncol=3)
  a=b=0
  for(i in 1:3)
    U[N,i]=final_reward(i,N,g)
  for(t in 1:N-1){
    for(i in 1:3){
      a<-exp_reward(M1,i,t,g)+sum(U[N-t,]*M1[i,])
      b<-exp_reward(M2,i,t,g)+sum(U[N-t,]*M2[i,])
      U[N-t-1,i]=max(a,b)
      D[N-t-1,i]=which.max(c(a,b))
    }
  }
  return(D)
}
#optact(M1, M2, 8, 10, 0.6)

#---------------------------

V = list()
for(th in 1:20)
{
  V[[th]] = matrix (0, nrow = 7, ncol = 3)
  for(n in 1:30)
  {
    #age <- rnorm(500, 65, 2)
    age <- runif(500, 60, 70)
    bp <- rmvnorm(1, age, 0.7*diag(500))
    bp <- as.vector(bp) + 60
    rdexp = rbinom(500, 1, 0.1)
    hrm = rnorm(500, 700, 20)
    
    #---------------------
    #LStyle is the degree of lifestyle e.g. smoking, drug consumption etc.
    #LStyle=sample(1:10, size=50, replace=TRUE)
    #library(mvtnorm)
    #age=as.vector(rmvnorm(1, mean=21-LStyle, sigma=5*diag(50)))
    #age=floor(age)
    #cor(age, LStyle)
    #plot(LStyle,age)
    
    X<-cbind(ipt=rep(1, 500), age, bp, rdexp, hrm)
    X1<- matrix(nrow = nrow(X), ncol = ncol(X))
    X1[ , c(1, 4)]=X[ , c(1, 4)]
    for(i in c(2, 3, 5)){
      X1[ ,i] = (X[ ,i]-mean(X[ ,i]))/sd(X[ ,i])
    }
    X = as.data.frame(X)
    
    #model<-polr(res~., data = data1[,-1])
    #model
    #model$coefficients
    
    B1<-matrix(nrow = 2, ncol = 5)
    B1[ ,1]<-c(-0.5, 0.5)
    B1[ ,2]<-0.15
    B1[ ,3]<-0.8
    B1[ ,4]<-0.45
    B1[ ,5]<-0.53
    #View(antilogit(X1%*%t(B1)))
    A1<-antilogit(X1%*%t(B1))
    P1<-cbind(A1[ ,1], A1[ ,2] - A1[ ,1], 1 - A1[ ,2])
    
    y = numeric(500)
    for(i in  1:500){
      y[i] = sample(3, size = 1, prob = P1[i,])
    }
    resp = as.factor(y)
    data1 = as.data.frame(cbind(X,resp))
    data1$resp = as.factor(data1$resp)
    data1$rdexp = as.factor(data1$rdexp)
    m1 <- polr(resp ~ ., data = data1[,-1], Hess=TRUE)
    
    
    B2<-matrix(nrow = 2, ncol = 5)
    B2[ ,1]<-c(-0.2, 0.7)
    B2[ ,2]<-0.2
    B2[ ,3]<-1
    B2[ ,4]<-0.2
    B2[ ,5]<-0.6
    
    #View(antilogit(X1%*%t(B1)))
    A2<-antilogit(X1%*%t(B2))
    P2<-cbind(A2[ ,1], A2[ ,2] - A2[ ,1], 1 - A2[ ,2])
    
    resp = numeric(500)
    for(i in  1:500){
      y[i] = sample(3, size = 1, prob = P2[i,])
    }
    data2 = as.data.frame(cbind(X,resp = as.factor(y)))
    data2$resp = as.factor(data2$resp)
    data2$rdexp = as.factor(data2$rdexp)
    m2 <- polr(resp ~ ., data = data2[,-1], Hess=TRUE)
    
    B3<-matrix(nrow = 2, ncol = 5)
    B3[ ,1]<-c(-0.45, 0.45)
    B3[ ,2]<-0.2
    B3[ ,3]<-0.75
    B3[ ,4]<-0.51
    B3[ ,5]<-0.62
    
    #View(antilogit(X1%*%t(B1)))
    A3<-antilogit(X1%*%t(B3))
    P3<-cbind(A3[ ,1], A3[ ,2] - A3[ ,1], 1 - A3[ ,2])
    
    resp = numeric(500)
    for(i in  1:500){
      resp[i] = sample(3, size = 1, prob = P3[i,])
    }
    data3 = as.data.frame(cbind(X,resp = as.factor(resp)))
    data3$resp = as.factor(data3$resp)
    data3$rdexp = as.factor(data3$rdexp)
    m3 <- polr(resp ~ ., data = data3[,-1], Hess=TRUE)
    
    B4<-matrix(nrow = 2, ncol = 5)
    B4[ ,1]<-c(-0.35, 0.45)
    B4[ ,2]<-0.15
    B4[ ,3]<-0.9
    B4[ ,4]<-0.38
    B4[ ,5]<-0.6
    
    #View(antilogit(X1%*%t(B1)))
    A4<-antilogit(X1%*%t(B4))
    P4<-cbind(A4[ ,1], A4[ ,2] - A4[ ,1], 1 - A4[ ,2])
    
    resp = numeric(500)
    for(i in  1:500){
      resp[i] = sample(3, size = 1, prob = P4[i,])
    }
    data4 = as.data.frame(cbind(X,resp = as.factor(resp)))
    data4$resp = as.factor(data4$resp)
    data4$rdexp = as.factor(data4$rdexp)
    m4 <- polr(resp ~ ., data = data4[,-1], Hess=TRUE)
    
    B5<-matrix(nrow = 2, ncol = 5)
    B5[ ,1]<-c(-0.38, 0.5)
    B5[ ,2]<-0.2
    B5[ ,3]<-1
    B5[ ,4]<-0.4
    B5[ ,5]<-0.52
    
    #View(antilogit(X1%*%t(B1)))
    A5<-antilogit(X1%*%t(B5))
    P5<-cbind(A5[ ,1], A5[ ,2] - A5[ ,1], 1 - A5[ ,2])
    
    resp = numeric(500)
    for(i in  1:500){
      resp[i] = sample(3, size = 1, prob = P5[i,])
    }
    data5 = as.data.frame(cbind(X,resp = as.factor(resp)))
    data5$resp = as.factor(data5$resp)
    data5$rdexp = as.factor(data5$rdexp)
    m5 <- polr(resp ~ ., data = data5[,-1], Hess=TRUE)
    
    B6<-matrix(nrow = 2, ncol = 5)
    B6[ ,1]<-c(-0.3, 0.6)
    B6[ ,2]<-0.2
    B6[ ,3]<-0.75
    B6[ ,4]<- 0.57
    B6[ ,5]<- 0.5
    
    #View(antilogit(X1%*%t(B1)))
    A6<-antilogit(X1%*%t(B6))
    P6<-cbind(A6[ ,1], A6[ ,2] - A6[ ,1], 1 - A6[ ,2])
    
    resp = numeric(500)
    for(i in  1:500){
      resp[i] = sample(3, size = 1, prob = P6[i,])
    }
    data6 = as.data.frame(cbind(X,resp = as.factor(resp)))
    data6$resp = as.factor(data6$resp)
    data6$rdexp = as.factor(data6$rdexp)
    m6 <- polr(resp ~ ., data = data6[,-1], Hess=TRUE)
    
    
    
    
    #Predict the probability matrices
    M1=matrix(nrow=3, ncol=3)
    M2=matrix(nrow=3, ncol=3)
    
    M1[1, ]=predict(m1,  type="probs", newdata = newdata)
    M1[2, ]=predict(m3, type="probs", newdata = newdata)
    M1[3, ]=predict(m5, type="probs", newdata = newdata)
    
    M2[1, ]=predict(m2, type="probs", newdata = newdata)
    M2[2, ]=predict(m4, type="probs", newdata = newdata)
    M2[3, ]=predict(m6, type="probs", newdata = newdata)
    
    #A function that returns the optimal action matrix
    
    V[[th]] <- V[[th]] + (optact(M1, M2, 8, th/4, 0.6) == 1)
  }
  V[[th]] <- V[[th]] / 30
}
