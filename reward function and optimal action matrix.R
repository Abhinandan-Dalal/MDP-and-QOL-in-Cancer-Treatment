###   reward function calculates the patient's reward
###   exp_reward function calculates the expected reward
###   final_reward function calculates the reward at the horizon
###   optact function calculates the optimal treatment action matrix


antilogit=function(x){
  return(1/(1+exp(-x)))
}
logit=function(x){
  return(log(x/(1-x)))
}

cost<- c(0, 5000)
lambda = 1.2

reward=function(s,i,t,g,a, theta){
  R=g*(i-s)/((t+1)^2)
  return( R - (cost[a]/(i*theta))*exp(-lambda*t) ) 
  #log(1+abs(R))*sign(R)
}


final_reward=function(s,N,g){
  R=g*(3-s)/(N+1)^2
  return(log(1+abs(R))*sign(R))
}

exp_reward=function(P,s,t,g, a, theta){
  sum=0
  for(i in 1:3)
    sum=sum+P[s,i]*reward(s,i,t,g, a, theta)
  return(sum)
}


optact=function(M1,M2,N,theta,g){
  
  # $ M_{t,j} = u_t^* (j)$, as solved from Bellman's equations
  
  U=matrix(data=0,nrow=N,ncol=3)
  D=matrix(data=0,nrow=N-1,ncol=3)
  a=b=0
  for(i in 1:3)
    U[N,i]=final_reward(i,N,g)
  for(t in 1:N-1){
    for(i in 1:3){
      A<-exp_reward(M1,i,N-t,g, 1, theta) + sum(U[N-t,]*M1[i,])
      B<-exp_reward(M2,i,N-t,g, 2 , theta) + sum(U[N-t,]*M2[i,])
      U[N-t-1,i]=max(A,B)
      D[N-t-1,i]=which.max(c(A,B))
    }
  }
  return(D)
}