library(ggplot2)

M1=matrix(nrow=3, ncol=3)
M2=matrix(nrow=3, ncol=3)
Z = list()
W = list()

means = seq(60, 70, by = 1)

for(i in 1:length(means)){
  Z[[i]] = matrix(0, nrow = 7, ncol = 3)  #N = 8
  for(t in 1:50){
    #a <- rnorm(1, means[i], 2)
    
    a <- means[i]
    bpressure <- rnorm(1, a + 60, 0.7)
    
    # bpressure = means[i]
    # a<- rnorm(1, bpressure-60, 2)
    rdsample = rbinom(1, 1, 0.1)
    hrsample = rnorm(1, 700, 20)
    newdata=data.frame(age = a, bp = bpressure, 
                       rdexp=as.factor(rdsample), hrm=hrsample)
    
    M1[1, ]=predict(m1,  type="probs", newdata = newdata)
    M1[2, ]=predict(m3, type="probs", newdata = newdata)
    M1[3, ]=predict(m5, type="probs", newdata = newdata)
    
    M2[1, ]=predict(m2, type="probs", newdata = newdata)
    M2[2, ]=predict(m4, type="probs", newdata = newdata)
    M2[3, ]=predict(m6, type="probs", newdata = newdata)
    
    Z[[i]] = Z[[i]] + (optact(M1, M2, N = 8,
                              theta = 0.7, g = 0.7) == 1)
  }
  Z[[i]] <- Z[[i]] / 50   # Estimate of the mean proportion
  W[[i]] <- sqrt(Z[[i]] * (1 - Z[[i]]) / 50)
}

sensiv= sensiv1 = sensiv2 = numeric(length(means))
for(i in 1:length(means)){
  sensiv[i] = Z[[i]] [6, 3]
  sensiv1[i] = sensiv [i] + W[[i]][6, 3] * qnorm(0.05, lower.tail = F)
  sensiv2[i] = sensiv [i] - W[[i]][6, 3] * qnorm(0.05, lower.tail = F)
}

# plot(means, sensiv, type = 'l', 
#      xlab = 'BP means', ylab = 'proportion of action 1', lwd = 2)
# lines(means, sensiv1, col = 'red', type = 3)
# lines(means, sensiv2, col = 'blue', type = '-')
# u = c(sensiv1[5], sensiv2[5])
# lines(c(124, 124), u)


dframe = data.frame(cbind(means, sensiv, sensiv1, sensiv2))
plot5 = ggplot(data = dframe, aes(x = means, y = sensiv))+
  geom_line(color = "red", lwd = 1.2, linetype = "solid")+
  geom_ribbon(data= dframe, aes(ymin = sensiv1, ymax = sensiv2), alpha=0.3)+
  ggtitle("Sensitivity w.r.t Age for (6, 3)-th entry")+
  xlab("Expected Age") + ylab("Proportion of Action 1")
plot5
