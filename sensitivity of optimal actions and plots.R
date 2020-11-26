M1=matrix(nrow=3, ncol=3)
M2=matrix(nrow=3, ncol=3)
Z = list()
W = list()

#Only done here for the purpose of checking sensitvity towards income
#Same can be repeated to check for blood pressure and age

means = c(10000, 80000)
#means = seq(100, 120, 1)
#means= seq(45, 55, 1)

L <- length(means)

for(i in 1:L){
  Z[[i]] = matrix(0, nrow = 7, ncol = 3)  #N = 8
  for(t in 1:100){
    agesample <- rnorm(1, mean = 50, sd = 3)
    #agesample <- means[i]
    
    bpsample <- rnorm(1, agesample + 60, 0.7)
    #bpsample<-means[i]
    
    rdsample = rbinom(1, 1, 0.1)
    hrsample = rnorm(1, 700, 20)
    
    #Thetasample <- 10000 + 1000000*rpareto(1, 100, 10)
    Thetasample<-means[i]
    
    newdata=data.frame(age = agesample, bp = bpsample, 
                       rdexp=as.factor(rdsample), hrm=hrsample, 
                       Theta = Thetasample)
    
    M1[1, ]=predict(m1,  type="probs", newdata = newdata)
    M1[2, ]=predict(m3, type="probs", newdata = newdata)
    M1[3, ]=predict(m5, type="probs", newdata = newdata)
    
    M2[1, ]=predict(m2, type="probs", newdata = newdata)
    M2[2, ]=predict(m4, type="probs", newdata = newdata)
    M2[3, ]=predict(m6, type="probs", newdata = newdata)
    
    Z[[i]] = Z[[i]] + (optact(M1, M2, N = 8,
                              theta = Thetasample, g = 0.7) == 2)
  }
  Z[[i]] <- Z[[i]] / 100   # Estimate of the mean proportion
  W[[i]] <- sqrt(Z[[i]] * (1 - Z[[i]]) / 100)
}

######## Plot for sensitivity towards income difference 

midline = Z[[1]][,3]
bandwidth = qnorm(0.05, lower.tail = F)*sqrt(midline * (1 - midline) / 100)
upperline = midline + bandwidth
lowerline = midline - bandwidth

midline1 = Z[[2]][,3]
bandwidth1 = qnorm(0.05, lower.tail = F)*sqrt(midline1 * (1 - midline1) / 100)
upperline1 = midline1 + bandwidth1
lowerline1 = midline1 - bandwidth1


col1<- rgb(red = 0, green = 0, blue = 1, alpha = 0.4)
col2<-rgb(red = 0.67, green = 1, blue = 0.18, alpha = 0.5)
par(mfrow = c(1,1))
plot(midline, type = 'l', lwd = 5, col = 'dodgerblue2', ylim = c(0,1),
     xlab="Time point", ylab = "Proportion of the treatment action",
     main = "s=3")
polygon(c(1:7,rev(1:7)), c(upperline,rev(lowerline)),
        col = col1, border = NA)

polygon(c(1:7,rev(1:7)),c(upperline1,rev(lowerline1)),
        col = col2, border = NA)

lines(midline, type = 'l', lwd = 5, col = 'blue',
      xlab = "Time point")
lines(x = 1:7, y = midline1, type = 'l', lwd = 5, 
      col = 'darkgreen')
legend(1, 1, legend=c("40000", "45000"),
       col=c("blue", "darkgreen"), lwd=5)

##################  Sensitivity twards bp or age


sensiv= sensiv1 = sensiv2 = numeric(L)
for(i in 1:L){
  sensiv[i] = Z[[i]] [5, 3]
  sensiv1[i] = sensiv [i] + W[[i]][5, 3] * qnorm(0.05, lower.tail = F)
  sensiv2[i] = sensiv [i] - W[[i]][5, 3] * qnorm(0.05, lower.tail = F)
}

plot(means, sensiv, col='dodgerblue', lwd=5, type='l', ylim = c(0,1),
     ylab="Proportion of treatment action", 
     xlab="Blood Pressure", main="(5,3)th entry")
polygon(c(means,rev(means)),c(sensiv1,rev(sensiv2)),
        col = 'violet', border = NA)
lines(means, sensiv, col='violetred3', lwd=5, type='l')
lines(means, sensiv1, col='violetred3', lwd=1, type='l')
lines(means, sensiv2, col='violetred3', lwd=1, type='l')