rm(list=ls())

N = 500

sd.eps = 4
#As we increase the variance, the OLS estimator 
#becomes more precise 
sd.x = 11000

x = rnorm(N,0,sd.x)
y = 0.5*x + rnorm(N,0,sd.eps)

#idk why negative 1
reg = lm(y ~ x-1)
summary(reg)

#Solving for standard deviation 
s2.beta = sum(reg$res^2)/(N-1)*1/sum(x^2)
sqrt(s2.beta)

plot(x,y)
#tAKE AWAY: the larger the variation in X,
#the likelier it is to capture the variation
# in the y variable