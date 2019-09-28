rm(list=ls())

#Running the Monte Carlo Simulation, Data Generating Process (DGP)

#Setting size
par(mfrow=c(1,2))

#M= amount of samples
#setting beta amounts 
M = 10000
beta1 = 1
beta2 = 2


# 10 observations
n = 10
#rnorm defaults 0 mean and 1 sd if nothing is given. Typically, 
#It can be written as rnorm(n, mean= , sd= )
x = rnorm(n)

#when defining the matrix, NA is placed if you dont have data 
b = matrix(NA,M,2)

#Here the function runs 10,000 regressions. where little "m" is our choice
#for naming the elements of M. We create our error term epsiolon
#

for(m in 1:M)
{
	eps = rnorm(n)
	y = beta1+beta2*x+eps

	reg = lm(y ~x)
	b[m,] = coef(reg)
}

#Plotting the density of our x coefficient
plot(density(b[,2]),main="Mila's Hunger",
     ylab="Production",xlab="Chocolate amount",ylim=c(0,9),xlim=c(1,3))



# 50 observations
n = 50
x = rnorm(n)
b = matrix(NA,M,2)

for(m in 1:M)
{eps= rnorm(n)
  y = beta1+beta2*x+eps
  
  reg = lm(y~x)
  b[m,] = coef(reg)
}

#plot(density(b[,2]))

plot(density(b[,2]))


#Defining number of samples
M = 10000
beta1 = 1
beta2 = 2

#Now we pull from a t(5)-distribution, df=5
n = 10
x = rt(n,5)
b = matrix(NA,M,2)

for(m in 1:M)
{
	eps = rt(n,5)
	y = beta1+beta2*x+eps

	reg = lm(y ~ x)
	b[m,] = coef(reg)
}

plot(density(b[,2]),main="Cheese",ylab="oatmeal",xlab="moose",
     ylim=c(0,9),xlim=c(1,3))





