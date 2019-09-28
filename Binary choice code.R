rm(list=ls())

setwd("C://users//admin/dropbox")
dat = read.table("rec_spread.txt",header=TRUE)

#Keep in mind that the weird variable name is the term spread.
#it is the difference between the 10 year and 3 month maturity interest rate
N = dim(dat)[1]
colnames(dat)
N
#Taking the sum of US recession to see how many recessions there were
sum(dat$USREC)
#Taking the sameple average of the recession dummy, rounding to 3 decimals
#The average just tells me the percent of recession months to total months
round(mean(dat$USREC),3)

#Setting up my window for graphs
par(mfrow=c(2,2),mar=c(2,2,2,2))

# linear regression
linreg = lm(dat$USREC ~ dat$T10Y3M)
summary(linreg)
#The plot will show the obvservations of the spread on the x-axis
ts.plot(dat$USREC,ylim=c(-0.1,1.1))
#The regression line is red
lines(linreg$fit,col=2)
#Setting lines at y=.25 and .5, and setting line type to 2=dashed
abline(h=0.25,lty=2)
abline(h=0.5,lty=2)
plot(linreg$residuals)

#Shortcomings: the fitted values are not restricted so that it is 
#between 0 and 1. Likewise,plotting the residuals shows that they are 
#heteroskedastic

# probit, setting the binomial distribution
preg = glm(dat$USREC ~ dat$T10Y3M, family = binomial(link = "probit"))
summary(preg)
#The loglikelihood of the the probit model
l1 = as.numeric(logLik(preg))

ts.plot(dat$USREC,ylim=c(-0.1,1.1))
#Plotting the fitted values 
lines(preg$fit,col=4)
abline(h=0.25,lty=2)
abline(h=0.5,lty=2)

#Running the Probit regression on just the intercept
preg0 = glm(dat$USREC ~ 1, family = binomial(link = "probit"))
summary(preg0)
#Getting the loglikelihood of this probit model
l0 = as.numeric(logLik(preg0))

#McFadden Pseudo R squared. =.2929, which means the 
#model of interest is not that strong
1-(l1/l0)

#Loglikelihood ratio test. The result of 69.60>3.84, so we reject the null
-2*(l0-l1)
qchisq(0.95,1)

#Probit uses the normal distribution. The marginal effect of the probit
#model is the pnorm, which is the normal dist., of our fitted values
#times the respective B coefficient
pmar = pnorm(preg$fit)*preg$coef[2]
#Average marginal effect
mean(pmar)


# logit
lreg = glm(dat$USREC ~ dat$T10Y3M, family = binomial(link = "logit"))
summary(lreg)
l1 = as.numeric(logLik(lreg))

ts.plot(dat$USREC,ylim=c(-0.1,1.1))
lines(lreg$fit,col=4)
abline(h=0.25,lty=2)
abline(h=0.5,lty=2)

lreg0 = glm(dat$USREC ~ 1, family = binomial(link = "logit"))
summary(lreg0)
l0 = as.numeric(logLik(lreg0))
1-(l1/l0)

#Likelihood ratio test
-2*(l0-l1)
qchisq(0.95,1)

#marginal effect of logit model
lmar = (exp(lreg$fit)/(1+exp(lreg$fit))^2)*lreg$coef[2]
mean(lmar)

# cauchit
creg = glm(dat$USREC ~ dat$T10Y3M, family = binomial(link = "cauchit"))
summary(creg)
l1 = as.numeric(logLik(creg))

ts.plot(dat$USREC,ylim=c(-0.1,1.1))
lines(creg$fit,col=4)
abline(h=0.25,lty=2)
abline(h=0.5,lty=2)

creg0 = glm(dat$USREC ~ 1, family = binomial(link = "cauchit"))
summary(creg0)
l0 = as.numeric(logLik(creg0))
1-(l1/l0)

-2*(l0-l1)
qchisq(0.95,1)

cmar = pt(creg$fit,1)*lreg$coef[2]
mean(cmar)

# plot Cauchy link function in comparison to Gaussian
plot(pt(seq(-30,30,0.01),1),type="l",ylim=c(-0.1,1.1),axes=F,lwd=2,ylab="")
abline(h=0)
abline(h=1)
box()
axis(2)
lines(pnorm(seq(-30,30,0.01)),col=2,lwd=2)

#This process below is an alternative to the goodness of fit.
#The idea is to compare the fitted result of the regressions
#to the actual dummy

sum(preg$fit>0.5)
sum(lreg$fit>0.5)
sum(creg$fit>0.5)

#This is the number of correct predictions for probit, logit and cauchit
pn11 = sum((preg$fit>0.5)*(dat$USREC==1))
ln11 = sum((lreg$fit>0.5)*(dat$USREC==1))
cn11 = sum((creg$fit>0.5)*(dat$USREC==1))

#This is also the number of incorrect predictions
pn00 = sum((preg$fit<0.5)*(dat$USREC==0))
ln00 = sum((lreg$fit<0.5)*(dat$USREC==0))
cn00 = sum((creg$fit<0.5)*(dat$USREC==0))

#Number of incorrect 
pn01 = sum((preg$fit<0.5)*(dat$USREC==1))
ln01 = sum((lreg$fit<0.5)*(dat$USREC==1))
cn01 = sum((creg$fit<0.5)*(dat$USREC==1))

#Number of incorrect
pn10 = sum((preg$fit>0.5)*(dat$USREC==0))
ln10 = sum((lreg$fit>0.5)*(dat$USREC==0))
cn10 = sum((creg$fit>0.5)*(dat$USREC==0))

#This is the proportion of incorrect predictions to observations
pwr1 = (pn01+pn10)/N
lwr1 = (ln01+ln10)/N
cwr1 = (cn01+cn10)/N
pwr1
lwr1
cwr1


#Now looking at the probit,logit, cauchit with only intercept
pn11 = sum((preg0$fit>0.5)*(dat$USREC==1))
ln11 = sum((lreg0$fit>0.5)*(dat$USREC==1))
cn11 = sum((creg0$fit>0.5)*(dat$USREC==1))

pn00 = sum((preg0$fit<0.5)*(dat$USREC==0))
ln00 = sum((lreg0$fit<0.5)*(dat$USREC==0))
cn00 = sum((creg0$fit<0.5)*(dat$USREC==0))

pn01 = sum((preg0$fit<0.5)*(dat$USREC==1))
ln01 = sum((lreg0$fit<0.5)*(dat$USREC==1))
cn01 = sum((creg0$fit<0.5)*(dat$USREC==1))

pn10 = sum((preg0$fit>0.5)*(dat$USREC==0))
ln10 = sum((lreg0$fit>0.5)*(dat$USREC==0))
cn10 = sum((creg0$fit>0.5)*(dat$USREC==0))

#proportion of incorrect predictions to observations
pwr0 = (pn01+pn10)/N
lwr0 = (ln01+ln10)/N
cwr0 = (cn01+cn10)/N
pwr0
lwr0
cwr0

#Goodness of fit measure takes the proportion of incorrect predictions
#For each model (model of interest vs intercept)
pr2 = 1-pwr1/pwr0
lr2 = 1-lwr1/lwr0
cr2 = 1-cwr1/cwr0
pr2
lr2
cr2
