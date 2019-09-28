rm(list=ls())

#Setting my data. It's in the R folder

dat = read.table(file="hetero (2).txt",header=TRUE)
N = dim(dat)[1]
#Generates the names of the columns
colnames(dat)

#making my X matrix, column binding and looking at the correlation. 
#Very little correlation present
X.mat  = cbind(dat$x1,dat$x2,dat$x3)
cor(X.mat)

#Running the regression, extracting my residuals and fitted values
reg = lm(dat$y ~ dat$x1 + dat$x2 + dat$x3)
summary(reg)
res = reg$res
fit = reg$fit

#Setting the parameters of the figure (the size) and plotting 
#my fitted values against my residuals to check for heteroskedasticity. The idea is
#to see whether any regressor causes my residuals to move. 
par(mfrow=c(2,2)) 
plot(fit,res)
plot(dat$x1,res)
plot(dat$x2, res) #possible heteroskedasticity 
plot(dat$x3,res)


#Testing for heteroskedasticity, (The "I" is there because the 
#professor mentioned a bad experience and said he always does it)
#We can regress each separately since they are not correlated. No omitted variable bias
#That is x1,x2,x3 are not correlated. 
#This is the Breusch-Pagan Test, where we test the residual squared vs the regressors
#Except to complete the test, we would have to do the test statistic:NR^2 of the 
#auxilary regressions, which are the ones below 

#Test-statistic =R2*N= .0143*250 observations = 3.575
reg.res = lm(I(res^2) ~ I(dat$x1^2))
summary(reg.res)
#At the 1 degrees of freedom, 3.575 <3.841459, so we do not reject, no hetereosked.

reg.res = lm(I(res^2) ~ I(dat$x2^2))
summary(reg.res)

reg.res = lm(I(res^2) ~ I(dat$x3^2))
summary(reg.res)

# using two regressors
reg.res = lm(I(res^2) ~ I(dat$x1^2) + I(dat$x2^2))
summary(reg.res)

# using three regressors
reg.res = lm(I(res^2) ~ I(dat$x1^2) + I(dat$x2^2) + I(dat$x3^2))
summary(reg.res)

# final reg - copy and paste here your preferred heterosked regressions 

reg.res = lm(I(res^2) ~ I(dat$x1^2) + I(dat$x2^2))
summary(reg.res)
fitted<- reg.res$fitted.values


# find the estimated h values to perform the Weighted least squares
#The h values are the fitted values of the residual dependent variable 
#regression. H-estim is already squared
h.estim = reg.res$fit
par(mfrow=c(1,1))
ts.plot(h.estim)

# sth on multicollinearity in the heterosked regressions. Checking for 
#multicollinearity wrt x-variables squared. All clear 
X2.mat  = cbind(dat$x1^2,dat$x2^2,dat$x3^2)
cor(X2.mat)

# FGLS transform. Requires dividing by h and since our h.estim is squared, we square root it
y.star = dat$y/sqrt(h.estim)
x1.star = dat$x1/sqrt(h.estim)
x2.star = dat$x2/sqrt(h.estim)
x3.star = dat$x3/sqrt(h.estim)

reg.star = lm(y.star ~ x1.star + x2.star + x3.star)
summary(reg.star)

# load sandwich package
library(sandwich)

# apply robust standard errors to OLS regression from line 16. 
#Generates a variance/covariance matrix 
vcovHC(reg)

# round standard deviations. Since the diagonal contains variances, we 
#square root it to get the standard errors. Round to 3 decimal places
round(sqrt(diag(vcovHC(reg))),3)

# call summary again of the old regression to compare the standard errors 
#generated from the previous command 
summary(reg)

# look at the estimates: they do not change (unless the E(e) is not equal to zero)
summary(reg)$coef[,1]

# finally, compare the t-ratios, where the numerator is the 
#coef of the regressor minus the null which is zero, divided
#by the square root of the variance of each respective x, which is 
#just equal to the standard errors

#So using those same estimates and dividing by the new SE's, we get the new t-stats
round(summary(reg)$coef[,1]/sqrt(diag(vcovHC(reg))),3)




