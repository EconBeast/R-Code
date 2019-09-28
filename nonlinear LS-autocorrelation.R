rm(list=ls())

### set local working directory
setwd("C:\\Users\\admin\\Dropbox")
###

#     U.S. data on real consumption and real disposable income(2005 $)
#     1960:Q1 to 2009:Q4 200 observations

#The data file is not being read for some reason 
USdata = `usdata.(1)`
c = USdata[,1] #First column
y = USdata[,2] #Second column

N = length(y)
N

# (i)
reg1 = lm(c ~ y)
summary(reg1)

# (ii)
reg2 = lm(log(c) ~ log(y))
summary(reg2)
exp(reg2$coef[1])

# (iii)
reg3 = lm(c ~ I(y^2))
summary(reg3)

# (iv)
reg4 = lm(c ~ I(sqrt(y)))
summary(reg4)

# (v)
#Creating a sequence from .01-2, in .01 intervals. Done 
#purposely for the 200 observations
beta.fix.vec = seq(0.01,2,0.01)

#Creating an empty matrix 
reg5.coef = matrix(NA,length(beta.fix.vec),2)

s2 = matrix(NA,length(beta.fix.vec),1)

#Fill in the matrices
for(i in 1:length(beta.fix.vec))
{
beta3.fix = beta.fix.vec[i]
reg5 = lm(c ~ I(y^beta3.fix))
#Filling in the matrix for all rows i 
#and all columns (hence not specified)
reg5.coef[i,] = reg5$coef
#Filling in the s2, column specified since theres only 1
s2[i,1] = summary(reg5)$sigma^2
}



#Running the t-test by doing NLS 
as<- .01
bs<- .0001
ae<- 20
be<- 5
m<- nls(c~b*(y**a),start=list(b =bs, a= as), 
        upper = list(a=ae, b=be), algorithm = "port")
summary(m)

#The t-test came out to 16.74


# plot objective function
plot(s2)

# find the arg min and run this particular regression and 
#compare to old regression when delta is equal 1
##To test the restriction of delta=1, we do an F test
#So we need the R2 of both the optimal and when delta =1

reg1 = lm(c ~ y)
summary(reg1)

#We find the minimum, set i= the minimum and define our 
#parameter as encompassing this value. We run the regression

which.min(s2)
i = which.min(s2)
beta3.fix = beta.fix.vec[i]
reg5 = lm(c ~ I(y^beta3.fix))
summary(reg5)

#The coefficient tells us how much of our income is used for
#consumption
round(reg5$coef,2)
#This is the beta estimator that minimized the residual sum
#of squares 
beta3.fix


#Plotting the residuals and their autocorrelation function

e = reg5$res
par(mfrow=c(1,2))
ts.plot(e)
abline(h=0)

acf<- acf(e)
acf(e,plot=TRUE)

#This is the first order auto-correlation
rho1 = acf(e,plot=FALSE)$acf[2]

#There is an option for degrees of freedom,
#which is just = to the lag (not always)

#You can type Q1 to make it show the actual statistic. Testing for 1 and 5 lags
Q1 = Box.test(e, lag=1, type =c("Box-Pierce"))

chi_lag1<- qchisq(.95, df=1)

Q5 = Box.test(e, lag=5, 
              type =c("Box-Pierce"), fitdf = 5)

chi_lag5<- qchisq(.95, df=5)

Q199<- Box.test(e, lag=199, type =c("Box-Pierce"), 
                fitdf=199)
chi_lag199<- qchisq(.95, df=199)
