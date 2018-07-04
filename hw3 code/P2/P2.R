## Chapter 12 R-lab

# Problem 1
library(AER)
data("USMacroG")
MacroDiff= data.frame(apply(USMacroG,2,diff))
mydata = cbind(MacroDiff$consumption, MacroDiff$dpi, MacroDiff$cpi, MacroDiff$government, MacroDiff$unemp)
colnames(mydata) = list("consumption","dpi","cpi","government","unemp")
pairs(mydata)

# Problem 2
mydata = data.frame(mydata)
fitLm1 = lm(consumption~dpi+cpi+government+unemp, data = mydata)
summary(fitLm1)
confint(fitLm1)

# Problem 3
anova(fitLm1)

# Problem 4
library(MASS)
fitLm2 = stepAIC(fitLm1)
summary(fitLm2)

# Problem 5
AIC(fitLm1)
AIC(fitLm2)
AIC(fitLm1)-AIC(fitLm2)

# Problem 6
library(car)
vif(fitLm1)
vif(fitLm2)

# Problem 7
par(mfrow=c(2,2))
sp = 0.8
crPlot(fitLm1,"dpi",span=sp,col="black")
crPlot(fitLm1,"cpi",span=sp,col="black")
crPlot(fitLm1,"government",span=sp,col="black")
crPlot(fitLm1,"unemp",span=sp,col="black")

#======================================================================
# problem 13.1
library(AER)
data(CPS1988)
attach(CPS1988)
fitLm1 = lm(wage~education+experience+ethnicity)
par(mfrow=c(3,2))
resid1 = rstudent(fitLm1)
plot(fitLm1$fit,resid1,
     ylim=c(-4,4),main="(a)")
lines(lowess(fitLm1$fit,resid1,f=.2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)
plot(fitLm1$fit,abs(resid1),
     ylim=c(0,4),main="(b)")
lines(lowess(fitLm1$fit,abs(resid1),f=.2),lwd=5,col="red")
abline(h=mean(abs(resid1)),col="blue",lwd=5)
qqnorm(resid1,datax=F,main="(c)")
qqline(resid1,datax=F,lwd=5,col="blue")
plot(education,resid1,ylim=c(-3,4),main="(d)")
lines(lowess(education,resid1),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)
plot(experience,resid1,ylim=c(-3,4),main="(e)")
lines(lowess(experience,resid1),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

fitLm2 = lm(log(wage)~education+experience+ethnicity)
summary(fitLm2)

#======================================================================
# problem 13.5
library(faraway)  #  required for halfnorm
par(mfrow=c(1,3))
plot(hatvalues(fitLm2))
plot(sqrt(cooks.distance(fitLm2)))
halfnorm(sqrt(cooks.distance(fitLm2)))



