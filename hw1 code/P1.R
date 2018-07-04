# Chapter 4 R-lab
data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)

# Problem 1
pdf("EuStocks.pdf", width = 6, height = 5)
plot(EuStockMarkets)
graphics.off()

# Problem 2
logR = diff(log(EuStockMarkets))
plot(logR)

# Problem 3
plot(as.data.frame(logR))
index.names = dimnames(logR)[[2]]
par(mfrow=c(2,2))
for(i in 1:4)
{
  qqnorm(logR[,i],datax=T,main=index.names[i])
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

shapiro.test(logR[,1]) # DAX
shapiro.test(logR[,2]) # SMI
shapiro.test(logR[,3]) # CAC
shapiro.test(logR[,4]) # FTSE

# Problem 4
n=dim(logR)[1]
q.grid = (1:n)/(n+1)
df=c(1,4,6,10,20,30)
for(i in 1:4)
{
  #quartz()
  par(mfrow=c(3,2))
  for(j in 1:6)
  {
    qqplot(logR[,i], qt(q.grid,df=df[j]),
           main=paste(index.names[i], ", df=", df[j]) )
    abline(lm(qt(c(.25,.75),df=df[j])~quantile(logR[,i],c(.25,.75))))
  } 
}

# Problem 6
library("fGarch")
x = seq(-.1,.1,by=.001)
par(mfrow=c(1,1))
plot(density(logR[,1]),lwd=2,ylim=c(0,60))
lines(x,dstd(x,mean=median(logR[,1]),sd=mad(logR[,1]),nu=5),lty=5,lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]),sd=sd(logR[,1])),lty=3,lwd=4)
legend("topleft",c("KDE","t: df=5","normal"),lwd=c(2,2,4),lty=c(1,5,3))

# zoom in the left tail
plot(density(logR[,1]),lwd=2,ylim=c(0,30),xlim=c(-0.05,-0.01))
lines(x,dstd(x,mean=median(logR[,1]),sd=mad(logR[,1]),nu=5),lty=5,lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]),sd=sd(logR[,1])),lty=3,lwd=4)
legend("topleft",c("KDE","t: df=5","normal"),lwd=c(2,2,4),lty=c(1,5,3))

# zoom in the right tail
plot(density(logR[,1]),lwd=2,ylim=c(0,30),xlim=c(0.01,0.05))
lines(x,dstd(x,mean=median(logR[,1]),sd=mad(logR[,1]),nu=5),lty=5,lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]),sd=sd(logR[,1])),lty=3,lwd=4)
legend("topright",c("KDE","t: df=5","normal"),lwd=c(2,2,4),lty=c(1,5,3))

# adjust the smooth
plot(density(logR[,1],adjust=3),lwd=2,ylim=c(0,60))
lines(x,dstd(x,mean=median(logR[,1]),sd=mad(logR[,1]),nu=5),lty=5,lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]),sd=sd(logR[,1])),lty=3,lwd=4)
legend("topleft",c("KDE","t: df=5","normal"),lwd=c(2,2,4),lty=c(1,5,3))
