NASDAQ = read.table("d_nasdaq_82stocks.txt", header = TRUE)
NASDAQ = NASDAQ[ -c(1) ]           # drop the Date column
full = lm(formula = Nasdaq~., data = NASDAQ) # fit a full regression model
summary(full)

library(rms) # for ols and fastbw
full_ols = ols(Nasdaq ~ AA + AAPL + AET + ALTR + AMAT + AMD + AMGN + AOC + APA +
                 APD + ASH + AT + AVY + AXP + AYE + BA + BAC + BAX + BBY +
                 BC + BF.B + BLL + BMY + BNI + C + CAH + CAT + CCE + CCL +
                 CEG + CFC + CLX + CMI + COP + COST + CSX + CVX + DD + DELL +
                 DIS + DOV + DUK + ED + EDS + EIX + EK + EOG + EXC + F + FPL +
                 FRX + GE + GM + GPS + GT + HD + HLT + HON + HPC + HPQ + HRB +
                 HSY + HUM + IBM + INTC + IPG + JNJ + JPM + JWN + K + KBH +
                 KMI + KO + KR + LEG + LEN + LLY + LM + LMT + LOW + LUV +
                 MAS, data = NASDAQ)
# Automated F-test-based backward selection using rms::fastbw()
seleted = fastbw(fit = full_ols, rule = "p", sls = 0.1)
factors_kept = NASDAQ[, c(0, seleted$factors.kept)+1]
reduced = lm(Nasdaq~., data = factors_kept)
anova(reduced, full)

# residual diagonize
resid1 = rstudent(reduced)
par(mfrow=c(1,2))
plot(reduced$fit,resid1,
     ylim=c(-15,15),main="(a)")
qqnorm(resid1,datax=F,main="(b)")
qqline(resid1,datax=F,lwd=5,col="blue")

# The best model in the 5-variable case
library(leaps)
five_factor_lm =
  regsubsets(Nasdaq~.,
             data = NASDAQ,
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = 5,    # limit on number of variables
             force.in = NULL, force.out = NULL,really.big=T,
             method = "exhaustive")
summary.out = summary(five_factor_lm)
summary.out
as.data.frame(summary.out$outmat)
plot(five_factor_lm, scale = "adjr2", main = "Adjusted R^2")
plot(five_factor_lm, scale="Cp")

# residual 
attach(NASDAQ)
subset_lm = lm(Nasdaq ~ ALTR + AMAT + GE + INTC + LM)
summary(subset_lm)
anova(subset_lm, reduced)
AIC(reduced) - AIC(subset_lm)

resid2 = rstudent(subset_lm)
par(mfrow=c(2,2))
plot(subset_lm$fit,resid2,
     ylim=c(-10,10),main="(a)")
plot(ALTR,resid2,
     ylim=c(-12,12),main="(b)")
plot(AMAT,resid2,
     ylim=c(-12,12),main="(c)")
plot(GE,resid2,
     ylim=c(-12,12),main="(d)")
plot(INTC,resid2,
     ylim=c(-12,12),main="(e)")
plot(LM,resid2,
     ylim=c(-12,12),main="(f)")

par(mfrow=c(1,1))
qqnorm(resid2,datax=F,main="QQ plot")
qqline(resid2,datax=F,lwd=5,col="blue")

## Don't care
library(car)
layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend <- 
  subsets(five_factor_lm, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <- 
  subsets(five_factor_lm, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)

