Dataset2
# OLS
reg2.1 <- lm(formula = Y ~ X, data = Dataset2)
summary(reg2.1)

# save residuals
Dataset2$residuals.reg2.1 <- residuals(reg2.1)

# get qq plot
library(car)
qqPlot(Dataset2$residuals.reg2.1, dist="norm")

# plot residuals against x
plot(Dataset2$residuals.reg2.1~Dataset2$X)

# density plot
plot(density(Dataset2residuals.reg2.1), dist="norm")

# shapiro wilk
shapiro.test(Dataset2$residuals.reg2.1)

# outlier test
outlierTest(reg2.1)
outlierTest(reg2.1, cutoff=.99)
# Test each case to see if it is an outlier, need to correct for multiple comparisons
# This test applies a bonferonni correction

# one abberant case screwing up normality assumption

# LORENTZIAN CAUCHY regression (Introduction to Scientific Programming by Andrew Robinson)
# Why choose LC dist - not much to choose from between, in social sciences the most common is LAD, LC more common in physics, huber is more common in economics --- Best practice = run lorentzian and a huber. Footnote the results.

lorcrit <- function(beta) {
	lorcrit<- sum(log(1+((Dataset2$Y - (beta[1]+(beta[2]*Dataset2$X)))**2))) #In R, log is by default the natural log
	return(lorcrit)
	}
lor_result <- optim(beta <- c(2.708,.0488),lorcrit)
# get the parameters
lor_result$par

# Need to use bootstrapping to get the significance tests for these parameters
# via simulation, work up a distribution of Lorentzian parameters (slope, intercept)

# data frame to hold bootstrap results
loopres<- data.frame(b0=numeric(0), b1=numeric(0))
# bootstrap the parameters
for(i in 1:100) {
	sample <- Dataset2(sample(15,15,replace=T), ]
	lorcrit_s <-function(beta) {
		lorcrit_s <- (sum(log(1+((sample$Y - (beta[1]+(beta[2]*sample$X)))**2))))
		return(lorcrit_s)
		}
ls <- lm(Y~X, data=sample)
lor_result_s <- optim(beta <- c(ls$coefficient[1],ls$coefficient[2],lorcrit_s))
loopres[i,1] <- lor_result_s$par[1]
loopres[i,2] <- lor_result_s$par[2]
}

# evaluate whether the distribution overlaps 0
summary(loopres)
plot(density(loopres$b1)) # the distribution of slopes is not normal or T (bigger picture - outlier influences parameter dists)
# this method allows us to find out what the dist of the parameter really looks like

