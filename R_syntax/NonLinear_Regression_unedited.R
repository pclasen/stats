# General idea:
#	fit a function that approximates data
#	minimize error using a non-linear search algorithm
# 	we did a lot of this in excel, using solver
#	but here is some general syntax for a logistic function and a sine wave
#	DISCLAIMER: these are not edited, or tested

############# general logistic function ########################

# start with polynomial regression
# run model with t, t+t^2, t+t^2+t^3, etc.
reg3.1 <- lm(Y~t)
summary(reg3.1)
# run AICs on each of these models
AIC(reg3.1)
# run BICs on each
library(stats4)
BIC(reg3.1)
# save fitted values and residuals from models
# plot the predicted (fitted values)

# hard pressed to use visual inspection to decide
# need to use other criterion for model fit

# e.g. use AIC values
# check assumptions for each of the models 

## Tests show that residuals are normally distributed - no reason not to use minimized SS as fitting criterion
# yhat = A+(C/1+e^[-B(t-m)]) General logistic function
# A = lower floor
# C = depth
# B = slope
# M = time of max growth

ss <- function(parameters) {
	floor <- parameters[1]
	depth <- parameters[2]
	maxgrowth <- parameters[3]
	growthrate <- parameters[4]
	ss < sum((Dataset3$Y - (floor + (depth/(1+ exp((-1*growthrate)*(Dataset#3*maxgrowth))))))**2)
	return(ss)
	}

result <- optim(parameters <- c(0.04,0.06,15,0.5),ss)
result$par
result$value # this is sum of squares for non-linear model
anova(reg3.3) # this is the sum of squares for polynomial
# compare the two
# can also compare the linear model



########### sine wave ######################################


# input data
# we know there is a cycle here, one way to look at it is to examine the autocorrelation plot

data <- read.csv("/Users/petercclasen/Desktop/Peter/AdvancedStats1/sinewave.csv")
acf(data$y)
acf(data$y, lag.max=40) # expand window

# we can see a pattern here that fluctuates in a cyclical fashion
# plot data to examine this pattern

plot(data$t,data$y)

# in this case it looks like we could model a sine wave 
# Sine Wave Model -- Yhat = [Ba*sin(2*pi*Bf*t+2*pi*Bp)]+Bo
# 	Ba is the amplitude (height and depth)
# 	Bf is frequency (cycles per observation) - inverse of the period of cycle
#		Eg. observation 60 days, 2 periods, then freq = 1/30
# 	Bp is the phase (this is the shift in function)
# 		Pure sine wave peak starts at 0 - this is not always true in data
#		Move wave to match up with data
# 	Bo is the y intercept
#		Pure sine wave is symetrical around 0 - data may not be

# This is non-linear