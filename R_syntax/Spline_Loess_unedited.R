data <-  data.frame(age=c(18,18,18,19,19,20,20,21,22,22,22,22,22,24,25,26,26,27,27,29,30,30,30,30,31,31,31,32,32,32,32,33,33,35,39,39,40,40,41,42,42,42,43,43,44,44,44,47,47,47,48,50,50,51,52,52,53,53,55,55,55,55,55,56,56,57,57,57,58,58,58,59,59,59,60,60,62,64,64,68,68,69,70,74,74,75,75,76,78,78,79,80,80,81,82,85,85,88,88,89),gender=c(0,1,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,1,1,1,1,1,1,0,0,1,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,0,1,0,0,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,1,0,1,1,0,0,0,1,0,1,1,1,1,0,0,0,1),satis=c(66,43,77,75,85,59,70,81,39,60,68,66,66,90,54,56,62,48,33,73,62,39,64,25,74,94,48,69,57,54,42,59,51,28,57,55,49,66,60,40,27,65,34,62,32,73,27,62,71,86,47,45,55,50,18,45,35,27,35,55,47,51,46,52,76,65,66,49,63,32,49,43,69,34,57,51,80,46,21,58,66,54,53,93,46,24,64,82,62,77,54,77,51,81,67,49,52,82,100,79))

fix(data)

summary(data)
# gender == 1 is male
# satis == satisfaction

shapiro.test(data$satis) # normal
shapiro.test(data$age) # not normal

##Must install car package, then issue library command

library(car)

qq.plot(data$satis, dist= "norm", labels=FALSE)
qq.plot(data$age, dist= "norm", labels=FALSE)
qq.plot(data$age, dist="t", df=8, labels=FALSE) # can use other distributions, in this case a t-dist with 8 df

density_age <- density(data$age)
plot(density_age) # kernal density plot

#This is a little more useful than a histogram -- shows the truncated left tail clearly.
#IVs don't have to be normal, outcome doesn't have to be either. Assumption only applies to residuals.
#In reality, if you have non normal outcome you are more likely to have non-normal residuals.

###########################
#	Splines analysis	#
###########################

matplot(data$age, data$satis, type="p", lty=1, ylab="data$satis", pch=1) # x,y, arguements (help matplot)
# plot looks like noise on first pass

autospline <- smooth.spline(data$age, data$satis)
# smooth.spline(x,y) - function that runs cross validation, determines all parameters for minimization
# result is the fit

autosplinepredict <- predict(autospline,data$age)
# use the predict function to predict all the cases in the dataset (for age)

data$autospline <- autosplinepredict$y
# add this object (these predicted values) to the dataset

fix(data)

matplot(data$age, data$satis, type="p", lty=1, ylab="data$satis", pch=1)
# plot the orignal scatter plot
matlines(data$age, data$autospline, type="l", lty=1)
# add the autospline predicted value as a function of age as a line on this plot

# Here we see a relationship that is parabolic (quadratic)
# Had we run a linear regression we would not see an effect

autospline
# Provides information about the spline that was converged upon
# Parameters
# Equivelant degrees of freedom: Hat matrix diagonal - equivelant dfs suggestive of quadratic
# The value here is that the spline helps us see things in the data that were hard to see before
# Then we can follow up with more traditional model fitting to run our inferential stats

##################################
##   RUN THE QUADRATIC MODEL   ###
##################################
data$age2 <- with(data, age^2)
RegModel.2 <- lm(satis~age+age2, data=data)
summary(RegModel.2)

data$fitted.RegModel.2 <- fitted(RegModel.2) # generate fitted values
data$residuals.RegModel.2 <- residuals(RegModel.2) # generate residuals
data$sqres.RegModel.2 <- with(data, residuals.RegModel.2^2) # generate squared error
summary(data$sqres.RegModel.2) # get mean squared error

##The meansq residual here is just over 250.

# Take a look at the quadratic solution on our original plot
matplot(data$age, data$satis, type="p", lty=1, ylab="data$satis", pch=1)
matlines(data$age, data$fitted.RegModel.2, type="l", lty=1)

## Test your residuals (normality, independence, homogenity of variance)
qq.plot(data$residuals.RegModel.2, dist= "norm", labels=FALSE)
shapiro.test(data$residuals.RegModel.2)
acf(data$residuals.RegModel.2)
matplot(data$age, data$residuals.RegModel.2, type="p", lty=1, ylab="data$satis", pch=1)

######################################################
##	Constrain the smooth spline equivelant dfs	##
######################################################

##Because the df for a spline smooth is the trace of the equivalent X matrix,
##the proper comparison for a parametric quadratic regression is a spline with 
##3 df -- because the intercept effectively counts as 1 df.

splinedf3 <- smooth.spline(data$age, data$satis, df=3)
splinedf3predict <- predict(splinedf3,data$age)
data$splinedf3 <- splinedf3predict$y
fix(data)
matplot(data$age, data$satis, type="p", lty=1, ylab="data$satis", pch=1)
matlines(data$age, data$splinedf3, type="l", lty=1)
splinedf3

data$sqres.splinedf3 <- with(data, (splinedf3-satis)^2) # squared error
summary(data$sqres.splinedf3) # mean squared error

##The mean squared residual for the spline fit is not as good at 256+, but this isn't
##terribly surprising for a couple of reasons: 1) there isn't a lot of local
##variation, and 2) the true underlying function is a parametric quadratic.

######################################################
##	Other non-parametric regression: Loess		##
######################################################

# Windowed regression - smaller you make the window more tight you are to data, but the penalty is expending more dfs. But local regression is another way of approaching the problem. Mathematically it is not as good as splines. 

# Need to give it some constraints ...
# 1) How big is the window?, 2) what kind of regression do you want to run in that window?
# span == window size, degree == type of regression (1 == linear)
# More accuracy with higher degrees, but expend more dfs
# span and degree trade off	
# Not automated - so this is more antiquated compared to smoothing splines
				
loessdf3 <- loess(data$satis~data$age, span=.70, degree = 1)
loessdf3predict <- predict(loessdf3,data$age)
data$loessdf3 <- loessdf3$fit
fix(data)
matplot(data$age, data$satis, type="p", lty=1, ylab="data$satis", pch=1)
matlines(data$age, data$loessdf3, type="l", lty=1)

data$sqres.loessdf3 <- with(data, (loessdf3-satis)^2)
summary(data$sqres.loessdf3)

##The loess fit with 3df comes closer to the parametric fit, with a meansq residual
##of just over 254.

##One graphics function that uses loess as part of a pretty comprehensive view of
##the data is the following.  Note, however, that this requires the Rcmdr library
##which cannot be installed painlessly on the Mac.

scatterplot(satis~age, ellipse=TRUE, data=data)

data$gender <- as.factor(data$gender)
scatterplot(satis~age | gender, ellipse=TRUE, by.groups=TRUE, data=data)


