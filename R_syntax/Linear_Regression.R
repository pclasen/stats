##################################################
#		General syntax for linear regression
##################################################

reg <-							# create a regression object
	lm(y~x,data=data)			# define your model, specify data source
								# 	interaction (y~a*b)
								# 	lower order interaction terms (y~a+b+c+a:b+a:c)

summary(reg)					# get regression output

names(reg)						# check out model outputs stored in this object

data$residuals					# save residuals to data set
	<- reg$residuals
	
data$yhat						# save fitted values
	<- reg$fitted.values
	
quartz()						# diagnostic plots - call device
par(mfrow = c(2,2))				# 	split screen into four quadrants
plot(reg1)						# 	resid vs fitted, qq, scale, leverage

outlierTest(reg)				# Test each case to see if outlier
outlierTest(reg, cutoff=.99)	#	correct for multiple comparisons

qq.plot(data$residuals)			# qq by itself
hist(data$residuals)			# histogram 

library(e1071)					# more normality, skewness/kurtosis library

skewness(data$residuals, 		# 	get skewness estimate
			type = "2")
se.skewness <- (6/n)^.5			# 	compare to standard error of skewness
se.skewness						#	sqrt(6/n), n = sample size

kurtosis(data$residuals, 		# 	get kurtosis estimate
			type="2")
se.kurtosis <- (24/396)^.5		# 	compare to standard error of kurtosis
se.kurtosis						# 	sqrt(24/n), n = sample size

shapiro.test(data$residuals)	# shapiro-wilk test of normality

acf(data$residuals)				# autocorrelation test for independence
for (i in 1:16)					# run Box-Ljung test of independence
	print(Box.test(				#	on first 16 AC lags
			data$residuals,
			lag=i,
			type="Ljung"))

plot(data$residuals, data$x)	# homogeneity of variance

AIC(reg1)						# AIC stat for model comparison (nested models)


