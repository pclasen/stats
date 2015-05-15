################################################################
#		General syntax for mixed effects (HLM) models
################################################################

data$ctime							# mean centering time
	<- data$time-(mean_time)		#	when working with polynomials of time
									#	need to mean center each polynomial term
	
library(nlme)						# linear mixed effects library

 mm <-								# create mixed effects object
 	lme(y~ctime,					#	specify model
 		data=data,					#	data source
 		random=~1|ID)				#	random effect error term (here random intercept)
summary(mm)							# view results

AIC(mm)								# for comparison to reg or rmanova

mm2…	random=~-1+ctime|ID			# 	random slopes error term (note -1)
mm3…	random=~1+ctime|ID			#	random slopes and intercepts term

AIC(mm2)							# model comparison
AIC(mm3)							# 	lowest AIC, indicator of best model for random effects
									#	way to conceptualize these terms:
									#		random intercepts: 
									#			significant subject to subject variability in outcome on average
									#		random slopes: 
												significant subject to subject variabiltiy in change across time

anova(mm, mm2)						# direct model comparison
									# 	aproximation only, AIC is better indicator
									
mm…		method="ML"					#	use this setting to work through fixed effects elimination
mm…		method="REML"				#	use this setting to work through random effects specification
									#	work flow:
									#		start by specifying random effects
									#		switch to "ML" to work through fixed effects
									#		go back to "REML" to re-estimate
									# 		check assumptions

data$yhat							# get fitted values
	<- fitted(mm)					#	note difference in calling them

data$residuals						# get residuals
	<- residuals(mm)				#	note difference in calling them

library(lattice)					# lattice plotting functionality

xplot(yhat~ctime,					# plot the fitted values by time
		data=data,					#	data source
		type="l",					# 	lines
		group=ID,					#	group by ID (could use other grouping vars)
		…)							#	see ?xyplot for other arguments

library(car)						# companion to applied regression

qq.plot(data$residuals,				# qq plot of model residuals
		dist="norm",				#	superimpose normal distribution
		labels=FALSE)				#	don't plot labels

hist(data$residauls, freq=FALSE)	# histogram of residuals
mean_res <- mean(data$residuals)	#	with normal distribution superimposed
sd_res <- sd(data$residuals)
curve(dnorm(x, 
			mean=mean_res,
			sd=sd_res,
			add=TRUE))

shapero.test(data$residuals)		# shapiro wilk

res_mm_anova <-						# test independence as a function of ID
	lm(residuals~factor(ID),		#	this is a formality, ID must be treated as a factor
		data=data)
summary(res_mm)						# view results

data_sorted <-						# sort on predicted values
	data[order(data$yhat),]

acf(data_sorted$residuals)			# run autocorrelation test of independence on residuals

for (i in 1:16) print(				# Box-Ljung for first 16 lags
		Box.test(data_sorted$residuals,
					lag=i,
					type="Ljung"))

xyplot(residuals~yhat,				# check hetergeneity of variance by (res vs. fitted)
		data=data,					#	data source
		type="p")					#	points
		
		residuals~ctime				#	or residuals by predictors

random_effects_mm <-				# extract random effects parameters (intercept, slope)
	ranef(mm)						#	for each case ?ranef.lme for details
summary(random_effects_mm)			# view them

qq.plot(random_effects_mm[,1],		# qq of random intercept ([,1] - column 1 is intercept)
		dist="norm",				#	both intercept and slope should be centered at 0
		labels=FALSE)				#	and be approximately symmetric around 0

qq.plot(random_effects_mm$ctime,	# qq of random slopes
		dist="norm",
		labels-FALSE)
		
# OTHER MODEL ARGUMENTS				# when we see hetergeneity of variance issues

mm…		correlation=corCAR1(		# there may be autocorrelation across time
				form=~ctime|ID)		#	individual who changes a lot from time 1 to time 2
									#	also changes a lot from time 2 to time 3
									# this argument allows us to model that structure

mm…		weights=varIndent(			# allow each time point to have it's own variance
				form=~ctime|ID)		#	when applying these weights and plotting variance
									# 	it will "look worse" on the plot - because of the weighting
									#	importatn to use AIC and anova to compare model fit
							
									# if and when applying these arguments
									#	if they improve model, you have to go to the beginning
									#	all fixed effects, and start the work flow over again