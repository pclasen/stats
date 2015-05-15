######################################################
#		General syntax for weighted least squares
######################################################

# Variance inconstancy problems…

data$residuals2						# create squared residuals
	<- data$residuals^2

reg2 <-								# create new regression object
	lm(residuals2~x, data=data)		#	regress squared residuals on original x
summary(reg2)						# see results

data$yhat2 							# save fitted values
	<- reg2$fitted.values

data$yhat2plus						# add a constant to shift data 
	<- data$yhat2+constant			# 	eliminate negative values

data$preweight						# take reciprocal
	<- 1/data$yhat2plus				# 	weight will be inversly proportional
									#	to squared error (bigger, less weight)

sum(data$preweight)					# should sum to original n (sample size)

data$weight 						# if not, scale so that it does
	<- data$preweight*(n/sum(data$preweight)) # n is sample size

sum(data$weight)					# confirm

wls <-								# RUN MODEL, create WLS object
	lm(y~x, 						#	model
		weights=weight,				#	weights
		data=data)					# 	data
summary(wls)						# view results

plot(data$x, data$y, type="p")		# scatter plot with points
abline(reg=reg, col="red")			# 	overlay regression in red
abline(reg=wls, col="green")		# 	overlay wls in green

data$wls_res						# save WLS residuals
	<- wls$residuals

data$graph_wls_res					# scale for plotting
	<- data$wls_res*(data$weight^.5)

plot(data$x, data$graph_wls_res, 	# plot to examine variance inconstancy
		type="p")