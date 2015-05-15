#################################################
#	General syntax for logistic regression
#################################################

lreg <-								# create object
	glm(y~x, 						# 	specify model (y is binary)
		family=binomial(logit),		#	call logit
		data=data)					# 	specify data source
summary(lreg)						# see results