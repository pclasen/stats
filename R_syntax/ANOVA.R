###################################################
#		General syntax for ANOVA
###################################################

library(car)					# companion to applied regression

data$x							# Need to factorize IVs
	<- as.factor(data$x)

anova <-						# create anova object
	lm(y~x, data=data)			# 	specify model and data source

Anova(anova, type="3")			# get anova table, type 3 sum squares

names(anova)					# check out stored output

data$residuals					# save residuals
	<- anova$residuals
	
data$yhat						# save fitted values
	<- anova$fitted.values

# Post-estimation tests
t.test(y~x,						# t-test
		alternative='two.sided',
		conf.level=.95,
		var.equal=FALSE,
		data=data.subset)		# need to subset on some factor first

oneway <-						# contrast
	lm(y~x, data=data)			# 	set up one way test
summary(oneway)					# 	view results
linear.hypothesis(oneway,		# 	contrast groups
					"x2=x2")	# 	specify groups to compare
					"x2=0"		# 	or compare against 0

								# other built in contrasts functions, run model
contrasts(data$x) <-			#	call polynomial contrasts
	contr.poly
anova1…							# 	run model
summary(anova1)					#	get results
	
contrasts(data#x) <-			# 	effect coding for Type III sum squares
	contr.sum
anova2…							#	run model
summary(anova2)					#	get results
								#	note that last category is reference by default

								# also possible to build a contrast matrix by hand
contrasts(data$x)				#	use same command to impliment it
		<- contrastmatrix		#	here "contrastmatrix" is some built matrix				
								# Tukey HSD for multiple comparisons
								#	q = 
								#	HSD = q*((MSerror/n)^.5) - n = cell size								# any difference larger than HSD is significant

# Post-estimateion assumptions
qq.plot(data$residuals,			# qq plot
		dist = "norm",			# 	superimposed normal distribution
		labels=FALSE)

shapiro.test(data$residuals)	# shapiro-wilk

levene.test(data$y,data$x)		# levene test
bartlett.test(data$y,data$x)	# bartlett test

plot(residuals~yhat,			# plot residuals vs fitted
		data=data,
		type="p")

outlier.test(anova)				# bonferonni-corrected, quantitiative outlier test

############################################################
#		General syntax for Repeate Measures Anova
############################################################

rmanova <-						# create repeate measures object
	aov(y~x + Error(subject), 	# 	specify model and between subjects error term
		data=data)				# 	specify data source
summary(rmanova)				# view results

library(nlme)					# non-linear mixed effects
rmanova <-						# create object
	lme(y~x, 					# 	specify model
		data=data,				# 	data source
		random=~1|ID)			#	random effects error term (intercept|ID)
anova(rmanova)					# see results
								
								# remember that RM Anova has compound symmetry assump
rmanova.cs <-					# 	we can evlauate whether this holds by comparing
	lme(y~x,					# 	models with different covariance structures
		data=data				# 	here we start with forced compound symmetry
		random=~1|ID,
		correlation=corCompSymm(form=~1|ID))
anova(rmanova.cs)

rmanova.un <-					#	here we allow correlations to vary
	lme(y~x,					# 	relaxing one assumption
		data=data,
		random=~1|ID,
		correlation=corSymm(form=~1|ID))

		weights = 				# 	add this argument to unconstrain variance 
			varIdent(
				form=~1|factor(TIME))

anova(rmanova.cs, rmanova.un)	# 	test assmuption
								# 	a significant difference indicates a violation
								# 	of the sphericity assumption

# relative efficiency as model fit evaluation
#	= [(n-1)*MSblocks]+[n(p-1*MSresid)]/[((n*p)-1)*MSresid]
#	bigger than 1, you have removed more variance than it cost in dfs
############################################################
#		General syntax for ANCOVA
############################################################

ancova <-						# create object
	aov(y~ continuous_var + x, 	# 	specify model
		data=data)				# 	specify data source
summary(ancova)					# view results