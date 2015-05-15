###########################################################
#		General syntax for general additive models (GAM)
###########################################################

gam <-								# create a gam object
	gam(y~s(x), data=DataSource)	# define your model, speficfy data source
									# 	s() will use gam to find "smooth" solution
									#	inside s() there should be a list of predictors
									# 	don't need to speficy interaction terms s(x,y,z)

summary(gam)						# get gam output
									#	estimated dfs, estimated F, etc

library(car)						# access for plotting

names(gam)							# access object data structures

DataSource$gamfit					# save fitted values
	<- gam$fitted.values

scatter3d(DS$x, DS$gamfit, DS$y, 	# plot them against predictors! 
	surface=FALSE, 					#	Only way to understand the result
	residuals=TRUE,					# 	Need to tweek this script
	bg="black", 
	axis.scales=TRUE, 
	grid=TRUE, 
	ellipsoid=FALSE, 
	xlab="a", 
	ylab="gamfit", 
	zlab="b")
 
 AIC(gam)							# AIC for gam model for comparison