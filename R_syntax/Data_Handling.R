#################################################
#			Data manipulation
#################################################

# Read in
library(foreign)					# read in functionality
data <-
	read.spss("~/file.sav",			# SPSS 
		use.value.labels=TRUE,
		max.value.labels=Inf,
		to.data.frame=TRUE)

	read.csv("~/file.csv")
	
	read.table("~/file.csv",		# csv
		header=TRUE,
		sep=",",
		na.strings="NA",
		dec=".",
		strip.white=TRUE)
	data <- data.frame(data)
	
	read.dta("~/file.dta",			# Stata
		to.data.frame=TRUE)			

# Write out
library(foreign)
write.dta(dataset,'~/new_file.dta')	# Stata

# Subsetting
data_fac1 <- subset(data, fac == 1)	# use & for multiple grouping 
data_fac2 <- subset(data, fac == 2)	#	(data, fac==1 & x==2)

# Sort 
data <- data[order(data$x),]		# sort data on x

# Generate ID variable
data#id <- c(1:length(data$x))		# ID for every x

# Eliminate missing data
data$missingx <- is.na(data$x)		# code for missing obs
data1 <- subset(data, 				#	subset 
				data$missingx == 0)