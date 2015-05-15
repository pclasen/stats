table2 <- c(6,25,16,2,18,20,4,20,13,3,13,10)

#Now we're gonna make a 3-way table.  Save this syntax -- I guarantee it'll save you a lot of pain versus trying to re-create this yourself later.

dim(table2) <- c(3,2,2)

dimnames(table2) = list ( c('Alone','With 1','With 2+'), c('Control','Experimental'), c('Male','Female'))

names(dimnames(table2)) <- c('preference','group','gender')

#So far, what we've done is created a 3-way array in R, but to do the analysis we want, we need R to understand it as a table.

table2 <- as.table(table2)

#This effects that conversion.

library(MASS)

saturated.model <- loglm(~preference*group*gender, data=table2)
reduced.model <- loglm(~preference*group*gender-preference:group:gender, data=table2)

# "-preference:group:gender" removes the three-way interaction

anova(saturated.model,reduced.model)

# comparing these models (Delta(dev) represents chi2 = 1.154446, p = 0.56146 on 2 df)
# THIS WAS THE loglm method, now we will try the multinomial_logistic_regression version ...

#This is a bit of follow-up using last week's table.
#The difference between these two models is the 3-way interaction on counts, so this anova is a test of its significance.
#More on that later.

Dataset <- read.table("/Users/petercclasen/Desktop/Peter/Statistics/AdvancedStats2/aas2_movpref_long.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

fix(Dataset)

Dataset$fmovpref <- Dataset$movpref
Dataset$fmovpref <- as.factor(Dataset$fmovpref)
#As I may have noted, in R you must explicitly declare factor variables.

#multinom is in the nnet library

library(nnet)

MLM.full <- multinom(fmovpref~group*gender, data=Dataset, trace=F) #trace gives likelhood at each iteration of the maximum likelihood
summary(MLM.full)

# Output is a set of coefficients for a logistic regression model - relative probabilies

step(MLM.full, direction="backward")

#Starting with a full model and then using the step command to work backwards from there.
#Remember, the step function uses the AIC, which will be a little more liberal than p<.05.
#That said, nothing comes out significant.
#We knew that from before, when nothing interacted with movpref in influencing counts.

MLM.reduced <- multinom(fmovpref~group+gender, data=Dataset, trace=F)
summary(MLM.reduced)

anova(MLM.full,MLM.reduced)

#This implements an explicit test of whether the group:gender interaction influences movpref.
#Notice that it is the EXACT same p value as the loglinear test for the group:gender:movpref interaction on counts.

# The three way interaction on counts is the same as the two way interaction when you divide counts into IVs and DVs - interactions between counts is the same as a main effect of one thing on the other

MLMpredictedarray <- predict(MLM.full,type="p")

#The predict function saves the predicted probabilities based on the full model.  It saves the predicted probabilities in each category. Unfortunately, it saves them as an array, which requires a bit more work.

Dataset$MLMpredicted.1 <- MLMpredictedarray[,1]
Dataset$MLMpredicted.2 <- MLMpredictedarray[,2]
Dataset$MLMpredicted.3 <- MLMpredictedarray[,3]

#Some older functions save predicted values as an array, which means you need to extract components one column at a time. When we pull up fix(Dataset) we can view these category probabilities.

MO <- read.table("/Users/petercclasen/Desktop/Peter/Statistics/AdvancedStats2/aas2_multinomial_ordinal_data.csv", 
  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

fix(MO)

# Simple measure of political conservatism (0-100) and a party affiliation category (1 = Dem, 2 = Ind, 3 = Rep)

MO$party <- MO$party
MO$party <- as.factor(MO$party)
# make the party variable categorical "as.factor()"

MLM.full <- multinom(party~cons, data=MO, trace=F)
summary(MLM.full)

MLM.reduced <- multinom(party~1, data=MO, trace=F)
summary(MLM.reduced)

anova(MLM.full,MLM.reduced)

synth_MO <- read.table("/Users/petercclasen/Desktop/Peter/Statistics/AdvancedStats2/aas2_synth_mo.csv", header=TRUE, sep=",", 
  na.strings="NA", dec=".", strip.white=TRUE)

MLMpredictedarray <- predict(MLM.full,synth_MO,type="p")

#This is the really, really cool thing about the predict function.  You can apply the coefficients of one model to a new set of data.
#In this case, our new set of data contains all the possible values from 0 to 100 for cons.
#By contrast, in SPSS you can only get predicted values (easily) for the IV values actually in your data.  Getting the rest requires some manual intervention.

#Notice too that I'm re-using the same object name as above.  That's a little sloppy, but I already got what I needed out and saved where I needed it.

synth_MO$MLMpredicted.1 <- MLMpredictedarray[,1]
synth_MO$MLMpredicted.2 <- MLMpredictedarray[,2]
synth_MO$MLMpredicted.3 <- MLMpredictedarray[,3]

#This is WAAAAAAAAAAAY easier than taking the SPSS output and working through the equations to get predicted probs.

#The function polr -- for ordinal regression -- is in the MASS library.

library(MASS)

OrdReg.reduced <- polr(party~1, method="logistic", data=MO, Hess=TRUE)
summary(OrdReg.reduced)

OrdReg.full <- polr(party~cons, method="logistic", data=MO, Hess=TRUE)
summary(OrdReg.full)

anova(OrdReg.full,OrdReg.reduced)

AIC(MLM.full)
AIC(OrdReg.full)

#Easiest way to test parallel lines is to compare the two models.  Anova won't work, but AIC does.
#This is recommended by Venables and Ripley (authors of MASS, one of the S and R landmark texts).

synth_MO$prob1 <- synth_MO$MLMpredicted.1
synth_MO$prob1or2 <- synth_MO$MLMpredicted.1+synth_MO$MLMpredicted.2

#The need for this form is occasioned by the fact that, despite appearances, the predicted probs are not regular variables.
#They are columns in arrays.  Many older functions work this way.

synth_MO$logodds1 <- log(synth_MO$prob1/(1-synth_MO$prob1))
synth_MO$logodds1or2 <- log(synth_MO$prob1or2/(1-synth_MO$prob1or2))

#Note: In R, the log function is the natural log by default.
# convert the probabilities to log(odds) - odds = prob/(1-prob)
# These log odds should be parallel to fit the assumption that we can treat the data as ordinal

fix(synth_MO)

#May need to issue this to make newly-created variables available in R Commander.

matplot(synth_MO$cons, synth_MO[, c("logodds1","logodds1or2")], type="l", 
  lty=1, ylab="(1) logodds1, (2) logodds1or2")

#These lines are very parallel, confirming what we saw with AIC.

# across levels of IV, the relationship between IV and response categories, even when they are not forced to be parallel, end up being parallel. This is good test of assumption. Doing the AIC comparison is a good adjunct or alternative.



