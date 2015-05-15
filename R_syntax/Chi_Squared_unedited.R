#Chi-squared goodness-of-fit is a lot easier in R than in Excel (or in SPSS, for that matter)

chisq.test(c(9,24,17)) # c() defines a vector

chisq.test(c(9,24,17),p=c(1/3,1/3,1/3))

chisq.test(c(9,24,17),p=c(1/6,1/2,1/3))

#Let's do a two-way table and see chi-squared test for independence

table1 <- matrix(c(9,2,24,18,17,20), 3, 2, byrow=TRUE)
rownames(table1) <- c('Alone','With 1','With 2+')
colnames(table1) <- c('Control','Experimental')

table1

#Always good to look at what you've done to make sure it's what you intended.

chisq.test(table1)

#The chisq.test function automatically does independence test if the data is a two-way table.

indtest <- chisq.test(table1)

#If we do the same analysis, but save it as an object, we can see all the other stuff we can get.

names(indtest)

indtest$expected

#Like the expected frequencies -- which are the source of the warning.

table2 <- c(6,25,16,2,18,20,4,20,13,3,13,10)

#Now we're gonna make a 3-way table.  Save this syntax -- I guarantee it'll save you a lot of pain versus trying to re-create this yourself later.

dim(table2) <- c(3,2,2) 
#dim is dimension - 3 rows, 2 columns, 2 sets

dimnames(table2) = list ( c('Alone','With 1','With 2+'), c('Control','Experimental'), c('Male','Female'))

names(dimnames(table2)) <- c('preference','group','gender')

#So far, what we've done is created a 3-way array in R, but to do the analysis we want, we need R to understand it as a table.

table2 <- as.table(table2)

#This effects that conversion.

library(MASS)

#The function we want is in the MASS package.

saturated.model <- loglm(~preference*group*gender, data=table2)

#Leave left hand side of tilda blank, R assumes your outcome is counts
#This gives us the starting point -- which is a saturated model -- for the next command.

step(saturated.model, direction="backward")

#This works backward, eliminating non-signifcant terms according to the AIC starting with the 3-way interaction and working backward through the 2-ways, etc.
#It stops with all three simple terms, which is decidedly uninteresting.

# Reading the output…
# highest three way - AIC drops so we can remove it
# go to two ways - AIC drops the most for gender*preference, but drops all
# keep going down
# last figure states that AIC starts at 13.968, removing each term makes AIC go up
# Therefore we cannot remove them

big <- loglm(~preference+group+gender, data=table2)
small <- loglm(~gender+preference, data=table2)
anova(big,small)

#Keep in mind that the AIC is more liberal than the standard p<.05 threshold.
#In addition to showing that, this shows us how we directly compare two nested models in R. 

