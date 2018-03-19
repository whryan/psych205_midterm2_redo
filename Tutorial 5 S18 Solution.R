#############################################################
##  In this tutorial, we are going to learn to assess the 
## goodnes of fits of models and to compare models.
## We are going to do this both with classical statistics (Anova) and
## with modern cross-validation approaches.

# We are now going to practice doing the statistics for linear models using the F-statistic.
library(car)

# Remember the job prestige data has a mixture of numeric predictors (income, education, women) and factors (group)

# Always take a peak at the data and look at the sample size
head(Prestige)
nrow(Prestige)

# Always visualize your data
scatterplotMatrix(~ prestige + log2(income) + education + women | type, data=Prestige, by.group=TRUE, id.n=0,
                  smooth=FALSE, col=gray(c(0,0.5,0.7)))

# Our favorite model is an ANOCOVA where type as a factor and education and log(income) as covariates  
prestige.mod <- lm(prestige ~ education*type + log2(income)*type,
                      data=Prestige)
summary(prestige.mod)


#######################################
# Type I and II ANOVA
#######################################

# The anova command (fom the stats library) shows sequential F values. This hypothesis
# testing is sometimes called a Type I anova. In a Type I ANOVA, also called a sequential
# ANOVA, a series of models are fit, and the *difference* in sum-of-square error between
# each model is reported in the summary. Examine the table that's printed out after
# running a sequential ANOVA:

anova(prestige.mod)

# The "Sum Sq" column in the table above is *difference* in model sum-of-squares (SSerr) for several
# different models. The first row is a model that just predicts prestige from education,
# which can be written (prestige ~ education). The "Sum Sq" column for this row shows
# the sum-of-squares error of a model that just uses the overall mean, also called SStotal,
# minus the sum-of-squares error of the model prestige ~ education.
# Likewise, the second row shows the difference in the sum-of-squares error between
# a the model prestige ~ education and the model prestige ~ education + type.
# Check out section 4.4.3 of the blue Fox book for more information.

# Sequential F values are "out-of-favor" ... It is better to look at type II anovas.  The function
# Anova() - with a capital A (from the car library) performs these tests.  In these tests, a model
# that includes all regressors is compared to a model that includes all other regressors but one. (It
# is a bit more complicated when interactions are involved)
#
# For more information on Type I/II/III ANOVA, check out sec 4.4.4 of blue Fox or this explanation:
#    https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/
Anova(prestige.mod)

# An ANOVA procedure can be used to test the significance of any two models (two alternative hypotheses)
# Here we'll compare the model prestige ~ education + log2(income) + type to a model with only
# an intercept term
prestige.mod.1 <- lm(prestige ~ education + log2(income) + type, data=na.omit(Prestige)) # full model
prestige.mod.0 <- update(prestige.mod.1, . ~ 1) # intercept only
anova(prestige.mod.0, prestige.mod.1) # compare models

#########################################
# EXERCISE 1
#
# Where else would you find the same F value as the one in the anova command above? Check to see if it is the same. Hint: the F value
# output of ANOVA is for comparison of the model prestige ~ education + log2(income) + type to a
# model with only an intercept. What do you know about the output of summary(prestige.mod.1)?
#########################################

# This F value is the same as the overall F test that you get from the summary of the complete model
summary(prestige.mod.1)

### End of Exercise 1.

# One can also test the effect of one additional parameter
prestige.mod.0inc <- update(prestige.mod.1, . ~ . - log2(income))
anova(prestige.mod.0inc, prestige.mod.1) # compare models


#########################################
# EXERCISE 2
#
# What does this F value correspond to?  Give two other ways of
# obtaining it. One way is to explore the output of a Type II ANOVA using
# on prestige.mod.1. Which model comparison does the matching F value
# correspond to?
#########################################

#You can get this F value by the Anova() function and looking at the entry for log2(income) and it is also the square of the t value in the Wald Test.
Anova(prestige.mod.1)
(prestige.mod.1.sum <- summary(prestige.mod.1))

(prestige.mod.1.sum$coefficients[3,3])^2

### End of exercise 2

# More generally you can use the anova command to compare two nested models
prestige.mod.1 <- lm(prestige ~ education + log2(income) + type,
    data=na.omit(Prestige)) # full model
prestige.mod.ed  <- lm(prestige ~ education, data=na.omit(Prestige))
(prestige.mod.ed.sum <- summary(prestige.mod.ed))

anova(prestige.mod.ed, prestige.mod.1)

######################  Done with Classical Model Validation #########

