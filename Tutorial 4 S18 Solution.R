# In the fourth tutorial we are going to practice a 2-way ANOVA with interaction
# and calculate some values of Goodness of Fit using R2.

# We are getting read of stars on our reports
options(show.signif.stars=FALSE)

# Load the car library
library(car)
# Load the effects library
library(effects)


###################################
# Multi-way ANOVA: lm() can also be used with multiple factors and interactions
# Background reading: “Models with Many Factors: Multiway ANOVA” , page 166, blue Fox book
###################################

# Let's now look at a new data set and do a 3x2 two-way ANOVA with interaction
set.seed(123456) # to reproduce the results in the text
some(Moore)  # sample 10 rows

# In the Moore data set a score of conformity measures the number of times (out of 40)
# that a subject will change their mind and agree with the partner.  The fscore and
# fcategory measures the subject’s authoritarianism, in number (fscore) and factor
# (fcategory) form. 

# This is the data size
nrow(Moore)

# Reorder the fcategory and partner.status so that “low” gets included as the 
# intercept in the linear models we will fit:
Moore$fcategory <- factor(Moore$fcategory, levels=c("low", "medium", "high"))
Moore$partner.status <- relevel(Moore$partner.status, ref="low")

# Now we’re going to look at the joint relationship between two factors. A “contingency
# table” shows how many data points are in each pair of levels, and can be created
# using the xtabs command:
xtabs(~ fcategory + partner.status, data=Moore)

# In this table, the upper right number is the number of samples that have
# fcategory=low, partner.status=low, and the lower right shows how many data
# points have fcategory=high, partner.status=high

# In the same way we can compute the number of data points for each pair of levels,
# we can compute the mean and standard deviation of conformity for each pair of levels
# This is just like a contingency table, with the number samples replaced by the
# mean (or sd) of the samples in that group.

# The mean conformity table (remember this for exercise 1.3)
with(Moore, tapply(conformity,
                   list(Authoritarianism=fcategory,
                        "Partner's Status"=partner.status),
                   mean))
# The standard deviation table
with(Moore, tapply(conformity,
                   list(Authoritarianism=fcategory,
                        "Partner's Status"=partner.status),
                   sd))

# Make a nice plot that shows the ranges of values that conformity takes
# on given each (fcategory, partner.status) combination
boxplot(conformity ~ fcategory:partner.status, data=Moore,
        xlab="Condition", ylab="conformity")
abline(v=3.5, lty="dashed")



###############################
# EXERCISES
###############################

# Do the ANOVA on the Moore dataset - first with interactions between fcategory
# and partner.status:
mod.moore.1 <- lm(conformity ~ fcategory*partner.status, data=Moore)
summary(mod.moore.1)

# Then train a model without interactions:
mod.moore.2 <- lm(conformity ~ fcategory + partner.status, data=Moore)
summary(mod.moore.2)

#############################
# EXERCISE 1
#   1) How many coefficients are in mod.moore.1, including intercept?
#   2) How many degrees of freedom (df) does the model have?
#   3) The table on the bottom of page 169 in the blue Fox book shows how to
#       convert model coefficients to the group means of (fcategory,partner.status)
#       combinations. Compute some of the table entries from the coefficients
#       of mod.moore.1. Do they match the mean conformity table you generated
#      earlier?
#############################

#1) mod.moore.1 model has 6 coefficients.
k <- length(mod.moore.1$coeff)
#2) 
# Your sample size
n <- nrow(Moore)
# More precisely if you want to only look at the data used in the model
n <- length(mod.moore.1$model$conformity)  # this is the number of y that were used to calculate the coefficients

# The degrees of freedom that are left are n-k
(df <- n-k)

#3) The means:
(mean.low.low <- mod.moore.1$coeff[1])
(mean.med.low <- mod.moore.1$coeff[1] + mod.moore.1$coeff[2])
(mean.high.low <- mod.moore.1$coeff[1] + mod.moore.1$coeff[3])
(mean.low.high <- mod.moore.1$coeff[1] + mod.moore.1$coeff[4])
(mean.med.high <- mod.moore.1$coeff[1] + mod.moore.1$coeff[2] + mod.moore.1$coeff[4] + mod.moore.1$coeff[5])
(mean.high.high <- mod.moore.1$coeff[1] + mod.moore.1$coeff[3] + mod.moore.1$coeff[4] + mod.moore.1$coeff[6])


###########################
#  EXERCISE 2
#     1) How many coefficients are in mod.moore.2, including intercept?
#     2) How many degrees of freedom (df) does the model have?
#     3) The table in the bottom of page 170 in the blue Fox book shows how to
#         convert coefficients for the no-interaction model to the group means of. Compute
#         some of the table entries from the coefficients of mod.moore.2. Do they match the
#         mean conformity table you generated earlier? Why not?
############################

#1) There are 4 coefficients for this model
(k <- length(mod.moore.2$coeff))
#2) df = n-k
n <- length(mod.moore.2$model$conformity)  # this is the number of y that were used to calculate the coefficients

# The degrees of freedom that are left are n-k
(df <- n-k)

#3) The predicted means
(mean.low.low <- mod.moore.2$coeff[1])
(mean.med.low <- mod.moore.2$coeff[1] + mod.moore.2$coeff[2])
(mean.high.low <- mod.moore.2$coeff[1] + mod.moore.2$coeff[3])
(mean.low.high <- mod.moore.2$coeff[1] + mod.moore.2$coeff[4])
(mean.med.high <- mod.moore.2$coeff[1] + mod.moore.2$coeff[2] + mod.moore.2$coeff[4])
(mean.high.high <- mod.moore.2$coeff[1] + mod.moore.2$coeff[3] + mod.moore.2$coeff[4])

# I noticed that none of the means match exactly.  This 4 parameter model
# gives me the best prediction of the means for these coefficients.

# Make an interaction plot with this data set. Your GSI can explain to you the
# syntax of this command.
with(Moore, {
  interaction.plot(fcategory, partner.status, conformity, type="b",
                   pch=c(1, 16), cex=2, ylim=range(conformity), leg.bty="o")
  points(jitter(as.numeric(fcategory), factor=0.5), conformity,
         pch=ifelse(partner.status == "low", "L", "H"))
})


#####################
# Now we’re going to look at the statistics of model parameters and residuals
######################

# We’re going to go back to the prestige model - in case you need to re-fit it,
# run this command:
prestige.mod.full <- lm(prestige ~ education*type + log2(income)*type, data=Prestige)
prestige.mod.noEd <- lm(prestige ~ log2(income)*type, data=Prestige)

############################
# EXERCISE 3
# 	For the prestige.mod.full (the ANOCOVA with interactions)
#          
# Calculate by hand (meaning with basic R commands):
# R2 and R2ajd to measures the goodness of fit of this model relative to
# the zeroth order model (the prediction for prestige given by its mean)
# Calculate by hand the corresponding F and the corresponding p value

# You will use the equations given in the lecture and you
# should also use the objects of the model.  In particular:
#  prestige.mod.full$residual are the predictions-actual
#  prestige.mod.full$model$prestige are the values of prestige used.
# 
# Compare with the values given in the summary of the model.


# You can also check this wiki page:
#	https://en.wikipedia.org/wiki/Coefficient_of_determination#Definitions
#
#
############################

# ss2 (also known as SSerror)
ss2 <- sum(prestige.mod.full$residual^2)
mean.prestige <- mean(prestige.mod.full$model$prestige)
# ss1 (also known as SStotal)
ss1 <- sum((prestige.mod.full$model$prestige - mean.prestige)^2)

(rsquare <- (ss1-ss2)/ss1)
k2 <- length(prestige.mod.full$coefficients)
k1 <- 1
n <- length(prestige.mod.full$model$prestige)

(rsquare.adj <- 1 - (ss2/(n-k2))/(ss1/(n-k1)))

(fvalue <- ((ss1-ss2)/(k2-k1))/(ss2/(n-k2)) )
# This will return the probability
pf(fvalue, k2-k1, n-k2, lower.tail = FALSE)

summary(prestige.mod.full)


############################
# EXERCISE 4
# 	Compare the prestige.mod.full to prestige.mod.noEd
#          
# Calculate by hand (meaning with R commands):
# R2 and R2ajd that you would get to quantify the increase in goodness of
# fit of the full model relative to the model without education.
# 
# Calculate the P and corresponding p-value.  Check your results using
# anova(mod1, mod2)

# ss1 
ss1 <- sum(prestige.mod.noEd$residual^2)

(rsquare <- (ss1-ss2)/ss1)
k2 <- length(prestige.mod.full$coefficients)
k1 <- length(prestige.mod.noEd$coefficients)
n <- length(prestige.mod.full$model$prestige)

(rsquare.adj <- 1 - (ss2/(n-k2))/(ss1/(n-k1)))

(fvalue <- ((ss1-ss2)/(k2-k1))/(ss2/(n-k2)) )
pf(fvalue, k2-k1, n-k2, lower.tail = FALSE)

anova(prestige.mod.noEd, prestige.mod.full)
