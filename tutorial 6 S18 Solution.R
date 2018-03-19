#############################################################
##  In this tutorial, we are going to learn to assess the 
## goodnes of fits of models and to compare models
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

# Some of the rows have missing data.  Since we are going to compare models using
# our own functions, we are going to eliminate rows with NA.
is.na(Prestige)

Prestige.clean <- Prestige[rowSums(is.na(Prestige)) == 0,]
# Our favorite model is an ANOCOVA where type as a factor and education and log(income) as covariates  
prestige.mod.full <- lm(prestige ~ education*type + log2(income)*type,
                      data=Prestige.clean)
(prestige.mod.full.sum <- summary(prestige.mod.full))

prestige.mod.ed <- lm(prestige ~ education, data=Prestige.clean)
(prestige.mod.ed.sum <- summary(prestige.mod.ed))

aov.results <- anova(prestige.mod.ed, prestige.mod.full)

# The partial R2
(R2.ed.vs.full <- 1 - (aov.results$RSS[2]/aov.results$Res.Df[2])/(aov.results$RSS[1]/aov.results$Res.Df[1]))

######################  Model Validation with Resampling #########
# Cross-validation with bootstrap estimates of R2.
# We are now going to perform a 10-fold cross validation on the prestige
# data frame to get our own value of R2.

# First we randomly shuffle the data and omit na rows.
n.folds <- 5

set.seed(12345)
my.Prestige <-na.omit(Prestige[sample(nrow(Prestige)),])

# Next, we create 10 equally size folds
folds <- cut(seq(1,nrow(my.Prestige)),breaks=n.folds,labels=FALSE)

########################################################
#   Exercise 1.  Complete the following loop to get n.folds value
# of cross validated R2 (one for each fold) for the full model and the model with Education only compared to
# the zeroth order model and the full model compared to the education model.  
# Compare the mean of those cv-R2 to the adjusted R2 you got in the summary
# or to the adjusted R2 you used to calculate the error decrease
# going from model 1 to model 2.


# Make space for your arrays
R2.cv.full <- array(data=0, dim = n.folds)
R2.cv.ed <- array(data=0, dim = n.folds)
R2.cv.ed.vs.full <- array(data=0, dim=n.folds)

#Perform n.folds fold cross validation
for(i in 1:n.folds){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- my.Prestige[testIndexes, ]
    trainData <- my.Prestige[-testIndexes, ]
    
    #Your code here
    # Fit the two models 
    prestige.cvmod.full <- lm(prestige ~ education*type + log2(income)*type,
                      data=trainData)
    prestige.cvmod.ed <- lm(prestige ~ education, data=trainData)
     
    prestige.mean.train <- mean(trainData$prestige)

    # Get predictions
    prestige.pred.full <- predict(prestige.cvmod.full, newdata = testData)
    prestige.pred.ed <- predict(prestige.cvmod.ed, newdata = testData)

    # Calculate cv R2.
    ss2 <- sum((testData$prestige - prestige.pred.full)^2)
    ss1 <- sum((testData$prestige - prestige.pred.ed)^2)
    ss0 <- sum((testData$prestige - prestige.mean.train)^2)
    
    R2.cv.full[i] <- 1 - (ss2/ss0)
    R2.cv.ed[i] <- 1 - (ss1/ss0)
    R2.cv.ed.vs.full[i] <- 1 - (ss2/ss1)
}

# More summary code here
(sprintf('Full Model: R2 cv = %.2f +- %.3f versus R2 adj = %.2f', 
    mean(R2.cv.full), sd(R2.cv.full), 
    prestige.mod.full.sum$adj.r.squared) )
(sprintf('Ed Model: R2 cv = %.2f +- %.3f versus R2 adj = %.2f', 
    mean(R2.cv.ed), sd(R2.cv.ed), 
    prestige.mod.ed.sum$adj.r.squared) )
(sprintf('Ed vs Full Model: R2 cv = %.2f +- %.3f versus R2 adj = %.2f', 
    mean(R2.cv.ed.vs.full), sd(R2.cv.ed.vs.full), 
    R2.ed.vs.full) )

boxplot(cbind(R2.cv.full,R2.cv.ed,R2.cv.ed.vs.full))

# The R2 cv is very similar to the R2ajd - slightly smaller. 
# From the SE of the R2 cv, I see that both are significantly above zero. 
# The full model is better that the model with just education but the effect is smaller than
# estimated with the non-crossvalidated result (and that comparison is not significant!)

#  END of Exercise 1.

####################################
#  Bootstrap example using the boot package.
#  Here we use it to estimate errors in regression coefficients
#
#####################################
library(boot)

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 

results <- boot(data=Prestige.clean, statistic=bs, 
  	R=1000, formula=prestige~education)

# Print results from bootstrap and compare to classical approach
results
prestige.mod.ed.sum

# Plot distribution of values
plot(results, index=1) # intercept 
plot(results, index=2) # education 

# get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # education 

###################################
# Permutation test example from Text book. 
###################################
# Our second example is the permutation test from the book.
# In this case we are interrested in determining whether a model for salary prediction fitted on men works for women as well.
library(car)
set.seed(12345) # to reproduce results in the text

# Let's look at the data format
some(Salaries)
nrow(Salaries)

# let's relevel the factor rank
Salaries$rank <- relevel(Salaries$rank, ref="AsstProf")

# Making a table to see how many subjects in each category
# xtabs make a table and ftable "flattens" the table
ftable(x1 <- xtabs(~ discipline + rank + sex, data=Salaries))
round(100*ftable(prop.table(x1, margin=c(1, 2))), 1) # % m and f

# We are plotting the data
library(lattice)
xyplot(salary ~ yrs.since.phd | discipline:rank, group=sex,
  data=Salaries, type=c("g", "p", "r"), auto.key=TRUE)

bwplot(salary ~ discipline:sex | rank, data=Salaries,
    scales=list(rot=90), layout=c(3, 1))

# Generate a model for Males only
fselector <- Salaries$sex == "Female" # TRUE for females
salmod <- lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
    subset=!fselector) # regression for males
    
# predictions for females:
femalePreds <- predict(salmod, newdata=Salaries[fselector, ])
(meanDiff <- mean(Salaries$salary[fselector] - femalePreds))

# Let's do a bootstrap to see how often we could get this result
set.seed(8141976) # for reproducibility
fnumber <- sum(fselector) # number of females
n <- length(fselector) # number of observations
B <- 999 # number of replications
simDiff <- numeric(B) # initialize vector with B entries
for (j in 1:B){
    sel <- sample(1:n, fnumber) # random sample of nominated 'females'
    m2 <- update(salmod, subset=-sel) # refit regression model
    simDiff[j] <- mean(Salaries$salary[sel]
        - predict(m2, newdata=Salaries[sel, ])) # compute mean diff.
    }
    
# Calculate the p-value    
(frac <- round(sum(meanDiff > simDiff)/(1 + B), 3))

# Plot the histogram
hist(simDiff,
   main=paste("Histogram of Simulated Mean Differences\np-value =",
       frac),
   xlab="Dollars")
abline(v=meanDiff, lty="dashed")



#######################################
# Our last example examines overfitting.
#######################################

# We are now going to do an example with data from the Theunissen lab.
# In the data, we quantify the "timbre" of a musical instrument with the timbre
# column, and various acoustic features of the sound in the columns labeled
# sound.1, sound.2, etc. 

# First, change this path to the path where the file mds1PCA40.txt lives:
setwd('/Users/frederictheunissen/Documents/Classes/Psych 205/Spring 2018')

# Then read the file and print out the number of rows
Timbre <- read.table('mds1PCA40.txt')
(n.inst <- nrow(Timbre))

# Check out a summary of the data. It's all numerical! 
summary(Timbre)

######################################
# EXERCISE 2
# Use Scatter plot matrix to visualize the data.  Use only the first 3 sound features, specifically
# sound.1, sound.2, and sound.3.
######################################
scatterplotMatrix(~ timbre + sound.1 + sound.2 + sound.3, data=Timbre, id.n=1, smooth=TRUE)

### End of 2


# We are now going to predict timbre and keep track of R-square values as a function of the number of parameters.

Rvals <- numeric(40)
Rvals.adj  <- numeric(40)
timbre.mod <- lm(timbre ~ sound.1, data=Timbre)
(timbre.sum <- summary(timbre.mod))
Rvals[1] <- timbre.sum$r.squared
sserror <- sum(timbre.mod$residual^2)
mean.timbre <- mean(timbre.mod$model$timbre)
sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)
dferror <- timbre.mod$df.residual  # n - k -1
dftotal  <- length(timbre.mod$fitted.values)-1 # n - 1
Rvals.adj[1]  <- 1- ((sserror/dferror)/(sstotal/dftotal))

i <- 2
timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
timbre.sum <- summary(timbre.mod)
Rvals[i] <- timbre.sum$r.squared
sserror <- sum(timbre.mod$residual^2)
dferror <- timbre.mod$df.residual  # n - k -1
Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))


################################################
# EXERCISE 3
# Compute and plot the R2 (obtained from the model) and the adjusted R square (as a red line) as a function of the number of sound parameters
# used to model timbre perception.  To do this, you can use the code above inside of a
# for loop, which iterates 40 times because there are 40 sound predictors.
################################################

for (i in 3:40) {
  timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
  timbre.sum <- summary(timbre.mod)
  Rvals[i] <- timbre.sum$r.squared	
	sserror <- sum(timbre.mod$residual^2)
	dferror <- timbre.mod$df.residual  # n - k -1
	Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))
	}
	
plot(1:40,Rvals,type='l', xlab='Number of Parameters', ylab='R-Square')	
lines(1:40, Rvals.adj, col='red')

################################################
# EXERCISE 4
# Calculate by "hand" the R-square obtained for a model with 3
# sound parameters. Hint: You have done this on your previous tutorial. Use
# the $residuals of the model timbre ~ sound.1 + sound.2 + sound.3.
###############################################

timbre.mod <- lm(timbre ~ sound.1 + sound.2 + sound.3, data=Timbre)
(timbre.sum <- summary(timbre.mod))
sserror <- sum(timbre.mod$residual^2)
mean.timbre <- mean(timbre.mod$model$timbre)
sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)

(rsquare <- (sstotal-sserror)/sstotal)

###############################################
# EXERCISE 5
# Using the cross-validation example above and a model with 3 sound parameters:
#  1) Randomly divide the sound instruments into a "fitting" and "validation" data set. (Use only one fold for simplicity)
#  2) Fit a linear model on the "fitting" data set.
#  3) Predict the timbre of the data points in the "validation" data set using the linear
#     model you fit on the "fitting" data set, report the R2. Hint: use the predict(..) function
#     with data = the fitting data set.
#  4) Repeat 1-3 for models that include an increasing number of sound features. The exercise
#     started with a model trained on 3 features, but combine the code you wrote to do 1-3 with
#     code from exercise 3 to loop through models that increase in the number of sounds they
#     utilize as predictors. The goal is to determine the "generalization" performance of each
#     model. Generalization performance is defined here as the R2 of model predictions on the "validation"
#     data set, when that model has been trained on the "fitting" data set.
#
# Suggestion: use 10 data ponts for a validation data set but note that you will then have n-10 rows
# in your data set for fitting model parameters.
###############################################


nvals  <- 10
nparam.max  <- n.inst-nvals-1
Rvals.validation <- numeric(nparam.max)
sel <- sample(1:n.inst, nvals)  # Select 10 for validation data set

timbre.mod <- lm(timbre ~ sound.1, data=Timbre, subset=-sel)
sserror <- sum((Timbre$timbre[sel]-predict(timbre.mod, newdata=Timbre[sel,]))^2)
mean.timbre = mean(Timbre$timbre[sel])
sstotal <- sum((Timbre$timbre[sel]-mean.timbre)^2)
Rvals.validation[1] <- (sstotal-sserror)/sstotal

for (i in 2:nparam.max) {
	timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
	sserror <- sum((Timbre$timbre[sel]-predict(timbre.mod, newdata=Timbre[sel,]))^2)

Rvals.validation[i] <- (sstotal-sserror)/sstotal
	}
lines(1:nparam.max, Rvals.validation, lty='dashed')

