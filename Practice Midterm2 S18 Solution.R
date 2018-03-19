# Midterm 2 (Practive). Spring 18.
# PSYCH 205 UC Berkeley
# Theunissen 

# -----------------------
# Name: YOUR NAME HERE
# SSID: YOUR SSID HERE
# -----------------------

# The quiz is open everything: book, internet, tutorials, etc.
# Upon completion of the exam, please upload your script to bCourses.

# Load the car library
library(car)

# EXERCISE 1
# ------------------------------------------------------------------------------
# We will be working with the ToothGrowth dataset. This dataset contains the length of
# the teeth of Guinea Pigs versus the dose of vitamin C. The vitamin C is delivered
# as orange juice (OJ) or ascorbic acid (VC). 


?ToothGrowth  # For additional information on the ToothGrowth dataset
ToothGrowth  # Print dataframe to the console

# You want to investigate how vitamin c (dose and delivery method) affect lenght. 
# In other words, len is your response variable and supp and dose are the predictors.

# Exercise 1. (1 point).  Visualize the data. Use the scatterplot command to
# make a scatterplot of the data with one line for each of the delivery methods. Perform a two-way anova (with interaction) to evaluate 
# What is the sample size?

scatterplot(len~dose|supp, data=ToothGrowth)
(nvals <- nrow(ToothGrowth))

# Exercise 2. (2 points).  Test the predictive power of three nested models that correspond
# to a single line, two lines with different intercepts and two lines with different intercepts and slopes.
# Type in the commands to make these models and using the summary() command comment on the results.  
# You comments should address both the values of the model coefficients and of the measures of goodness of fit.
# Type in your comments as "comments" in this R script below the R commands.

mod.1line <- lm(len ~ dose, data=ToothGrowth)
mod.2plines <- lm(len ~ dose + supp, data=ToothGrowth)
mod.2lines <- lm(len ~ dose*supp, data=ToothGrowth)

(mod.1line.sum <- summary(mod.1line))
(mod.2plines.sum <- summary(mod.2plines))
(mod.2lines.sum <- summary(mod.2lines))

# Increase the dose of vitamin C increases the lenght of teeth since
# the slope is positive and significant.

# Exercise 3. (1 point).  How many parameters are there in each of the three models?
# Make a line plot of model parameters versus R2adj. You can get R2adj from
# the summary commands.

# 2,3,4

n.p <- c(2,3,4)
R2.adj <- c(mod.1line.sum$adj.r.squared, mod.2plines.sum$adj.r.squared, mod.2lines.sum$adj.r.squared)
plot(n.p, R2.adj, type = "b", ylim = c(0.0, 1.0))
# plot(n.p, R2.adj, type = "b")

# Exercise 4. (2 points).  Calculate the R2, R2 adj, F and p value for the full model "by hand".
# Make sure that the results match those you obtained from the summary command.

k1 <- 1;
k2 <- length(mod.2lines$coefficients)
n <- length(mod.2lines$fitted.values)
sserror <- sum(mod.2lines$residual^2)
mean.len <- mean(mod.2lines$model$len)
sstotal <- sum((mod.2lines$model$len - mean.len)^2)

(adj.rsquare <- 1 - (sserror/(n-k2))/(sstotal/(n-k1)))

(dfmodel <- k2-k1)
(dferror <- n-k2)
(fvalue <- ((sstotal-sserror)/dfmodel)/(sserror/dferror))

# This will return the probability
pf(fvalue, dfmodel, dferror, lower.tail = FALSE)


# Exercise 5. (2 points).  Using 10 fold cross-validation estimate the cross-validated
# R2 for the three models.  Make a plot of average R2CV versus number of parameters.

nCV <- 10
Rvals.CV.1line <- numeric(nCV)
Rvals.CV.2plines <- numeric(nCV)
Rvals.CV.2lines <- numeric(nCV)

folds <- cut(sample(nvals),breaks=nCV,labels=FALSE)

#Perform n.folds fold cross validation
for(i in 1:nCV){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- ToothGrowth[testIndexes, ]
    trainData <- ToothGrowth[-testIndexes, ]
    
    # The three models
    mod.1line.CV <- lm(len ~ dose, data=trainData)
    mod.2plines.CV <- lm(len ~ dose + supp, data=trainData)
    mod.2lines.CV <- lm(len ~ dose*supp, data=trainData)
    
    # Get predictions
    pred.1line <- predict(mod.1line.CV, newdata = testData)
    pred.2plines <- predict(mod.2plines.CV, newdata = testData)
    pred.2lines <- predict(mod.2lines.CV, newdata = testData)
    mean.cv <- mean(testData$len)

    # Calculate R2
    sstotal <- sum((testData$len - mean.cv)^2)
    sserror.1line <- sum((testData$len - pred.1line)^2)
    sserror.2plines <- sum((testData$len - pred.2plines)^2)
    sserror.2lines <- sum((testData$len - pred.2lines)^2)
    
    Rvals.CV.1line[i] <- (sstotal-sserror.1line)/sstotal
    Rvals.CV.2plines[i] <- (sstotal-sserror.2plines)/sstotal
    Rvals.CV.2lines[i] <- (sstotal-sserror.2lines)/sstotal
}

R2.CV <- c( mean(Rvals.CV.1line), mean(Rvals.CV.2plines), mean(Rvals.CV.2lines) ) 
R2.CV.SE <- c( sd(Rvals.CV.1line)/sqrt(nCV), sd(Rvals.CV.2plines)/sqrt(nCV), mean(Rvals.CV.2lines)/sqrt(nCV) ) 
plot(n.p, R2.CV)

library(Hmisc)
errbar(n.p, R2.CV, R2.CV+R2.CV.SE, R2.CV-R2.CV.SE, add=T, pch=1, cap=.1)


# Exercise 6. (2 points).  First, use the appropriate classical commands to statiscally compare the single line model to
# the full model. Second, using the results of the cross-validation in 5, determined whether you would reach the same conclusion. 
# For example compare the mean R2cv plus or minus two standard errors to assess whether one is significantly
# better.

anova(mod.1line, mod.2lines)

print(sprintf('Rcv 1 Line %.3f +- %.3f', mean(Rvals.CV.1line), 2.0*sd(Rvals.CV.1line)/sqrt(nCV) ))
print(sprintf('Rcv 1 Line %.3f +- %.3f', mean(Rvals.CV.2lines), 2.0*sd(Rvals.CV.2lines)/sqrt(nCV) ))

# I notice that the cross-validated R2 are lower and that the estimates
# overlap.  Altought the classical statistical test tells me that the full model
# is better than the 1 line model, cross validation fails to find a significant difference.
# This could be in part due to the effect of uneven sample sizes.

# Don't forget to eat your fruits and vegetables!
