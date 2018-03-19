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

tooth = ToothGrowth

# You want to investigate how vitamin c (dose and delivery method) affect lenght. 
# In other words, len is your response variable and supp and dose are the predictors.

# Exercise 1. (1 point).  Visualize the data. Use the scatterplot command to
# make a scatterplot of the data with one line for each of the delivery methods. 
summary(tooth$len)
table(tooth$supp)
table(tooth$dose)

scatterplotMatrix(tooth)

#Perform a two-way anova (with interaction) to evaluate 
twoway = lm(len ~ supp*dose, data=tooth)
summary(twoway)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   11.550      1.581   7.304 1.09e-09 ***
#  suppVC        -8.255      2.236  -3.691 0.000507 ***
#  dose           7.811      1.195   6.534 2.03e-08 ***
#  suppVC:dose    3.904      1.691   2.309 0.024631 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 4.083 on 56 degrees of freedom
#Multiple R-squared:  0.7296,	Adjusted R-squared:  0.7151 

#All terms are significant, suggesting they are related
#the reference level appears to be Supp OJ 
#taken as a whole, VC decreases tooth length relative to OJ all else equal
# interacting with dose, VC has an estimate of 3.9 as opposed to the normal dose
#slope of 7.8 - implying again that even in the interaction VC is providing less of 
#an increase in length as dose increases
#dose is positively related to tooth length. 


# What is the sample size?
nrow(tooth)
#60

# Exercise 2. (2 points).  Test the predictive power of three nested models 
#that correspond to a single line, 
#two lines with different intercepts and 
#two lines with different intercepts and slopes.

# Type in the commands to make these models and using the summary() command comment on the results.  
# You comments should address both the values of the model coefficients and of the measures of goodness of fit.
# Type in your comments as "comments" in this R script below the R commands.

#single line
mod1 = lm(len ~ supp, data=tooth)
summary(mod1)
#R^2 and adjR2 of .06 and .04 respectively -- this is very low
#Furher, supp isn't actually significant in this model, though it is close
# with a .06 p value. This does imply that it may not be the strongest/most 
#explanator relationship
#the reference level appears to be Supp OJ 
#VC appears to slightly decrease tooth length, tho this relationship is not significant

#two lines, diff intercept
mod2 = lm(len ~ supp + dose, data=tooth)
summary(mod2)
#both R2 and adjR2 increase massivly here -- going to .7 and .69 respectively
# This indicates that adding dose means our model  explains much more of the
#variability in length of teeth
#and both are higher than the single term model, so we aren't being punished so much
#for additional parameters that this isn't a benefit - it is
#Both dose and supp are significant, dose extremely significant with a much higher 
#coefficient. This seems to imply that dose was a very important term for doing 
#accurate predictions
#the reference level appears to be Supp OJ 
#VC decreases tooth length relative to OJ all else equal
# increased dose increases tooth length

#two lines, diff intercept and slope
mod3 = lm(len ~ supp*dose, data=tooth)
summary(mod3)
#R2 and adjR2 increased - despite additional terms, it is still higher than the
#model, meaning more variability is explained even when adjusted for the additional 
#terms. All terms are significant, suggesting they are related
#the reference level appears to be Supp OJ 
#taken as a whole, VC decreases tooth length relative to OJ all else equal
# interacting with dose, VC has an estimate of 3.9 as opposed to the normal dose
#slope of 7.8 - implying again that even in the interaction VC is providing less of 
#an increase in length as dose increases
#dose is positively related to tooth length. (same description as in previous q)

#Comparing adjusted R^2, it increases from model1 -> model2 -> model3. 
#So, it appears that even with the additional parameters, adding these terms is still
#making this regression more accurate


# Exercise 3. (1 point).  How many parameters are there in each of the three models?
# Make a line plot of model parameters versus R2adj. You can get R2adj from
# the summary commands.

m1len = length(coef(mod1))
#2
m2len = length(coef(mod2))
#3
m3len = length(coef(mod3))
#4
m1r2 = summary(mod1)$adj.r.squared
m2r2 = summary(mod2)$adj.r.squared
m3r2 = summary(mod3)$adj.r.squared

plot(c(m1len,m2len,m3len),c(m1r2,m2r2,m3r2), type="l")

# Exercise 4. (2 points).  Calculate the R2, R2 adj, F and p value for the full model "by hand".
# Make sure that the results match those you obtained from the summary command.

# GOALS
#Multiple R-squared:  0.7296,	Adjusted R-squared:  0.7151 
#F-statistic: 50.36 on 3 and 56 DF,  p-value: 6.521e-16

#R2
#get null model
mod0 = lm(len ~ 1, data=tooth)
tooth_hat_full = predict(mod3)
tooth_hat_null =  predict(mod0)

ss3 = sum((tooth$len - tooth_hat_full)^2)
ss0 = sum((tooth$len - tooth_hat_null)^2)

R2 = 1 - (ss3/ss0)
print(R2)
#0.7295544

#Adjusted R2
n = length(mod3$model$len)
k = length(mod3$coefficients)

R2adj = 1 - (ss3/(n-k))/(ss0/(n-1))
print(R2adj)
#0.7150662

# F value
F_val = ((ss0 - ss3)/(k-1))/((ss3)/(n-k))
print(F_val)
# 50.35522

require(stats)
pfval = 1 - pf(F_val, df1 = (k-1), df2 = n-k)
# 6.661338e-16

#All results match up

# Exercise 5. (2 points).  Using 10 fold cross-validation estimate the cross-validated
# R2 for the three models.  Make a plot of average R2CV versus number of parameters.

#convert name so it's more general
df = tooth

#find the number of folds
n_folds = 10

#get rid of nas, shuffle data
df <-na.omit(df[sample(nrow(df)),])

#create folds
folds <- cut(seq(1,nrow(df)),breaks=n_folds,labels=FALSE)

#create arrays for results
R2cv_mod1 = array(data=0, dim=n_folds)
R2cv_mod2 = array(data=0, dim=n_folds)
R2cv_mod3 = array(data=0, dim=n_folds)

#for loop
for(i in 1:n_folds){
  
  #split up data
  test = df[folds==i,]
  train = df[folds !=i,]
  
  #fit models
  mod0 = lm(len ~ 1, data=train)
  mod1 = lm(len ~ supp, data=train)
  mod2 = lm(len ~ supp + dose, data=train)
  mod3 = lm(len ~ supp*dose, data=train)
  
  yhat_mod0 = predict(mod0, data=test)
  yhat_mod1 = predict(mod1, data=test)
  yhat_mod2 = predict(mod2, data=test)
  yhat_mod3 = predict(mod3, data=test)
  
  ss0 = sum((yhat_mod0 - test$len)^2)
  ss1 = sum((yhat_mod1 - test$len)^2)
  ss2 = sum((yhat_mod2 - test$len)^2)
  ss3 = sum((yhat_mod3 - test$len)^2)
  
  R2cv_mod1[i] = 1-ss1/ss0
  R2cv_mod2[i] = 1-ss2/ss0 
  R2cv_mod3[i] = 1-ss3/ss0
  
}

meanR2_1 = mean(R2cv_mod1)
meanR2_2 = mean(R2cv_mod2)
meanR2_3 = mean(R2cv_mod3)

means = c(meanR2_1, meanR2_2, meanR2_3)
params = c(m1len, m2len, m3len)

plot(params, means, type='l')

# Exercise 6. (2 points).  First, use the appropriate classical commands to statiscally compare the single line model to
# the full model. Second, using the results of the cross-validation in 5, determined whether you would reach the same conclusion. 
# For example compare the mean R2cv plus or minus two standard errors to assess whether one is significantly
# better.


