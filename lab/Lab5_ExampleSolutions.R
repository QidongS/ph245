#### Lab 5 Problems ####
### GSI: Weijie Yuan ###
########################

# clear workspace
rm(list=ls())

# set working directory (optional)

# load data (download PH245Lab5_data.csv from bCourses)
farm = read.csv("PH245Lab5_data.csv")
print("---------------------------------------")
# view the first few rows and columns of the dataset
head(farm)
print("---------------------------------------")
# the variable Crop.History is a categorical variable (denotes the type of crop), so let's
# check how the variable is coded in the dataset
print("-09999---")
class(farm$Crop.History)
print("-09999---")
# since the Crop.History variable is not coded as a factor, let's create a new variable
# that stores this data recoded as a factor using the as.factor command
farm$fCropHistory = as.factor(farm$Crop.History)
class(farm$fCropHistory)

# let's change the labels of the levels for each group in the fCropHistory variable 
# so they are more descriptive
levels(farm$fCropHistory)
print("---------------------------------")
levels(farm$fCropHistory) = c("Soybeans", "Oats", "Snapbeans")
# you can check to see how the labels of the levels have now changed
levels(farm$fCropHistory) 

# fit a linear model to the data
# we are using the variable names to refer to the dependent/independent variables 
# in the code
model1 = lm(Score ~ Rainfall + Wind + Temperature + fCropHistory, data = farm)
summary(model1) 
print("-------------------------------------------------")
# we can reorder the levels of the fCropHistory variable to make 
# snapbeans the first level 
# (baseline/reference group) rather than soybeans (as in the model above)
# note: when using the levels command, you need to use the same level 
# names as previously specified, 
# just in a different order
farm$fCropHistory = factor(farm$fCropHistory,
                           levels = c("Snapbeans", "Oats", "Soybeans"))
levels(farm$fCropHistory)

# fit a new linear model (with the changed levels for fCropHistory)
model2 = lm(Score ~ Rainfall + Wind + Temperature + fCropHistory, data = farm)
summary(model2)

# can also code model2 using dummy variables instead of factor variables:
# note: it seems that R can handle dummy variables without changing their 
# types to factors
# first, create a new variable that contains 0s for the length of the sample 
# size of the dataset
farm$soybean_dummy = rep(0,nrow(farm))

# recode this variable to fill in 1s for the observations that are coded as 
# soybeans in the 
# fCropHistory variable
farm$soybean_dummy[farm$fCropHistory == "Soybeans"] = 1

# similarly code a new dummy variable for oats
farm$oats_dummy = rep(0,nrow(farm))
farm$oats_dummy[farm$fCropHistory == "Oats"] = 1

# fit a new linear model with the constructed dummy variables instead of fCropHistory
model3 = lm(Score ~ Rainfall + Wind + Temperature + oats_dummy + soybean_dummy , data = farm)
summary(model3)


# let's try to compute the F statistic that appears in the lm command (in this case, the
# restricted model is the model that only includes the intercept term)
model0 = lm(Score~1, data = farm)
summary(model0)

# for any linear regression model, the degrees of freedom using in the calculation of the 
# F statistic are calculated as n - p - 1, where n = number of observations and p = number 
# of covariates
# restricted model: n = 25, p = 0 (since no covariates), df = 25 - 0 - 1 = 2
# full model (model 3): n = 25, p = 5, df = 25 - 5 - 1 = 19

# to get the degrees of freedom in R, we can calculate the values as above, or extract the 
# information directly from the summary of each model that we ran
df_F = summary(model3)$df[2]
df_R = summary(model0)$df[2]

# can calculate the F statistic (see lecture 4, slide 27 for the formula)
# the deviance functions gives SSE
# SSE: the sum of squared residuals
Fstat=((deviance(model0)- deviance(model3))/ (df_R- df_F)) /  (deviance(model3)/ df_F)
Fstat

# get the p-value associated with this F statistic, using the pf function
# the test statistic has an F distribution with (df_R- df_F, df_F) degrees of freedom
pf(Fstat, df_R- df_F, df_F, lower.tail=F)
1-pf(Fstat, df_R- df_F, df_F)
