###########################
####  Lab 5 Problems  #####
#### Linear Regression ####
#### GSI: Weijie Yuan  ####
###########################

# clear workspace

# set working directory (optional)

# load data (download PH245Lab5_data.csv from bCourses)

# view the first few rows and columns of the dataset

# the variable Crop.History is a categorical variable (denotes the type of crop), so let's
# check how the variable is coded in the dataset

# since the Crop.History variable is not coded as a factor, let's create a new variable
# that stores this data recoded as a factor using the as.factor command

# let's change the labels of the levels for each group in the fCropHistory variable so they
# are more descriptive

# you can check to see how the labels of the levels have now changed

# fit a linear model to the data
# we are using the variable names to refer to the dependent/independent variables in the code

#	Fit	a	linear	model	to	the	data


# we can reorder the levels of the fCropHistory variable to make snapbeans the first level 
# (baseline/reference group) rather than soybeans (as in the model above)
# note: when using the levels command, you need to use the same level names as previously specified, 
# just in a different order

# fit a new linear model (with the changed levels for fCropHistory)


# can also code model2 using dummy variables instead of factor variables:
# note: it seems that R can handle dummy variables without changing their types to factors
# first, create a new variable that contains 0s for the length of the sample size of the dataset

# recode this variable to fill in 1s for the observations that are coded as soybeans in the 
# fCropHistory variable

# similarly code a new dummy variable for oats

# fit a new linear model with the constructed dummy variables instead of fCropHistory


# let's try to compute the F statistic that appears in the lm command (in this case, the
# restricted model is the model that only includes the intercept term)

# for any linear regression model, the degrees of freedom using in the calculation of the 
# F statistic are calculated as n - p - 1, where n = number of observations and p = number of covariates
# restricted model: n = 25, p = 0 (since no covariates), df = 25 - 0 - 1 = 24
# full model (model 3): n = 25, p = 5, df = 25 - 5 - 1 = 19

# to get the degrees of freedom in R, we can calculate the values as above, or extract the 
# information directly from the summary of each model that we ran

# can calculate the F statistic (see lecture 4, slide 27 for the formula)
# the deviance functions gives SSE
# SSE: the sum of squared residuals

# get the p-value associated with this F statistic, using the pf function
# the test statistic has an F distribution with (df_R- df_F, df_F) degrees of freedom
