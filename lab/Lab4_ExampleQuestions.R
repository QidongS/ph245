#### Lab 4 Problems#####
### GSI: Weijie Yuan ###
#imade one after another. Assuming that the measures of stiffness 
# arise from four treatments, test for the equality of treatments. 


# set your working directory to the folder the dataset has been downloaded to (optional)


# read in dataset (change the file path name, if you did not set your working directory as above)
stiff = read.table("Data-06-16.dat")
# view dataset
View(stiff)
# remove last column of dataset (not needed)

stiff = stiff[,-5]

# compute the sample size

# compute the mean

# compute the variance

# specify C (based on your null hypothesis)

# compute q.tilda
# q.tilda is used to specify degree of freedom for F distribution on lecture03, page 17

# compute the test statistic
# t(): gives the transpose of the matrix you passed into the parentheses
# solve(): gives the inverse of the matrix you passed into the parentheses
# %*%: a percent sign, a asterisk sign, a percent sign: these three signs together means "multiplying" in matrix multiplication

# compute the p-value

# do you reject or fail to reject the null hypothesis that the treatments are equal?


######################
#### PROBLEM 6.18 ####
######################

# Three measurements of size (length, width, and height) of female and male painted turtles 
# are given in the data "Data-06-18.dat". Test for differences in the mean size between male 
# and female turtles.

# read in dataset 
stiff = read.table("Data-06-18.dat")
# view the dataset
View(stiff)

#### Separate the dataset into males and females ####
# the first argument is subsetting all the rows that the variable "V4" in the dataset "turtles" is "male"
# the second argument is subsetting the 1st to 3rd column
# repeat the same procedure for the female case
males = turtles[turtles$V4 == "male", 1:3]
females = turtles[turtles$V4] == "female", 1:3]
# compute the column means (x-bar) for males and females
# (hint: you can use the apply command)
males.bar = apply(males, 2, mean)
females.bar = apply(females, 2, mean)

# get covariance matrices for both males and females
s.maes = cov(males)
s.females = cov(females)

# since we have the same number of observations in both groups, meaning the same
# number of males and females (n1 = n2), set n to be the number of rows in the males and females datasets
n = nrow(males); n
# compute the pooled covariance estimate
s.p= ((n-1)*s.males + (n-1)*s.females) / (n+n-2); s.p
#### compute the T2 statistic ####
# lecture03, page 24
T2 = t(males.bar - females.bar)%*% solve((1/n+1/n)*s.p)%*%(males.bar-females.bar);12

# set p to be 3 (3 different variables that we are looking at)
p=3 
# compute the p-value

# do you reject or fail to reject the null hypothesis that there is no difference
# between the male and female painted turtles?
