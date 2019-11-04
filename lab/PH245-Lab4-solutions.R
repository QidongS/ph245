# #### Lab 4 Problems####
# ### GSI: Yang Li ###
# ### Adapted from Lab 4 by Courtney Page (PH 245 Fall 2015) ###


# ######################
# #### PROBLEM 6.16 ####  check lecture03 14-20
# ######################

# # Four measures of the response stiffness on each of 30 boards are given in the
# # data "Data-06-16.dat". The measures, on a given board, are repeated in the sense
# # that they were made one after another. Assuming that the measures of stiffness 
# # arise from four treatments, test for the equality of treatments. 
# # the first four columns are the measures of four treatments
# # the last column is not needed


# # set your working directory to the folder the dataset has been downloaded to (optional)

# # read in dataset 
# # change the file path name, if you did not set your working dire ctory as above
# stiff = read.table("Data-06-16.dat")

# # view dataset
# View(stiff)

# # remove last column of dataset (not needed)
# stiff = stiff[,-5]

# # compute the sample size
# n = nrow(stiff) ; n

# # compute the mean
# X.bar = apply(stiff, 2, mean)

# # compute the variance
# S = cov(stiff)

# # specify C (based on your null hypothesis)
# C = cbind(c(-1,0,0), c(1,-1,0), c(0,1,-1), c(0,0,1)) ; C
# C = matrix(,nrow=,ncol=)
# # compute q.tilda
# # q.tilda is used to specify degree of freedom for F distribution on lecture03, page 17
# q.tilda = nrow(C)

# # compute the test statistic
# # t(): gives the transpose of the matrix you passed into the parentheses
# # solve(): gives the inverse of the matrix you passed into the parentheses
# # %*%: a percent sign, a asterisk sign, a percent sign: 
# # these three signs together means "multiplying" in matrix multiplication
# CX = C %*% X.bar; CX
# CS = C %*% S %*% t(C); CS
# T2 = n * t(CX) %*% solve(CS) %*% CX ; T2

# # compute the p-value
# T2
# alpha <- 0.05
# f.cutoff = ((n - 1)*q.tilda)/(n - q.tilda) * qf(1 - alpha, q.tilda, n - q.tilda)
# f.cutoff
# 1 - pf((n-q.tilda)*(T2)/((n-1)*q.tilda), q.tilda, n - q.tilda)

# # do you reject or fail to reject the null hypothesis that the treatments are equal?


######################
#### PROBLEM 6.18 ####   check lecture 03 21-25
######################

# Three measurements of size (length, width, and height) of female and male painted turtles 
# are given in the data "Data-06-18.dat". Test for differences in the mean size between male 
# and female turtles.

# read in dataset 
turtles = read.table("Data-06-18.dat")

# view the dataset
View(turtles)

#### Separate the dataset into males and females ####
# the first argument is subsetting all the rows that the variable "V4" in the dataset "turtles" is "male"
# the second argument is subsetting the 1st to 3rd column
# repeat the same procedure for the female case
males = turtles[turtles$V4 == "male", 1:3]
females = turtles[turtles$V4 == "female", 1:3]

# compute the column means (x-bar) for males and females
# (hint: you can use the apply command)
males.bar = apply(males,2,mean)
females.bar = apply(females,2,mean)

# get covariance matrices for both males and females
s.males = cov(males)
s.females = cov(females)

# since we have the same number of observations in both groups, meaning the same
# number of males and females (n1 = n2), 
# set n to be the number of rows in the males and females datasets
n = nrow(males) ; n

# compute the pooled covariance estimate
s.p = ((n - 1) * s.males + (n - 1) * s.females) / (n + n - 2) ; s.p

#### compute the T2 statistic ####
# lecture03, page 24
T2 = t(males.bar - females.bar) %*% solve((1/n + 1/n) * s.p) %*% (males.bar - females.bar); T2

# set p to be 3 (3 different variables that we are looking at)
p = 3

# compute the p-value
1 - pf(T2*(n+n-p-1)/((n+n-2)*p), p, n + n - p -1)

# do you reject or fail to reject the null hypothesis that there is no difference
# between the male and female painted turtles?
