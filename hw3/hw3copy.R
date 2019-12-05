#############################################################
library(dummies)
data<-read.table(file="Data-HW3-CHeartDisease.dat", header=FALSE, quote="", sep=",")
n.all<-nrow(data)
id.ms<-sort(c(seq(1,n.all)[data[,12]=='?'], seq(1,n.all)[data[,13]=='?'])) 
data2<-data[-id.ms,]
data2[,12]<-as.numeric(data2[,12]) - 2 #Why minus 2?
data2[,13]<-as.numeric(data2[,13]) - 1 #Why minus 1?
X<-data.matrix(data2[,1:13]) 
Y<-data2[,14]; 
Y[Y > 0]<-1 
colnames(X)<-c("age", "gender", "chestpain", "bldpressure", "chol", "bldsugar", "electrocardio", "heartrate", "angina", "STdepression", "STslope", "vessel", "thal")
CHD <- data2


###################################QA########################################
Have_CHD <- nrow(CHD[CHD$V14!=0, ])
None_CHD <- nrow(CHD[CHD$V14==0, ])
str(CHD)
# The V1, V4, V5, V8 and V10 are numerical factors,since they all continuous
# The rest are categorical factors, since they all discrete.


###################################QB########################################






chd <- CHD
#chd[chd$V14 != 0, 14] <- 1
#chd$V2 <- as.factor(chd$V2)
#chd$V3 <- as.factor(chd$V3)
#chd$V6 <- as.factor(chd$V6)
# chd$V7 <- as.fatcor(chd$V7) V7 = resting electrocardiographic results, unclear whether it is categorical
#chd$V9 <- as.factor(chd$V9)
#chd$V11 <- as.factor(chd$V11) V11 = The slope of the peak exercise ST segment, unclear whether it is categorical
#chd$V12 <- as.factor(chd$V12)
#chd$V13 <- as.factor(chd$V13)
#chd_model <-  glm(V14 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13, family = "binomial", data = chd)
chd_model <- glm(Y ~ X)
summary(chd_model)
#Gender, type of chestpain, heartrate, angina, vessel and thal are all 
#significant predictors in this model, and the rest are not.


###################################QC########################################
d <- data2
d$V3 <- as.factor(d$V3)
d$V13 <- as.factor(d$V13)

V3dummy <- dummy("V3",d)
V13dummy <- dummy("V13",d)
chd_cmodel <- glm(Y ~ X[,1:2] + V3dummy[,2:4] + X[,4:12] + V13dummy[,2:3])
summary(chd_cmodel)
#Gender, the fourth type of chest pain, heartrate, angina, vessel and thal are 
#significant predictors in this model, and the rest are not.


###################################QD########################################
#The coefficient estimate means the odds of presence/absence heart
#disease with every one unit increase in serum cholesterol are 
#e0.0003137 = 1.00031374921 times higher, keeping everthing else fixed.
#The P-value is 0.44843, which is higher than the significant level (0.05)
#So we do not reject the null hypothesis, this coefficient is equals zero.


###################################QE########################################
#0.0358508 is the estimation for chest pain type 4: 
#having chest pain type 4 versus type 1 (baseline)
#changes the odds of presence/absence heart disease by
#e0.0358508 = 1.036501, holding the other predictors fixed.
#The P-value is 0.66948, which is higher than the significant level (0.05)
#So we do not reject the null hypothesis, this coefficient is equals zero.


###################################QF########################################
n <- nrow(chd)
yb.hat <- rep(0,n)
yb.hat[fitted(chd_cmodel) > 0.5] <- 1
1 - sum(chd$V14 == yb.hat) / length(chd$V14)