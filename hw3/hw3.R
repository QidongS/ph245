
#Set up
library(dummies)

data<-read.table(file="Data-HW3-CHeartDisease.dat", header=FALSE, quote="", sep=",")
n.all<-nrow(data)
id.ms<-sort(c(seq(1,n.all)[data[,12]=='?'], seq(1,n.all)[data[,13]=='?'])) 
data2<-data[-id.ms,]


data2[,12]<-as.numeric(data2[,12]) - 2 
data2[,13]<-as.numeric(data2[,13]) - 1 



X<-data.matrix(data2[,1:13]) 
Y<-data2[,14]
Y[Y > 0]<-1 
#Y <- as.factor(Y) 
colnames(X)<-c("age", "gender", "chestpain", "bldpressure", "chol", "bldsugar", "electrocardio", "heartrate", "angina", "STdepression", "STslope", "vessel", "thal")



#data2[,2] <- as.factor(data2[,2]) #gender
data2[,3] <- as.factor(data2[,3]) #chestpain 
#data2[,6] <- as.factor(data2[,6]) #bldsugar
#data2[,7] <- as.factor(data2[,7]) 
#data2[,9] <- as.factor(data2[,9]) #angina 
#data2[,11] <- as.factor(data2[,11])
#data2[,12] <- as.factor(data2[,12]) #vessel
data2[,13] <- as.factor(data2[,13]) #thal 

names(data2)[1] = "age"
names(data2)[2] = "gender"
names(data2)[3] = "chestpain"
names(data2)[4] = "bldpressure"
names(data2)[5] = "chol"
names(data2)[6] = "bldsugar"
names(data2)[7] = "electrocardio"
names(data2)[8] = "heartrate"
names(data2)[9] = "angina"
names(data2)[10] = "STdepression"
names(data2)[11] = "STslope"
names(data2)[12] = "vessel"
names(data2)[13] = "thal"
names(data2)[14] = "disease"

data2$disease[data2$disease > 0] <- 1
data2[,14] <- as.factor(data2[,14])







#A
#data2;
#summary(X);
#Y
numDisease = nrow(data2[data2$disease!=0, ]);numDisease 
numNoDisease = nrow(data2[data2$disease==0, ]);numNoDisease 
str(data2)
#contrasts(X["chestpain"])

# The V1, V4, V5, V8 and V10 are numerical factors,since they all continuous
# The rest are categorical factors, since they all discrete.


#B
#chd <- CHD
#model <- glm(Y ~ X)
#summary(model)


#mymodel <- glm(disease ~ age + gender + chestpain , family = 'binomial', data=data2  )
# mymodel <- glm(disease ~ age + gender + chestpain + bldpressure + chol + bldsugar + electrocardio + heartrate + angina + STdepression + STslope + vessel + thal, family = 'binomial', data = data2)
# summary(mymodel)
#Gender, type of chestpain, heartrate, angina, vessel and thal are all 
#significant predictors in this model, and the rest are not.



#C



chestpain <- dummy("chestpain",data2)
thal <- dummy("thal",data2)
my_model <- glm(Y ~ X[,1:2] + chestpain[,2:4] + X[,4:12] + thal[,2:3])
summary(my_model)
#d <- data2
#chestpainDummy = dummy("chestpain", d)
#dummy.data.frame(d, names= c("chestpain","thal"))
# thalDummy = dummy("thal", data2)
# my_model <- glm(Y ~ X[,1:2] + chestpainDummy[,2:4] + X[,4:12] + thalDummy[,2:3])
# summary(chd_cmodel)
# chd <- CHD
# V3dummy <- dummy("V3",chd)
# V13dummy <- dummy("V13",chd)
# chd$V3 <- as.factor(chd$V3)
# chd$V13 <- as.factor(chd$V13)
# V3dummy <- dummy("V3",chd)
# V13dummy <- dummy("V13",chd)
# chd_cmodel <- glm(Y ~ X[,1:2] + V3dummy[,2:4] + X[,4:12] + V13dummy[,2:3])
# summary(chd_cmodel)
#Gender, the fourth type of chest pain, heartrate, angina, vessel and thal are 
#significant predictors in this model, and the rest are not.






















#D
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

cmp <- rep(0,nrow(data2))
cmp [fitted(my_model) > 0.5] <- 1
1 - sum(data2$disease == cmp) / length(data2$disease)