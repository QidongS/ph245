# PH245 HW3
> Qidong Sun 
> 3033086032

## Problems

```
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
colnames(X)<-c("age", "gender", "chestpain", "bldpressure", "chol", "bldsugar", "electrocardio", "heartrate", "angina", "STdepression", "STslope", "vessel", "thal")


#data2[,2] <- as.factor(data2[,2]) #gender
data2[,3] <- as.factor(data2[,3]) #chestpain 
#data2[,6] <- as.factor(data2[,6]) #bldsugar
#data2[,7] <- as.factor(data2[,7]) 
data2[,9] <- as.factor(data2[,9]) #angina 
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
```

#### (a)
##### code
```
numDisease = NROW(Y[Y==1]); numDisease
numNoDisease = NROW(Y[Y==0]); numNoDisease
str(data2)
```

##### ouput
```
[1] 137
[1] 160


'data.frame':	297 obs. of  14 variables:
 $ age          : num  63 67 67 37 41 56 62 57 63 53 ...
 $ gender       : Factor w/ 2 levels "0","1": 2 2 2 2 1 2 1 1 2 2 ...
 $ chestpain    : Factor w/ 4 levels "1","2","3","4": 1 4 4 3 2 2 4 4 4 4 ...
 $ bldpressure  : num  145 160 120 130 130 120 140 120 130 140 ...
 $ chol         : num  233 286 229 250 204 236 268 354 254 203 ...
 $ bldsugar     : Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 2 ...
 $ electrocardio: Factor w/ 3 levels "0","1","2": 3 3 3 1 3 1 3 1 3 3 ...
 $ heartrate    : num  150 108 129 187 172 178 160 163 147 155 ...
 $ angina       : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 2 1 2 ...
 $ STdepression : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
 $ STslope      : Factor w/ 3 levels "1","2","3": 3 2 2 3 1 1 3 1 2 3 ...
 $ vessel       : Factor w/ 4 levels "0","1","2","3": 1 4 3 1 1 1 3 1 2 1 ...
 $ thal         : Factor w/ 3 levels "1","2","3": 2 1 3 1 1 1 1 1 3 3 ...
 $ disease      : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 2 1 2 2 ...
```




According to the output above, there are 137 people with heart disease, and 160 none disease people. 
Accordint to the table, We can see that age, bldpressure, chol, heartrate, STdepression are numerical. Electrocardio and Stslope are unclear. All others are categrical. 

#### (b)
Here are two logistic fittings. For the first one, I treat gender chestpain, bldsugar, electrocardio, angina, STslope, Vessel, thal as categorical. But for the second one, I didn't because I feel the design of the question wants me directly use the logistic fitting and further recognize the types of variable in later question.  



##### code
```
mymodel <- glm(disease ~ age + gender + chestpain + bldpressure + chol + bldsugar + 
electrocardio + heartrate + data2$angina + 
STdepression + data2$STslope + data2$vessel + thal, family = 'binomial', data = data2)
summary(mymodel)
```

##### output
```
Call:
glm(formula = disease ~ age + gender + chestpain + bldpressure + 
    chol + bldsugar + electrocardio + heartrate + data2$angina + 
    STdepression + data2$STslope + data2$vessel + thal, family = "binomial", 
    data = data2)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.0490  -0.4847  -0.1213   0.3039   2.9086  

Coefficients:
                Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -6.253978   2.960399  -2.113 0.034640 *  
age            -0.023508   0.025122  -0.936 0.349402    
gender1         1.670152   0.552486   3.023 0.002503 ** 
chestpain2      1.448396   0.809136   1.790 0.073446 .  
chestpain3      0.393353   0.700338   0.562 0.574347    
chestpain4      2.373287   0.709094   3.347 0.000817 ***
bldpressure     0.027720   0.011748   2.359 0.018300 *  
chol            0.004445   0.004091   1.087 0.277253    
bldsugar1      -0.574079   0.592539  -0.969 0.332622    
electrocardio1  1.000887   2.638393   0.379 0.704424    
electrocardio2  0.486408   0.396327   1.227 0.219713    
heartrate      -0.019695   0.011717  -1.681 0.092781 .  
data2$angina1   0.653306   0.447445   1.460 0.144267    
STdepression    0.390679   0.239173   1.633 0.102373    
data2$STslope2  1.302289   0.486197   2.679 0.007395 ** 
data2$STslope3  0.606760   0.939324   0.646 0.518309    
data2$vessel1   2.237444   0.514770   4.346 1.38e-05 ***
data2$vessel2   3.271852   0.785123   4.167 3.08e-05 ***
data2$vessel3   2.188715   0.928644   2.357 0.018428 *  
thal2          -0.168439   0.810310  -0.208 0.835331    
thal3           1.433319   0.440567   3.253 0.001141 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 409.95  on 296  degrees of freedom
Residual deviance: 183.10  on 276  degrees of freedom
AIC: 225.1

Number of Fisher Scoring iterations: 6
```
By reading the summary of logistic regression, we find that gender1, chestpain, bldpressure, STslope, vessel, and thal are statistically significant. 



#### code
model <- glm(Y ~ X)
summary(model)

##### output

```

glm(formula = Y ~ X)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-0.95099  -0.21719  -0.04465   0.19177   0.88874  

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -0.3206635  0.3206120  -1.000 0.318087    
Xage           -0.0012505  0.0027322  -0.458 0.647534    
Xgender         0.1483876  0.0490138   3.027 0.002693 ** 
Xchestpain      0.0826754  0.0241094   3.429 0.000696 ***
Xbldpressure    0.0021831  0.0012512   1.745 0.082106 .  
Xchol           0.0003295  0.0004153   0.794 0.428151    
Xbldsugar      -0.0955640  0.0596062  -1.603 0.109993    
Xelectrocardio  0.0329834  0.0212532   1.552 0.121797    
Xheartrate     -0.0026911  0.0011311  -2.379 0.018014 *  
Xangina         0.1334793  0.0505880   2.639 0.008787 ** 
XSTdepression   0.0315276  0.0230370   1.369 0.172221    
XSTslope        0.0646453  0.0423642   1.526 0.128142    
Xvessel         0.1437419  0.0250276   5.743 2.39e-08 ***
Xthal           0.1228227  0.0257060   4.778 2.85e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.1212131)

    Null deviance: 73.805  on 296  degrees of freedom
Residual deviance: 34.303  on 283  degrees of freedom
AIC: 231.78

Number of Fisher Scoring iterations: 2

```
By reading the summary of logistic regression, we find that gender, chestpain, heartrate, angina, vessel, and thal are statistically significant. 

#### (c)


##### code 
```
chestpain <- dummy("chestpain",data2)
thal <- dummy("thal",data2)
my_model <- glm(Y ~ X[,1:2] + chestpain[,2:4] + X[,4:12] + thal[,2:3])
summary(my_model)
```


##### output
```
Call:
glm(formula = Y ~ X[, 1:2] + chestpain[, 2:4] + X[, 4:12] + thal[, 
    2:3])

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-0.95427  -0.23225  -0.02444   0.16865   0.96950  

Coefficients:
                             Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -0.0813903  0.3197356  -0.255  0.79925    
X[, 1:2]age                -0.0009670  0.0027153  -0.356  0.72202    
X[, 1:2]gender              0.1444848  0.0491464   2.940  0.00356 ** 
chestpain[, 2:4]chestpain2  0.0869007  0.0913087   0.952  0.34206    
chestpain[, 2:4]chestpain3  0.0358508  0.0838977   0.427  0.66948    
chestpain[, 2:4]chestpain4  0.2575722  0.0833773   3.089  0.00221 ** 
X[, 4:12]bldpressure        0.0020394  0.0012477   1.635  0.10326    
X[, 4:12]chol               0.0003137  0.0004133   0.759  0.44843    
X[, 4:12]bldsugar          -0.0700977  0.0598440  -1.171  0.24246    
X[, 4:12]electrocardio      0.0290499  0.0211770   1.372  0.17123    
X[, 4:12]heartrate         -0.0025455  0.0011269  -2.259  0.02467 *  
X[, 4:12]angina             0.1068200  0.0510787   2.091  0.03740 *  
X[, 4:12]STdepression       0.0324768  0.0230902   1.407  0.16068    
X[, 4:12]STslope            0.0674272  0.0424120   1.590  0.11300    
X[, 4:12]vessel             0.1372903  0.0249407   5.505 8.36e-08 ***
thal[, 2:3]thal2            0.0872373  0.0939143   0.929  0.35374    
thal[, 2:3]thal3            0.2337495  0.0513588   4.551 7.96e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.1190062)

    Null deviance: 73.805  on 296  degrees of freedom
Residual deviance: 33.322  on 280  degrees of freedom
AIC: 229.16

Number of Fisher Scoring iterations: 2

```

By reading the summary of logistic regression, we find that gender, chestpain type 4, heartrate, angina, vessel, and thal are statistically significant. 


#### (d)

```
X[, 4:12]chol               0.0003137  0.0004133   0.759  0.44843    
```
Reading the coefficient estimate of redictor [5] serum cholestoral which is 0.0003137. With others unchanged, it tells us, for every one unit of serum cholesterol increase, we are 0.0003137 more log odd to have heart disease. The p value = 0.44843 > 0.05. So we do not reject null hypothesis that it has no effect (coefficient equals 0). 

#### (e)

```
chestpain[, 2:4]chestpain4  0.2575722  0.0833773   3.089  0.00221 ** 

```
Still reading from the ouput above, we find the  coefficient estimate of chest pain 4 is 0.2575722. According to the significant code, we can tell it's pretty statistically significant. For every one unit of increase, we are 0.2575722 more chance to have heart disease compared to type 1. The p value = 0.00221<0.05. So we reject the null hypothesis that it's coefficient is 0. 

#### (f)
```
cmp <- rep(0,nrow(data2))
cmp [fitted(my_model) > 0.5] <- 1
1 - sum(data2$disease == cmp) / length(data2$disease)
```
By doing the calculation, we find the misclassification rate is 0.1313131. 