##HW2
Qidong Sun 
3033086032

```
#load libray
library(ggplot2)
library(corrgram)
library(gclus)
library(glmnet)

#load data
bodyFat = read.table("Data-HW2-Bodyfat.txt" )

#basic data analysis
head(bodyFat)

#Summary Data
summary(bodyFat)

#Correlation gram 
corrgram(b, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, 
    main = "Body Fat % Data")

bfBrozek = bodyFat[,2]
bfSiri = bodyFat[,3]
age = bodyFat[,5]
weight = bodyFat[,6]
height = bodyFat[,7]
circums = as.matrix(bodyFat[,10:19])
colnames(circums) = c("neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
b = cbind(bfBrozek, bfSiri, age, weight, height, circums) 
summary(b)
remove = c(seq(1, nrow(b))[b[,4]>300],seq(1,nrow(b))[b[,5]<40])
b = b[-remove,]
```
#Q(A)
####Code
```
multi_fit = lm(formula = bfSiri ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data.frame(b))
summary(multi_fit)
plot(multi_fit)
```
####Output
```
Call:
lm(formula = bfSiri ~ age + weight + height + neck + chest + 
    abdomen + hip + thigh + knee + ankle + biceps + forearm + 
    wrist, data = data.frame(b))

Residuals:
     Min       1Q   Median       3Q      Max 
-10.9900  -3.1244  -0.1674   3.0248   9.8648 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.68516   23.37412   0.072 0.942587    
age          0.07189    0.03217   2.234 0.026389 *  
weight      -0.01762    0.06714  -0.263 0.793153    
height      -0.24675    0.19114  -1.291 0.197989    
neck        -0.38682    0.23486  -1.647 0.100887    
chest       -0.11919    0.10825  -1.101 0.272004    
abdomen      0.90452    0.09140   9.897  < 2e-16 ***
hip         -0.15878    0.14586  -1.089 0.277446    
thigh        0.17299    0.14683   1.178 0.239926    
knee        -0.04580    0.24560  -0.186 0.852230    
ankle        0.18502    0.21985   0.842 0.400862    
biceps       0.17968    0.17039   1.054 0.292732    
forearm      0.27605    0.20692   1.334 0.183454    
wrist       -1.80162    0.53304  -3.380 0.000848 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.255 on 236 degrees of freedom
Multiple R-squared:  0.7505,	Adjusted R-squared:  0.7368 
F-statistic: 54.61 on 13 and 236 DF,  p-value: < 2.2e-16

```

#Q(B)
```
age          0.07189    0.03217   2.234 0.026389 * 
```
By reading the output listed above, we find that age is one of the major factor that affects percent body fat using Siri’s equation according to the significant code (one star). For one unit of percent body fat increase, we have age increased by 0.07189. 

In order to test the null hypothesis, we read the p value which is 0.026389. We compare it to 0.05 and find it's lower than that. So we reject the null hypothesis that the ture coefficient is zero. 

#Q(C)
####Code
```
single_fit = lm(formula = bfSiri ~ abdomen, data = data.frame(b))
summary(single_fit)
```
####Output
```
Call:
lm(formula = bfSiri ~ abdomen, data = data.frame(b))

Residuals:
     Min       1Q   Median       3Q      Max 
-10.8987  -3.6453   0.1864   3.1775  12.7887 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -42.73413    2.71651  -15.73   <2e-16 ***
abdomen       0.66928    0.02926   22.88   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.713 on 248 degrees of freedom
Multiple R-squared:  0.6785,	Adjusted R-squared:  0.6772 
F-statistic: 523.3 on 1 and 248 DF,  p-value: < 2.2e-16
```
```
abdomen      0.90452    0.09140   9.897  < 2e-16 ***
```
As the first glance, the Multiple R-squared:  0.6785 which is pretty significant. It tells us the proportion of the variance in the body fat data explained by the linear regreesion modle by just using "abdomen".
By reading the output listed above, we find that abdomen is one of the major factor that affects percent body fat using Siri’s equation according to the significant code (three starts). For one unit of percent body fat increase, we have abdomen increased by 0.90452. 

In order to test the null hypothesis, we read the p value which is 2e-16. We compare it to 0.05 and find it's much lower than that. So we reject the null hypothesis that the ture coefficient is zero. 

#Q(D)
Key assumptions: There must be linear relationship between the dependent and independent variable. Residuals are normally distributed. 
According to the graph, we observe that the red line is relatively smoothy and straight. And points of (value,residual) are around the curve, since we don't want overfitting nor high variance. All of these tells us the linear regression performs well. 


#Q(E)
####Code 
```
#part e
three_fit = lm(formula = bfSiri ~ age + weight + height,data.frame(b) )
summary(three_fit)
```
####Output
```
Call:
lm(formula = bfSiri ~ age + weight + height, data = data.frame(b))

Residuals:
     Min       1Q   Median       3Q      Max 
-11.9223  -3.9437   0.0327   3.9586  12.8856 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 57.27217   10.39897   5.507 9.15e-08 ***
age          0.13732    0.02806   4.895 1.78e-06 ***
weight       0.25366    0.01483  17.110  < 2e-16 ***
height      -1.27416    0.15801  -8.064 3.24e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.382 on 246 degrees of freedom
Multiple R-squared:  0.584,	Adjusted R-squared:  0.579 
F-statistic: 115.1 on 3 and 246 DF,  p-value: < 2.2e-16
```
Comparing the summary we get from the model with 3 predictors age, weight, height to the summary we get from question (a). We find the Multiple R-squared: 0.584 < 0.7505. So the linear model we got from queston a explains more about the data. Because miultiple R-squared tells the proportion of the variance in the data that's explained by the model.


#Q(F)
By looking at the output from the last question and the first question. We can see that the frist question (full model) explains more about the data while the reduced model can't do as well. Since p value is < 2.2e-16 the null hypothesis is rejected. 

Q(G)
```
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(~neck + chest + hip + thigh + knee + ankle + biceps + forearm + wrist, data = data.frame(b), lower.panel=panel.smooth, upper.panel=panel.cor, pch=20, main="10 measurements scatterplot matrix")
```
By observing the plots, we see that hip and thigh, hip and knee, hip and chest are highly correlated. Ankle and forearm, ankle and knee are not very related and have more outliers.


#Q(H)

```
summary(b)
m <- glmnet(as.matrix(b[,3:15]), b[,2], family = "gaussian", alpha = 1)
plot(m, "lambda", label = T)
```