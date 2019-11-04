library(ggplot2)
library(corrgram)
library(gclus)
library(glmnet)

bodyFat = read.table("Data-HW2-Bodyfat.txt" )
#summary(bodyFat)
bfBrozek = bodyFat[,2]
bfSiri = bodyFat[,3]
age = bodyFat[,5]
weight = bodyFat[,6]
height = bodyFat[,7]
circums = as.matrix(bodyFat[,10:19])
colnames(circums) = c("neck", "chest", "abdomen", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
b = cbind(bfBrozek, bfSiri, age, weight, height, circums) 
#summary(b)
remove = c(seq(1, nrow(b))[b[,4]>300],seq(1,nrow(b))[b[,5]<40])
b = b[-remove,]

corrgram(b, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, 
    main = "Body Fat % Data")
#colnames(b) = c(c("bf1", "bf2", "age", "weight", "height"), colnames(circums))
#summary(b)
# head(ids)

multi_fit = lm(formula = bfSiri ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data.frame(b))
summary(multi_fit)

#ggplot(data.frame(b), aes(x=age,y=bfSiri))+geom_point() + geom_smooth(method="lm") 

single_fit = lm(formula = bfSiri ~ abdomen, data = data.frame(b))
summary(single_fit)


plot(multi_fit)

#part e
three_fit = lm(formula = bfSiri ~ age + weight + height,data.frame(b) )
summary(three_fit)


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

summary(b)
m <- glmnet(as.matrix(b[,3:15]), b[,2], family = "gaussian", alpha = 1)
plot(m, "lambda", label = T)