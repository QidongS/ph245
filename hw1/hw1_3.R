skulldata = read.table(file="./Data-HW1-Skull.dat")

levels(skulldata$V5) = c("4000BC", "3300BC", "1850BC")
period = as.factor(skulldata$V5)
aggregate(skulldata[,1:4], by = list(period), FUN = mean)
summary(manova(cbind(skulldata$V1,skulldata$V2,skulldata$V3,skulldata$V4) ~ period,data=skulldata))

