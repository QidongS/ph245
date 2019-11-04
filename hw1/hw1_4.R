sensingdata = read.table(file="./Data-HW1-Sensing.dat")

levels(sensingdata$V4) = c("Julian 150", "Julian 235", "Julian 320")
species = as.factor(sensingdata$V3)
time = as.factor(sensingdata$V4)
summary(manova(cbind(sensingdata$V1,sensingdata$V2) ~ species*time,data=sensingdata),test="Wilks")
summary.aov(manova(cbind(sensingdata$V1,sensingdata$V2) ~ species*time,data=sensingdata))

