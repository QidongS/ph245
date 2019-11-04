alpha = 0.05
#readin dataset
data = read.table(file="./Data-HW1-Transportation.dat")

# seperate two datasets 
gasoline = data[data$V4 == "gasoline",1:3]
diesel = data[data$V4 == "diesel",1:3]

# compute the column means (x-bar) for both gas and diesel
gasoline.bar = apply(gasoline, 2, mean)
diesel.bar = apply(diesel, 2, mean)

# get covariance matrices for both gas and diesel 
s.gasoline = cov(gasoline)
s.diesel = cov(diesel)

# get number of rows 
n.gasoline = nrow(gasoline)
n.diesel = nrow(diesel)

# compute the pooled covariance estimate
s.p = ((n.gasoline - 1) * s.gasoline + (n.diesel - 1) * s.diesel) / (n.gasoline + n.diesel - 2) ; s.p

t2 = t(gasoline.bar - diesel.bar) %*% solve((1/n.gasoline + 1/n.diesel) * s.p) %*% (gasoline.bar - diesel.bar); t2

p = 3 
# compute the p-value
1 - pf(t2*(n.gasoline+n.diesel-p-1)/((n.gasoline+n.diesel-2)*p), p, n.gasoline + n.diesel - p -1)



