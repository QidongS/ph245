alpha  = 0.05
data = read.table(file="./Data-HW1-Cognition.dat")

#get number of rows 
n = nrow(data); n

#calculate the x-bar mean
X.bar = apply(data,2,mean);
#compute the variance 
S = cov(data) ; 

# get C 
C = cbind(c(-1,0,0),c(1,-1,0),c(0,1,-1),c(0,0,1)); 

#compute q.tilda which can be used to specify degree of freedom for F distribution 
q.tilda = nrow(C)

# compute statistic
CX= C%*%X.bar;CX

CS = C %*% S %*% t(C); CS
T2 = n*t(CX) %*% solve(CS) %*% CX ; 
#compute p-value
T2
f.cutoff = ((n - 1)*q.tilda)/(n - q.tilda) * qf(1 - alpha, q.tilda, n - q.tilda); 
f.cutoff
1 - pf((n-q.tilda)*(T2)/((n-1)*q.tilda), q.tilda, n - q.tilda)