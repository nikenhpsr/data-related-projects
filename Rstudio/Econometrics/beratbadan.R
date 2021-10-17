#anreg bb

setwd("E:/Kuliah/Semester 6/Ekonometrika/prak/")

data <- read.csv("berat_badan.csv")
n=nrow(data)
u=matrix(c(1),nrow=50, ncol=1)
X0=as.matrix(data[,1:6])
X= cbind(u,X0)
Y=as.matrix(data[,7])

#b = (X'X)-1X'Y

b=solve(t(X)%*%X)%*%t(X)%*%Y

k=ncol(X)
e= Y-X%*%b
s2= t(e)%*%e/(n-k)

se= sqrt(s2%*%diag(solve(t(X)%*%X)))