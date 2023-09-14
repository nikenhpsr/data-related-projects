#Load and defining the data
data=read.csv(file.choose(), sep = ";", header = T)
attach(data)
head(data)

#Defining variables
y=as.matrix(data[,7])
n=nrow(y)
u=matrix(c(1),nrow=n,ncol=1)
x0=as.matrix(data[,c(1,2,3,4,5,6)])
x=cbind(u,x0)

#OLS Estimation of beta using matrix
b=solve(t(x)%*%x)%*%t(x)%*%y

#Estimation using lmtest packages
model <- lm(y~x1+x2+x3+x4+x5+x6, data = data)
summary(model)
