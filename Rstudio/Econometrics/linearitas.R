data=read.csv(file.choose(), sep = ",", header = T)
data
head(data)
#Membentuk vektor y
y <- as.matrix(data[,7])
y

#Membentuk matriks X
##Kolom matriks X dari data
X0 <- as.matrix(data[,c(8)])
X0
##Menentukan banyak unit pengamatan (baris dari vektor y) 
n <- nrow(y)
n

##Membentuk vektor unit konstan
u <- matrix(c(1),n,1)

X <- cbind(u,X0)

#Taksiran kodefisien regresi
b <- solve(t(X)%*%X)%*%t(X)%*%y

#Taksiran standar error
#Matriks Hat (H)
H <- X%*%solve(t(X)%*%X)%*%t(X)

##Membentuk matriks identitas (nxn)
I <- diag(c(1),n,n)

##Menghitung Sum Square Residual
SSE <- t(y)%*%(I-H)%*%y

R20=1-SSE/(n-1)/var(y)
R20

#Q=2
yhat2=(H%*%y)^2
yhat2

#Auxilary Regression
X1 = cbind(X,yhat2)
X1

#Matriks Hat (H)
H1 <- X1%*%solve(t(X1)%*%X1)%*%t(X1)
H1

##Menghitung Sum Square Residual
SSE1 <- t(y)%*%(I-H1)%*%y
SSE1

#Menghitung R square baru dibawah h1
R21=1-SSE1/(n-1)/var(y)
R21

##Menghitung banyak regresor+1
p <- ncol(X)
p

#Menghitung Stat uji F
Fh=((R21-R20)/1)/((1-R21)/(n-p))
Fh

#Menghitung critical value
Fcrit=qf(0.95,1,n-p)
Fcrit

#F hitung > F tabel sehingga menolak H0
#Model belum cukup menspesifikan

#cek pake syntax jadi
model=lm(y~X0)
summary(model)

