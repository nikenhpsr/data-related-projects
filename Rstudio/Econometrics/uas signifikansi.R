###Estimasi OLS###

#Import data
data=read.csv(file.choose(), sep = ";", header = T)
data
head(data)
y=as.matrix(data[,4])
n=nrow(y)
u=matrix(c(1),nrow=n,ncol=1)
x0=as.matrix(data[,c(1,2,3)])
x=cbind(u,x0)

#taksiran koef, var, residual, se
b=solve(t(x)%*%x)%*%t(x)%*%y
k=ncol(x)
e1=y-(x%*%b)
s2=(t(e1)%*%e1)/(n-k)
se=(diag(solve(t(x)%*%x))%*%s2)^0.5

###MULTIKOLINEARITAS###
#mencari VIF
R=cor(x0)
VIF=diag(solve(R))
#semua x VIF kurang dari 5 maka , dari semua 
#tidak perlu ada yang perlu dikhawatirkan ttg multikolinearitas

###OUTLIER###

#OUTLIER DI Y
#menghitung head matrix
I=diag(c(1),n,n)
H=x%*%solve(t(x)%*%x)%*%t(x)
sum(diag(H))
#menghitung residual berdasarkan head matrix
e=(I-H)%*%y
e1=y-(x%*%b)
#untuk cek apakah e sma kek e1 e==e1

boxplot(e)
boxplot(e1)

#semi studentized residual
ess=e1/as.vector(s2^0.5)
boxplot(ess)

#studentized residual
#ri= ei/(s*akar 1-hii)

r=e/matrix(c(sqrt(s2)),n,1)/sqrt(diag(I-H))
boxplot(r)

#deleted residual
d=e/diag(I-H)

#studentized deleted residual
#sse/n-p-i=s2
SSE=t(y)%*%(I-H)%*%y
s2=SSE/(n-k)
s=sqrt(s2)
MSE=(matrix(c(SSE),n,1)-e*e/diag(I-H))/(n-k-1)
vd=MSE/diag(I-H)
t=d/sqrt(vd)
alpha=0.05
t_cri=qt(1-alpha/n/2,n-k-1)
abs(t)>=t_cri

#OUTLIER DI X

## leverage
h <- diag(H)
h_cri <-2*k/n
h>=h_cri

## DFFITS
DFF <- t*h/diag(I-H)
DFFcri<- 2*sqrt(p/n)
abs(DFF)>1

## Cook's Distance
D <- (e^2/p/s2[1,1])*(h/diag(I-H)^2)
Dcri <- qf(0.5,p,n-p)
D>=Dcri

## DFBETAS
X=x
DFB <- matrix(c(0),n,p) #individual koefisien regresi
Da <- matrix(c(0),n,1) #semua koefisien regresi

for (i in 1:n)
{
  yi <- y[-i]
  Xi <- X[-i,]
  
  bi <- solve(t(Xi)%*%Xi)%*%t(Xi)%*%yi
  
  #Matriks Hat (H)
  Hi <- Xi%*%solve(t(Xi)%*%Xi)%*%t(Xi)
  
  ##Membentuk matriks identitas (nxn)
  Ii <- diag(c(1),n-1,n-1)
  
  ##Menghitung Sum Square Residual
  SSEi <- t(yi)%*%(Ii-Hi)%*%yi
  
  ##Menghitung taksiran varians kekeliruan (MSE)
  s2i <- SSEi/(n-p-1)
  si <- sqrt(s2i)
  
  DFBi <- (b-bi)/si[1,1]/sqrt(diag(solve(t(X)%*%X)))
  DFB[i,] <- DFBi
  
  Dai <- t(b-bi)%*%t(X)%*%X%*%(b-bi)/p/s2[1,1]
  Da[i,] <- Dai
}

DFB
DFBcri <- 2/sqrt(n)
DFB>DFBcri
DFB>1
#kota semarang sangat berpengaruh pada koefisien regresi (beta 2) (kalau sample kecil)
#kab cilacap(b0), kebumen purworejo (b3) kota semarang (b2) (sample besar)
Da
Dacri <- Dcri
Da>=Dacri
#pengamatan ke-i tidak berpengaruh terhadap semua koefisien regresi.

###UJI SIMULTAN###
#Menghitung MSE dan MSR
J=matrix(c(1),ncol = n, nrow = n)
SSR=t(y)%*%(H-(J/n))%*%y
MSR=SSR/(p-1)
SSE=t(y)%*%(I-H)%*%y
MSE=SSE/(n-p)
Fhit=MSR/MSE
ftab=qf(0.95, p-1, n-p )
test_significant=
  if (Fhit >= ftab) {
    print("H0 ditolak")
  } else { print("H0 diterima")
  }

###TESTING ASSUMPTION:HETEROSKEDASTIC###

