#Membaca data
data=read.csv(file.choose(), sep = ";", header = T)


#Melihat bagian atas data
head(data)

#Membentuk vektor y
y	<- as.matrix(data[,4])

#Membentuk matriks X
##Kolom matriks X dari data
X0	<- as.matrix(data[,1:3])
##Menentukan banyak unit pengamata (baris dari vektor y) 
n	<- nrow(y)
##Membentuk vektor unit konstan
u	<- matrix(c(1),n,1)

X	<- cbind(u,X0)


#Taksiran kodefisien regresi
b	<- solve(t(X)%*%X)%*%t(X)%*%y

#Taksiran standar error
#Matriks Hat (H)
H	<- X%*%solve(t(X)%*%X)%*%t(X)

##Membentuk matriks identitas (nxn)
I	<- diag(c(1),n,n)

##Menghitung Sum Square Residual
SSE	<- t(y)%*%(I-H)%*%y

##Menghitung banyak regresor+1
p	<- ncol(X)

##Menghitung taksiran varians kekeliruan (MSE)
s2	<- SSE/(n-p)
s	<- sqrt(s2)

#Menghitung MSE dan MSR untuk test simultan
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

#test individu
#untuk beta0
c=solve(t(X)%*%X)
t0=b[1,]/(s*sqrt(c[1,1]))
t1=b[2,]/(s*sqrt(c[2,2]))
t2=b[3,]/(s*sqrt(c[3,3]))
t3=b[4,]/(s*sqrt(c[4,4]))


## REsidual
e	<- (I-H)%*%y

## Semistudentized residual
es	<- e/s[1,1]

## studentized residual
r	<- e/matrix(c(s),n,1)/sqrt(diag(I-H)) 	#s dijadikan matrik (nX1)
r1	<- e/s[1,1]/sqrt(diag(I-H))			#s dijadikan skalar		


## deleted residual
d	<- e/diag(I-H)

## studentized deleted residual
MSEi	<- (matrix(c(SSE),n,1)-e*e/diag(I-H))/(n-p-1)
vd	<- MSEi/diag(I-H)
t	<- d/sqrt(vd)
alpha	<- 0.05
t_cri	<- qt(1-alpha/n/2,n-p-1)

## leverage
h	<- diag(H)
h_cri	<-2*p/n

## DFFITS
DFF	<- t*h/diag(I-H)
DFFcri<- 2*sqrt(p/n)

## Cook's Distance
D	<- (e^2/p/s2[1,1])*(h/diag(I-H)^2)
Dcri	<- qf(0.5,p,n-p)

## DFBETAS

DFB	<- matrix(c(0),n,p)	#individual koefisien regresi
Da	<- matrix(c(0),n,1)	#semua koefisien regresi

for (i in 1:n)
{
yi	<- y[-i]
Xi	<- X[-i,]

bi	<- solve(t(Xi)%*%Xi)%*%t(Xi)%*%yi

#Matriks Hat (H)
Hi	<- Xi%*%solve(t(Xi)%*%Xi)%*%t(Xi)

##Membentuk matriks identitas (nxn)
Ii	<- diag(c(1),n-1,n-1)

##Menghitung Sum Square Residual
SSEi	<- t(yi)%*%(Ii-Hi)%*%yi

##Menghitung taksiran varians kekeliruan (MSE)
s2i	<- SSEi/(n-p-1)
si	<- sqrt(s2i)

DFBi	<- (b-bi)/si[1,1]/sqrt(diag(solve(t(X)%*%X)))
DFB[i,]	<- DFBi

Dai	<- t(b-bi)%*%t(X)%*%X%*%(b-bi)/p/s2[1,1]
Da[i,]	<- Dai
}

DFB
DFBcri	<- 2/sqrt(n)

Da
Dacri		<- Dcri










