# vector & matrix data
n	<- 30
p	<- 3

library(orcutt)
data(icecream)
y	<- as.matrix(icecream[,2],nrow=n,ncol=1)
unit	<- matrix(1,n,1)
X	<- as.matrix(cbind(unit,icecream[,c(1,3,4)]),nrow=n,ncol=p+1)

# taksiran beta
be_h	<- solve(t(X)%*%X)%*%t(X)%*%y

# residual
I	<- diag(1,n,n)				#matriks identitas
H	<- X%*%solve(t(X)%*%X)%*%t(X)		#Hat matrix
e	<- (I-H)%*%y

# uji autokorelasi (asymtotic test)
ya	<- e[-1,]	
Xa	<- e[-n,]

rho	<- as.vector(solve(t(Xa)%*%Xa)%*%t(Xa)%*%ya)
rho

za	<- sqrt(n-1)*rho
za
pv_a	<- 1-pnorm(za)
pv_a


#Iterasi 1
y1	<- y[-1,]-rho*y[-n,]
X1	<- X[-1,]-(X[-n,]*rho)

# taksiran beta
be_1	<- solve(t(X1)%*%X1)%*%t(X1)%*%y1

# residual
e1	<- y-X%*%be_1

# autokorelasi 
ya1	<- e1[-1,]	
Xa1	<- e1[-n,]

rho1	<- as.vector(solve(t(Xa1)%*%Xa1)%*%t(Xa1)%*%ya1)
rho1
d1	<- abs(rho-rho1)
d1

#Iterasi 2
y2	<- y[-1,]-rho1*y[-n,]
X2	<- X[-1,]-(X[-n,]*rho1)

# taksiran beta
be_2	<- solve(t(X2)%*%X2)%*%t(X2)%*%y2

# residual
e2	<- y-X%*%be_2

# autokorelasi 
ya2	<- e2[-1,]	
Xa2	<- e2[-n,]

rho2	<- as.vector(solve(t(Xa2)%*%Xa2)%*%t(Xa2)%*%ya2)
rho2
d2	<- abs(rho1-rho2)
d2

#Iterasi 3
y3	<- y[-1,]-rho2*y[-n,]
X3	<- X[-1,]-(X[-n,]*rho2)

# taksiran beta
be_3	<- solve(t(X3)%*%X3)%*%t(X3)%*%y3

# residual
e3	<- y-X%*%be_3

# autokorelasi 
ya3	<- e3[-1,]	
Xa3	<- e3[-n,]

rho3	<- as.vector(solve(t(Xa3)%*%Xa3)%*%t(Xa3)%*%ya3)
rho3
d3	<- abs(rho2-rho3)
d3

#Iterasi 4
y4	<- y[-1,]-rho3*y[-n,]
X4	<- X[-1,]-(X[-n,]*rho3)

# taksiran beta
be_4	<- solve(t(X4)%*%X4)%*%t(X4)%*%y4

# residual
e4	<- y-X%*%be_4

# autokorelasi 
ya4	<- e4[-1,]	
Xa4	<- e4[-n,]

rho4	<- as.vector(solve(t(Xa4)%*%Xa4)%*%t(Xa4)%*%ya4)
rho4
d4	<- abs(rho3-rho4)
d4

e_gls	<- y4-X4%*%be_4
s2_gls	<- as.vector((t(e_gls)%*%e_gls)/(n-2))

se_be_gls	<-sqrt(diag(s2_gls*(solve(t(X4)%*%X4))))

