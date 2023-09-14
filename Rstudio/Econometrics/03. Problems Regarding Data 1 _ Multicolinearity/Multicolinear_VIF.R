#mencari VIF
mkorelasi= cor(X)
kov=cov(X)
VIFVIF=diag(solve(mkorelasi))

#mencari outlier
h=X%*%solve(t(X)%*%X)%*%t(X)
sum(diag(h))
i=diag(1,n,n)

e=(i-h)%*%Y
e1= Y-X%*%b

ssr=e/as.vector(s2^0.5)

I=matrix(c(1),n,n)
s=s2^0.5
r=e/(s*(I-h)^0.5)
