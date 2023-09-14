## Input Data
data=read.csv("bulanan_nias.csv",sep=";")
data
attach(data)
X1=data$WNPMI

## Plot
data_X1=ts(X1,start=c(2011,1),frequency=12)
ts.plot(data_X1,ylab="",main="PLOT DATA WNPMI",col="magenta",lwd=2)

# ADF Test X1
adf.test(X1)

# Differencing 1
dataX1=diff(X1)
plot.ts(dataX1)
adf.test(dataX1)

## Differencing musiman 
X1t=diff(dataX1, lag=12,differences = 1)
ts.plot(X1t)
adf.test(X1t)

## Uji ACF PACF WNPMI
# ACF PACF (X1)
acf(X1t, main="ACF WNPMI (D=1)",lag=40)
axis(1,at=seq(1,40,by=1))
pacf(X1t, main="PACF WNPMI (D=1)",lag=40)
axis(1,at=seq(5,40,by=1))

## Estimasi Parameter WNPMI
b1=Arima(data_X1,order=c(1,1,1),seasonal=list(order=c(1,1,0),period=12));b1
b2=Arima(data_X1,order=c(1,1,2),seasonal=list(order=c(1,1,0),period=12));b2
b3=Arima(data_X1,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12));b3
b4=Arima(data_X1,order=c(1,1,2),seasonal=list(order=c(1,1,1),period=12));b4
b5=Arima(data_X1,order=c(1,1,1),seasonal=list(order=c(1,1,2),period=12));b5
b6=Arima(data_X1,order=c(1,1,2),seasonal=list(order=c(1,1,2),period=12));b6
b7=Arima(data_X1,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12));b7
b8=Arima(data_X1,order=c(1,1,2),seasonal=list(order=c(0,1,1),period=12));b8
b9=Arima(data_X1,order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12));b9
b10=Arima(data_X1,order=c(1,1,2),seasonal=list(order=c(0,1,2),period=12));b10
b11=Arima(data_X1,order=c(0,1,1),seasonal=list(order=c(1,1,0),period=12));b11
b12=Arima(data_X1,order=c(0,1,2),seasonal=list(order=c(1,1,0),period=12));b12
b13=Arima(data_X1,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12));b13
b14=Arima(data_X1,order=c(0,1,2),seasonal=list(order=c(1,1,1),period=12));b14
b15=Arima(data_X1,order=c(0,1,1),seasonal=list(order=c(1,1,2),period=12));b15
b16=Arima(data_X1,order=c(0,1,2),seasonal=list(order=c(1,1,2),period=12));b16
b17=Arima(data_X1,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12));b17
b18=Arima(data_X1,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12));b18
b19=Arima(data_X1,order=c(0,1,1),seasonal=list(order=c(0,1,2),period=12));b19
b20=Arima(data_X1,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12));b20
b21=Arima(data_X1,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12));b21
b22=Arima(data_X1,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=12));b22
b23=Arima(data_X1,order=c(1,1,0),seasonal=list(order=c(1,1,2),period=12));b23
b24=Arima(data_X1,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12));b24
b25=Arima(data_X1,order=c(1,1,0),seasonal=list(order=c(0,1,2),period=12));b25
#hasil: b7 dengan nilai aic terkecil

## UJI SIGNIFIKANSI PARAMETER
printstatarima <- function (x, digits = 4,se=T,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = F)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

## Uji Signifikansi
printstatarima(b7)

## Non Autokorelasi
# Residu
rX1=residuals(b7);rX1
wnX1=Box.test(rX1,lag=1,type=c("Ljung-Box"));wnX1
#hasil : pval=0.7655 ; H0 diterima

## NORMALITAS WNPMI
# H0 : residual berdistribusi normal
# H1 : residual tidak berdistribusi normal
nX1=length(rX1)
meanX1=mean(rX1)
sdX1=sd(rX1)
resX1=rnorm(nX1,meanX1,sdX1)
hasilX1=ks.test(rX1,resX1)
hasilX1

## Uji Homoskedastisitas
# H0 : varians kekeliruan homogen
# H1 : varians kekeliruan heterogen
Box.test(rX1^2, type=c("Ljung-Box"))
#hasil : 0.3375, H0 diterima

##FORECAST
forecastX1=predict(arima(data_X1,order=c(1,1,1),seasonal=list(order=c(0,1,1))),n.ahead=16)
forecastX1
