data=read.csv(file.choose(), sep = ";", header = T)
a=data[1:72,2]
library(forecast)
misal=auto.arima(a)
checkresiduals(misal)
forecast(a)

#Cek Stasioneritas
library(tseries)
dt=ts(a,frequency=12,start=2014)
dt
ts.plot(dt,main="Yield Rate (%)",col="red",lwd=2)
adf.test(a)

#ACF PACF data awal (sebelum differencing)
acf(a, main="ACF Yield Rate (%)")
pacf(a,main="PACF Yield Rate (%)")
#dari sini dpt model ARIMA(1,1,0)
Arima(a, order = c(1,1,0))

#Differencing
dt1=diff(a,differences=1)
dt1
adf.test(dt1)
ts.plot(dt1,main="Yield Rate (%)",col="red")
acf(dt1, main="ACF Yield Rate (%)",lag.max = NULL )
pacf(dt1,main="PACF Yield Rate (%)", lag.max = NULL)
#dari sini dpt ACF sama PACF nya tidak signifikan jadi, modelnya ARIMA (0,1,0)
Arima(a, order = c(0,1,0))

library(forecast)
misal=auto.arima(a)
checkresiduals(misal)
forecast(a,level = c(95), model = Arima(a, order = c(0,1,0)))
Arima(a, order = c(1,1,0))
Arima(a, order = c(0,1,0))

#H0: data nya white noise, alpha=0.05
revisi=auto.arima(dt1)
forecast(revisi, 5)
b=BoxCox(a, lambda="auto")
revisi=auto.arima(dt1)

checkresiduals(revisi)

