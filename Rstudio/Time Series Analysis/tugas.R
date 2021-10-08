data=matrix(c(13,8,15,4,4,12,11,7,14,12),byrow = T)
library(tseries)

dt=ts(data,frequency=10)
dt
ts.plot(dt,main="",col="red",lwd=2)
adf.test(dt)

#ACF PACF data awal (sebelum differencing)
acf(dt, main="",lag=3)
pacf(dt,main="PACF Permintaan Persediaan",lag=3)
#lag=... itu maksudnya kalian mau liat plot acf sama pacfnya sampai lag ke berapa, itu optional diisi tergantung kebutuhan

#Differencing
dt1=diff(dt,differences=1)
dt1
adf.test(dt1)
ts.plot(dt1,main="Permintaan Persediaan",col="red")
