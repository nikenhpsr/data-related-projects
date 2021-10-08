#Input Data
data=read.csv("datademand.csv",header=T,sep=",")
data

#Cek Stasioneritas
library(tseries)
dt=ts(data,frequency=12,start=2018)
dt
ts.plot(dt,main="Permintaan Persediaan",col="red",lwd=2)
adf.test(dt)

#ACF PACF data awal (sebelum differencing)
acf(dt, main="ACF Permintaan Persediaan",lag=12)
pacf(dt,main="PACF Permintaan Persediaan",lag=12)
#lag=... itu maksudnya kalian mau liat plot acf sama pacfnya sampai lag ke berapa, itu optional diisi tergantung kebutuhan

#Differencing
dt1=diff(dt,differences=1)
dt1
adf.test(dt1)
ts.plot(dt1,main="Permintaan Persediaan",col="red")
