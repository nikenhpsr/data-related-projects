install.packages("forecast")
install.packages("tidyverse")
install.packages("fpp2")
library(forecast)
library(tidyverse)
library(fpp2)

## Single Exponential Smoothing
data1=read.csv("data_ses.csv",sep=";",header=T)
datases=ts(data1$Average.Temp,start=c(1956,1),frequency=12)
plot.ts(datases)
analisis=ses(datases, h =1, alpha = 0.3);analisis
summary(analisis)
forecast=predict(analisis$fitted,5);forecast

## Holt Exponential Smoothing (trend)
data2=read.csv("bond.csv",sep=";",header=T)
dataholt=ts(data2$Yield_Rate,start=c(2014,1),frequency=12)
plot.ts(dataholt)
forecast2=HoltWinters(dataholt);forecast2
plot(forecast2)
#Forecast (trial=1)
holtforecast=holt(dataholt,h=12,alpha=0.5010704,beta=0.0722999);holtforecast
summary(holtforecast)
plot(holtforecast)

## Holt Winters (seasonal, trend)
forecast3=HoltWinters(dataholt);forecast3
#Forecast (trial=1)
plot.ts(dataholt)
holtwinterforecast=hw(dataholt,seasonal="additive",alpha = 0.9486985,beta= 0,
	   gamma = 1,h=12);forecast3
plot(holtwinterforecast)
summary(holtwinterforecast)
#Forecast (trial=2)
plot.ts(dataholt)
holtwinterforecast2=hw(dataholt,seasonal="multiplicative",alpha = 0.6541333, beta = 0.0528399,
			gamma = 0.1,h=6);forecast3
summary(holtwinterforecast2)
plot(holtwinterforecast2)




