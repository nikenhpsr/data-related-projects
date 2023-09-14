data=read.csv(file.choose(), sep = ";", header = T)
data1=ts(data,frequency=12,start=2009)
data1

library(tseries)
library(TSA)
library(timeSeries)
library(starma)
library(MTS)
library(fGarch)
library(portes)
library(MVN)

######
#plot acf dan pacf
ts.plot(data1)
acf(data1,lag.max=50) #pengulangan setiap 12 periode
pacf(data1,lag.max=50)

#Pengujian Stasioner
library(tseries)
adf.test(data1)
#pvalue=0.01

#Data differencing musiman
data2=diff(data1,lag=12,differences=1)
ts.plot(data2)
adf.test(data2)

#acf pacf
acf(data2,lag.max=40)
