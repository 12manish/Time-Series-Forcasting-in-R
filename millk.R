getwd()
setwd("E:\\Jigshaw lectures\\R STUDIO CLASS\\time series")
install.packages("forecast") 
install.packages("tseries")
milk<-read.csv("milk.csv",stringsAsFactors = F)

View(milk)
class(milk)
milk<-milk[,2]  #first column deleted
class(milk)
milk <- as.data.frame(milk)
dim(milk)
colnames(milk)[1]<-"R1"
               

#convert the data frame into atime series object, using ts() fumction belong to forecast
?ts
ml<-ts(milk,start =1962, frequency = 12)
class(ml)
ml
start(ml)
end(ml)
frequency(ml)
cycle(ml)
plot(ml)


#aggregate
?aggregate
aggregate(ml)   #adding year wise= 12months (sum)
plot(aggregate(ml))

#increasing trend
aggregate(ml,FUN = mean)   #particular mean wise we want to dothen FUN=mean
plot(aggregate(ml,FUN = mean))

#milk production has been increasing year after year

#box plot creates one boxplot for each month

?boxplot
b1<-boxplot(ml~cycle(ml))    #year jan for all year seasonality for each yaer
b1

#subsetting the time series using window function
#we shall create  two window (1962-1972) and (1972-1974)
#creating training and testing window datasets
#frequency is the number of the time period in a year


mlTr<-window(ml,start = c(1962,1),end=c(1971,12),frequency=12)
dim(mlTr)


mlT<-window(ml,start = c(1972,1),end = c(1974,12),frequency=12)
dim(mlT)


plot(mlTr)
plot(mlT)

#we shall create a time series model on (1962 to 1972)          and forecasting 

#we shall then compare the forecast with actual time series

  
?decompose
dec<-decompose(mlTr)
plot(dec)


#top panel contains the original time series
#second panel contains the trend
#third panel contains seasonality componenet
#last panel contains random fluctuation

#simple exponential smoothing;=

?ses
#to forcast milk production for the next 10 months
library(forecast)
es<-ses(mlTr,h=10) #no. of time peri
plot(es)
summary(es)
  #forcasting value for the next month


es$x
es$fitted
es$residuals #es$x-es$fitted



100*es$residuals/es$x   #PE
mean(abs(100*es$residuals/es$x))   #MAPE

#finding the accuracy of 
accuracy(es,mlT)


#checking residual #plotting of residual :-pattern
#heteroscadicity :- variation:--pattern
checkresiduals(es)

#nrow(mlTr)
#2*1/sqrt(120)
#68%= +- 1sigma



#fitting holt's model
?holt
hol<-holt(mlTr,h=10)   #forecasting for 10 period
plot(hol)
summary(hol)
accuracy(hol,mlT)  #test vs train
checkresiduals(hol)


?Box.test
Box.test(hol$residuals,lag = 20,type = "Ljung-Bo")
#blue line is slightly elevated, meaning that this forecast is 
#better than the previous one
#The ljung box test reveal that p value is low
#HO:data is indentically and independent distributed(white noise)
#HA:data is not identically and independently 
#distributed(exhibts serial corelation)
#therefore we rejcet H0 and conclude that data exhits
#serial corelation

#clearly the series is seasinal so seasonal component is required





?checkresiduals

hwTS<-hw(mlTr,h=10)
hwTS

plot(hwTS)
accuracy(hwTS,mlT)
summary(hwTS)
checkresiduals(hwTS)


# P VALUE is high indicationg that data is identically and independent distributed
#ALL spikes are below blue line
#there is no pattern in the residuals 
#Histogram is also closer to bell- shaped 


#automating model building using ets()
?ets
auto<-ets(mlTr)
summary(auto)
#MAPE IS LESS THAN 7 and its seem 

foc<-forecast(auto,h=10)
foc<-forecast(hwTS,h=10)
foc
plot(foc)

checkresiduals(foc)


#ARIMA 
?Arima
auto.arima(mlTr)
mlTrarima<-arima(mlTr,c(0,1,0),
                 seasonal = list(order = c(0,1,1),period =12))
                 
                 
mlTrarima
mlTrarimaF<-forecast(mlTrarima,h=10)
plot(mlTrarimaF)
#validating the model
summary(mlTrarimaF)
checkresiduals(mlTrarimaF)
mlTrarimaF

