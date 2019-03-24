#Time series
setwd("E:\\Jigsaw\\BC1\\DataSets\\Class-Datasets\\Class Datasets")
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)

milk<-read.csv("milk.csv",stringsAsFactors = F) #here strings as factors T or F doesnt matter
View(milk)
class(milk)
#File should not have missing values...if present impute or check same month last year data

milk<-milk[,-1]#REmoving first column
milk<-as.data.frame(milk)#we have only 1 col so vector so convertto data frame
View(milk)
dim(milk)
sum(is.na(milk))#Check for missing values..never remove..either impute or ask business
class(milk)

#convert the data frame into a time series object using ts() function
ml<-ts(milk,start=1962,frequency=12) #12 months and start year is 1962
class(ml)
ml
start(ml)
end(ml)
frequency(ml)
cycle(ml)
plot(ml)#increasing trend


aggregate(ml,FUN=mean)#gives each cycle sum
plot(aggregate(ml,FUN=mean))#increasing trend
     
#Box plot time series
#For each month we get a box plot

b1<-boxplot(ml~cycle(ml))#for every cycle of ml we get a box plot
b1
#Every year production increases from jan to june

#Partioning data set
#Here we create 2 windows:1962 to 1972-training data set then 1972-74 test data set

mltr<-window(ml,start=c(1962,1),end=c(1971,12),frequency=12)
dim(mltr)

mlt<-window(ml,start=c(1972,1),end=c(1974,12),frequency=12)
dim(mlt)

plot(mltr)
plot(mlt)

#WE create a t s model on 1962-1972
#forecast for period 1972-1974

#Decomposing is an extra step here:optional
dec<-decompose(mltr)
plot(dec)
#Top panel has original time series
#second contains trend
#third seasonality
#last random fluctuations

#exponential smoothing on training window dataset
#SES
#to forecast milk production for next 10 months
?ses
es<-ses(mltr,h=10)#time period -Forecast for next 10 months
plot(es)#The blue part here is the forecast,inner shadedarea says 80%confident ble line lies within this range and outer shaded area tells 95% confidence
summary(es)
es$x#these are original values
es$fitted#this algo doesnot take into consideration trend or seasonality
es$residuals#diff b/w actual and fitted

100*es$residuals/es$x  #PE
mean(abs(100*es$residuals/es$x))#MAPE

#Accuracy o exponential smoothing
accuracy(es,mlt)#Automatically calculates MAPE for train and test

#checking residuals
checkresiduals(es)
#1st chart plots residuals-there should be no pattern ..in case pattern exists bad model like in this case
#2nd auto correlation graph
#all spikes should lie within the blue lines
#not an accurate forecast


#Hold's method-takes care of trend
hol<-holt(mltr,h=10)
plot(hol)#Better graph because it takes care of trend
summary(hol)
accuracy(hol,mlt)#check mape values
checkresiduals(hol) #Spikes are much better

#Ljung test-check pvalue here
Box.test(hol$residuals,lag=20,type="Ljung-Bo")
#Lag is diffbw current and previous value

?checkresiduals
#Blue line is slightly elevated meaning that forecast is better than previous one
##Ljung test reveals that p value is low
#REfer notebk notes

#Hold winter-takes care of trend and seasonality
hwTs<-hw(mltr,h=10)
hwTs

plot(hwTs)
accuracy(hwTs,mlt)
summary(hwTs)
checkresiduals(hwTs)#now all spikes b/w  blue lines
#All spikes below blue line
#no pattern in residuals
#histogram closer to bell shape

#Automatic Model building using ets()-alt way of hold winter
??eds
auto<-ets(mltr)
summary(auto)

#MAPE less than 7 so seems to be a good mape
foc<-forecast(auto,h=10)
foc
plot(foc)
checkresiduals(foc)+
#P value should be more than 0.05 so good model


#ARIMA-Auto Regressive Integrated Moving Average-considers trend and seasonality
auto.arima(mltr)
#We get values trend-p-0,d-1,q-0 1st set
#seasonal p-0,d=1,q=1 2nd set
?arima
mltrarima<-arima(mltr,c(0,1,0),
                 seasonal=list(order=c(0,1,1),period=12))
mltrarima
mltrarimaF<-forecast(mltrarima,h=10)
plot(mltrarimaF)

#Validating the model
checkresiduals(mltrarimaF)

install.packages("fpp")
library(fpp)
