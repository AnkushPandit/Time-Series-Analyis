library(forecast)
library(astsa)
library(tseries)
#Load Orginal Data
data<-read.csv("C://Users//dell//Downloads//Time Series//WTI Price FOB.csv")
oil_data_df<-data$Cushing..OK.WTI.Spot.Price.FOB..Monthly..Dollars.per.Barrel.

#Data cleaning and EDA
#length(oil_data_df)
whole_data<-ts(oil_data_df, frequency = 12)
plot(whole_data)
oil_data<-oil_data_df[0:284]
plot(oil_data)
ts<-ts(oil_data, frequency=12)
plot(ts)
log_ts<-log(ts)
plot(log_ts)
end(log_ts)
#ts_clean<-tsclean(ts)
#ts_clean<-ts
#plot(ts_clean)
ts_test<-ts(oil_data_df[285:300], start=c(24,9), frequency=12)
log_ts_test<-log(ts_test)
points(log_ts_test)
points(log(whole_data), col="red")

ddata<-decompose(log_ts, "multiplicative")
plot(ddata)
lag1.plot(ts_clean, 1)
acf2(log_ts)
#acf(ts_clean)
#pacf(ts_clean)

#Making stationary by diff 1
diff_ts<-diff(log_ts, differences = 1)
plot(diff_ts)
acf2(diff_ts)
adf.test(diff_ts)
#acf(diff_ts ,xlim=c(0.1:2))
#pacf(diff_ts)

#Making stationary by diving by trend component
st_data<-log_ts/ddata$trend
#st_data<-ts/ddata$seasonal
plot(st_data)
acf2(st_data)
acf(st_data, na.action = na.pass, xlim=(0.1:2))
pacf(st_data,  na.action = na.pass)

#Prediction for approach 1
mymodel1<-auto.arima(diff_ts, trace = T)
mymodel1 #ARIMA(1,0,0)(0,0,2)[12] with zero mean
#acf(ts(mymodel1$residuals),na.action = na.pass)
#pacf(ts(mymodel1$residuals),na.action = na.pass)
#predict1<-predict(mymodel1, n.ahead = 25)
#predict1$pred
sarima(log_ts,2,1,3,1,0,0,12)
pred<-sarima.for(log_ts,16,2,1,3,1,0,0,12)
points(log_ts_test, col="blue")
log_ts_test
pred$pred
mae(log_ts_test,pred$pred)
#points(log_ts)

#Prediction for approach 2
mymodel2<-auto.arima(st_data, trace = T)
mymodel2 #ARIMA(2,0,2)(1,0,0)[12] with non-zero mean 
acf(ts(mymodel2$residuals),na.action = na.pass)
predict2<-predict(mymodel2, n.ahead = 25)
predict2$pred
sarima(st_data,4,0,0,1,0,1,12)

#Approach 3
mymodel3<-auto.arima(ts_clean, trace = T)
mymodel3<-arima(ts_clean, c(1,1,1), c(0,0,2))
mymodel3 #ARIMA(1,0,0)(0,0,2)[12] with zero mean
acf(ts(mymodel3$residuals),na.action = na.pass)
pacf(ts(mymodel3$residuals),na.action = na.pass)
predict3<-predict(mymodel3, n.ahead = 25)
predict3$pred


#Plot for 1
plot(NULL, xlim=c(0,40), ylim=c(0,16), ylab="y label", xlab="x lablel")
points(diff_ts, col="Red", type="b")
points(predict1$pred, col="Yellow",  type="b")

#Plot for 2
plot(NULL, xlim=c(0,40), ylim=c(1,2), ylab="y label", xlab="x lablel")
points(ts_clean)
points(st_data*ddata$trend, col="Red", type="b")
points(predict2$pred, col="Yellow",  type="b")


#Plot for 3
plot(NULL, xlim=c(0,40), ylim=c(0,120), ylab="y label", xlab="x lablel")
points(ts_clean, col="Red", type="b")
points(predict3$pred, col="Yellow",  type="b")
points(ts_test)
