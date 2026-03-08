mydata <- read.table("C:/Users/Owner/Downloads/ARIMAsim2.dat", header = TRUE)

ts1 <- ts(mydata$ts1)
ts2 <- ts(mydata$ts2)
ts3 <- ts(mydata$ts3)

x11()
plot(ts1)
plot(ts2)
plot(ts3)

diff_ts3 <- diff(ts3)
plot(diff_ts3)

acf(ts1)
pacf(ts1)
acf(ts2)
pacf(ts2)
acf(diff_ts3)
pacf(diff_ts3)

arima(ts1, order = c(4,0,3))
arima(ts1, order = c(4,1,3))
arima(ts1, order = c(4,0,2)) #lag2 close to to insignificance

arima(ts2, order = c(4,0,4))
arima(ts2, order = c(4,1,4))
arima(ts2, order = c(4,0,3))

arima(ts3, order = c(2,1,3))
arima(diff_ts3, order = c(2,0,3))
arima(diff_ts3, order = c(2,1,3))

acf(residuals(ts1))
acf(residuals(ts2))
acf(residuals(diff_ts3))