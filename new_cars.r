# Load the Data
eng_car_reg <- read.csv("./eng_car_reg.csv", header = TRUE)

# time series
no_new_reg <- ts(eng_car_reg$no_new_reg,start=c(2001,1),frequency=4)

# Plot the time series
par(mfrow = c(1, 1))
ts.plot(no_new_reg, xlab="Year", ylab="Number of New Registrations (in thousands)", main="Number of New Registrations of Cars in England")

# Plot the ACF and PACF
par(mfrow = c(1, 1))
acf(no_new_reg, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Number of New Registrations of Cars")
pacf(no_new_reg, lag.max = 30, xlab="Lag", ylab="PACF", main="PACF of Number of New Registrations of Cars")

# difference seasonally at lag 4
no_new_reg_diff <- diff(no_new_reg, differences = 1, lag = 4)

# Produce the time plot
par(mfrow = c(1, 1))
ts.plot(no_new_reg_diff, xlab="Year", ylab="New Registrations (in thousands)", main="New Registrations of Cars - Difference")

# ACF of difference
par(mfrow = c(1, 1))
acf(no_new_reg_diff, lag.max = 30, xlab="Lag", ylab="ACF", main="New Registrations of Cars ACF - Difference")

# take the first difference of the seasonally differenced data.
no_new_reg_diff2 <- diff(no_new_reg_diff, differences = 1)

# Produce the time plot
par(mfrow = c(1, 1))
ts.plot(no_new_reg_diff2, xlab="Year", ylab="New Registrations (in thousands)", main="New Registrations of Cars - 1st Difference")

# ACF of difference
par(mfrow = c(1, 1))
acf(no_new_reg_diff2, lag.max = 30, xlab="Lag", ylab="ACF", main=paste("New Registrations of Cars ACF", "\n- 1st Difference"))
pacf(no_new_reg_diff2, lag.max = 30, xlab="Lag", ylab="PACF", main=paste("New Registrations of Cars PACF", "\n- 1st Difference"))


# ARIMA(0,1,1)(0,1,0)4
model.1 <- arima(no_new_reg, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 0), period = 4))
model.1

resid.model.1 <- model.1$residuals
par(mfrow = c(1, 1))
ts.plot(resid.model.1, xlab="Year", ylab="Z(t)", main="Time Plot of Residuals")
acf(resid.model.1, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.1 <- LB_test_SARIMA(resid.model.1, max.k = 11, p = 0, q = 1, P=0, Q=0)
plot(LB.1$deg_freedom, LB.1$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# ARIMA(1,1,0)(0,1,0)4
model.2 <- arima(no_new_reg, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 4))
model.2

resid.model.2 <- model.2$residuals
par(mfrow = c(1, 1))
ts.plot(resid.model.2, xlab="Year", ylab="Z(t)", main="Time Plot of Residuals")
acf(resid.model.2, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.2 <- LB_test_SARIMA(resid.model.2, max.k = 11, p = 1, q = 0, P=0, Q=0)
plot(LB.2$deg_freedom, LB.2$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# ARIMA(1,1,1)(0,1,0)4
model.3 <- arima(no_new_reg, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 0), period = 4))
model.3

resid.model.3 <- model.3$residuals
par(mfrow = c(1, 1))
ts.plot(resid.model.3, xlab="Year", ylab="Z(t)", main="Time Plot of Residuals")
acf(resid.model.3, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.3 <- LB_test_SARIMA(resid.model.3, max.k = 11, p = 1, q = 1, P=0, Q=0)
plot(LB.3$deg_freedom, LB.3$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# ARIMA(1,1,1)(1,1,0)4
model.4 <- arima(no_new_reg, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 4))
model.4

resid.model.4 <- model.4$residuals
par(mfrow = c(1, 1))
ts.plot(resid.model.4, xlab="Year", ylab="Z(t)", main="Time Plot of Residuals")
acf(resid.model.4, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.4 <- LB_test_SARIMA(resid.model.4, max.k = 11, p = 1, q = 1, P=1, Q=0)
plot(LB.4$deg_freedom, LB.4$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# ARIMA(1,1,1)(0,1,1)4
model.5 <- arima(no_new_reg, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 4))
model.5

resid.model.5 <- model.5$residuals
par(mfrow = c(1, 1))
ts.plot(resid.model.5, xlab="Year", ylab="Z(t)", main="Time Plot of Residuals")
acf(resid.model.5, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.5 <- LB_test_SARIMA(resid.model.5, max.k = 11, p = 1, q = 1, P=0, Q=1)
plot(LB.5$deg_freedom, LB.5$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# ARIMA(1,1,1)(1,1,1)4
model.6 <- arima(no_new_reg, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 4))
model.6

resid.model.6 <- model.6$residuals
par(mfrow = c(1, 1))
ts.plot(resid.model.6, xlab="Year", ylab="Z(t)", main="Time Plot of Residuals")
acf(resid.model.6, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.6 <- LB_test_SARIMA(resid.model.6, max.k = 11, p = 1, q = 1, P=1, Q=1)
plot(LB.6$deg_freedom, LB.6$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# Forecasting

# Fit the model
model <- arima(no_new_reg, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 4))

# Forecast for the next 1 quarter 
library(forecast)
fc_1q <- forecast(no_new_reg,h=1,model=model,level=95)
autoplot(fc_1q, main="Forecast Q4 2022", xlab="Year", ylab="No. of New Registrations")

# Forecast for the next 4 quarters with 95% and 80% confidence intervals
fc_4q <- forecast(no_new_reg,h=4,model=model,level=95)
autoplot(fc_4q, main=" ", xlab="Year", ylab="No. of New Registrations")
fc_4q
