# Analysing daily mean measurements of the nitrous oxides level

# Load the data
a23_nox <- read.csv("./a23_nox.csv", header = TRUE)

# time series
daily_mean_nox <- ts(a23_nox$daily_mean_nox, start = 1, end = 242)

# Plot the time series
par(mfrow = c(1, 1))
ts.plot(daily_mean_nox, xlab="Day", ylab="NOx (mic_g/m3)", main="NOx Concentration at A23")

# Plot the ACF and PACF
par(mfrow = c(1, 2))
acf(daily_mean_nox, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of NOx")
pacf(daily_mean_nox, lag.max = 30, xlab="Lag", ylab="PACF", main="PACF of NOx")

# Apply Seasonal Differencing at lag 7
daily_mean_nox_diff <- diff(daily_mean_nox, differences = 1, lag = 7)

par(mfrow = c(1, 1))
ts.plot(daily_mean_nox_diff, xlab="Day", ylab="NOx (mic_g/𝑚3)", main="7 days diff NOx Concentration at A23")

# Plot the ACF and PACF
par(mfrow = c(1, 2))
acf(daily_mean_nox_diff, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF")
pacf(daily_mean_nox_diff, lag.max = 30, xlab="Lag", ylab="PACF", main="PACF")

# Apply Differencing at lag 1
daily_mean_nox_diff <- diff(daily_mean_nox_diff, differences = 1)

par(mfrow = c(1, 1))
ts.plot(daily_mean_nox_diff, xlab="Day", ylab="NOx (mic_g/𝑚3)", main="NOx Concentration at A23")

# Plot the ACF and PACF
par(mfrow = c(1, 2))
acf(daily_mean_nox_diff, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF")
pacf(daily_mean_nox_diff, lag.max = 30, xlab="Lag", ylab="PACF", main="PACF")

# Modeling

# MA(1) Model
model.1 <- arima(daily_mean_nox, order = c(0, 1, 1), method = "ML", seasonal = list(order = c(0, 1, 0), period = 7))
model.1

# Plot the residuals
resid.model.1 <- residuals(model.1)
par(mfrow = c(1, 1))
ts.plot(resid.model.1, xlab="Day", ylab="Residuals", main="Time Plot or Residuals")

par(mfrow = c(1, 1))
acf(resid.model.1, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.1 <- LB_test_SARIMA(resid.model.1, max.k = 11, p = 0, q = 1, P=0, Q=0)
plot(LB.1$deg_freedom, LB.1$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)

# AR(1) Model
model.2 <- arima(daily_mean_nox, order = c(1, 1, 0), method = "ML", seasonal = list(order = c(0, 1, 0), period = 7))
model.2

# Plot the residuals
resid.model.2 <- residuals(model.2)
par(mfrow = c(1, 1))
ts.plot(resid.model.2, xlab="Day", ylab="Residuals", main="Time Plot or Residuals")

par(mfrow = c(1, 1))
acf(resid.model.2, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.2 <- LB_test_SARIMA(resid.model.2, max.k = 11, p = 1, q = 0, P=0, Q=0)
plot(LB.2$deg_freedom, LB.2$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)


# ARIMA(1, 1, 1) Model
model.3 <- arima(daily_mean_nox, order = c(1, 1, 1), method = "ML", seasonal = list(order = c(0, 1, 0), period = 7))
model.3

# Plot the residuals
resid.model.3 <- residuals(model.3)
par(mfrow = c(1, 1))
ts.plot(resid.model.3, xlab="Day", ylab="Residuals", main="Time Plot or Residuals")

par(mfrow = c(1, 1))
acf(resid.model.3, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.3 <- LB_test_SARIMA(resid.model.3, max.k = 11, p = 1, q = 1, P=0, Q=0)
plot(LB.3$deg_freedom, LB.3$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)


# ARIMA(1, 1, 1)(1,1,1) Model
model.4 <- arima(daily_mean_nox, order = c(1, 1, 1), method = "ML", seasonal = list(order = c(1, 1, 1), period = 7))
model.4

# Plot the residuals
resid.model.4 <- residuals(model.4)
par(mfrow = c(1, 1))
ts.plot(resid.model.4, xlab="Day", ylab="Residuals", main="Time Plot or Residuals")

par(mfrow = c(1, 1))
acf(resid.model.4, lag.max = 30, xlab="Lag", ylab="ACF", main="ACF of Residuals")

# Ljung-Box test
LB.4 <- LB_test_SARIMA(resid.model.4, max.k = 11, p = 1, q = 1, P=1, Q=1)
plot(LB.4$deg_freedom, LB.4$LB_p_value, xlab="Degrees of Freedom", ylab="P-value", main="Ljung-Box Test", ylim=c(0,1))
abline(h=0.05, col="blue", lty=2)