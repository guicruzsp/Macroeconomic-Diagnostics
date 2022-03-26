library(rbcb)
library(ipeadatar)
library(xts)
library(zoo)
library(mFilter)

Y_data <- get_series("22109", as = "xts")

##### Estimation using Linear Time Trend #####

# Flaws of the model:
# - assumes constant growth rate of potential output

reg = lm(log(Y_data) ~ seq(1,nrow(Y_data)))

plot(exp(reg$model[,1]), main = "Log GDP and Trend")
lines(exp(reg$fitted.values))

plot((exp(reg$model[,1]) - exp(reg$fitted.values))/exp(reg$fitted.values), main = "Output Gap")

##### Estimation using HP filter #####

# Flaws of the model:
# - No theoretical foundation
# - Endpoint problem: Last point is biased

# Lambda parameter suggested values:
# 100   - yearly data
# 1600  - quarterly data
# 14400 - monthly data

filtered_data <- hpfilter(Y_data, freq = 1600)

plot(filtered_data)


##### Estimation using production function approach #####

# Flaws:
# - Requires several economic data series as inputs
# - Relies on estimates of the inputs
# - Requires us to make an assumption about the exact form 
# of the aggregate production function

# We need to estimate:
# - Full employment
# - Capital at full capacity utilization
#  - Trend TFP

# We will use Cobb-Douglas

# Steps:
#
# 1. Use Y, K and L to estimate TFP
# 2. Estimate K*
# 3. estimate L*
# 4. Estimate TFP - TFP*
# 5. Plug TFP*, K* and L* into Cobb-Douglas and obtain Y*


K_data <- ipeadata(code = "DIMAC_ECFLIQTOT4", language = "br")
Y_data <- get_series("22109", as = "xts")
L_data <- get_series("28784", as = "xts")

K_data <- xts(K_data$value, order.by = as.Date(K_data$date))

full_dataset <- merge(Y_data,K_data)
full_dataset <- merge(full_dataset,L_data)

colnames(full_dataset) <- c("Y_data","K_data","L_data")

full_dataset$L_data[1:length(na.approx(full_dataset$L_data))] <- na.approx(full_dataset$L_data)
full_dataset <- full_dataset[!is.na(full_dataset$Y_data),]
full_dataset <- full_dataset[!is.na(full_dataset$L_data),]
full_dataset$Y_growth <- diff(full_dataset$Y_data)/lag(full_dataset$Y_data)
full_dataset$K_growth <- diff(full_dataset$K_data)/lag(full_dataset$K_data)
full_dataset$L_growth <- diff(full_dataset$L_data)/lag(full_dataset$L_data)

reg <- lm((full_dataset$Y_growth) ~ full_dataset$K_growth + full_dataset$L_growth)
summary(reg)

full_dataset$TFP_growth <- full_dataset$Y_growth - coef(reg)[2]*full_dataset$K_growth - coef(reg)[3]*full_dataset$L_growth
full_dataset$TFP_estimate <- full_dataset$Y_data/(full_dataset$K_data^coef(reg)[2] * full_dataset$L_data^coef(reg)[3])

full_dataset <- full_dataset[!is.na(full_dataset$K_growth),]

# Finding K*, L* and TFP*

K_star   <- exp(hpfilter(log(full_dataset$K_data), freq = 1600)$trend)
L_star   <- exp(hpfilter(log(full_dataset$L_data), freq = 1600)$trend)
TFP_star <- exp(hpfilter(log(full_dataset$TFP_estimate), freq = 1600)$trend)

Y_star <- TFP_star*K_star^(coef(reg)[2])*L_star^(coef(reg)[3])

plot(as.numeric(Y_data), type = "l", main = "GDP and Potential Output")
lines(Y_star, col = "red")

plot((Y_data[-c(1,nrow(Y_data)),] - Y_star)/Y_star, main = "Output Gap")

