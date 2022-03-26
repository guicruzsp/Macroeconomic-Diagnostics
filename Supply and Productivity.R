##### Deterministic Model without technological growth #####

# We use labor augmenting type of technical progress

n_periods = 200
# State Variables
A   = 1 # Total Factor Productivity
Y   = rep(0, n_periods) # Product
K   = rep(0, n_periods) # Capital
L_F = rep(0, n_periods) # Labor Force
L   = rep(0, n_periods) # Labor
I   = rep(0, n_periods) # Investment
ER  = rep(0.95, n_periods) # Employment rate

# Parameters
alpha = 0.3  # Growth elasticity for capital/Capital share of output
delta = 0.03 # Depreciation rate
n = 0.01     # Labor supply growth rate
s = 0.2      # Savings rate

# Initial positions
I[1] = 0.2 
Y[1] = 1
K[1] = 1
L_F[1] = 1

# Dynamic of the model
for(t in 2:n_periods){
  L_F[t] = L_F[t-1]*exp(n)
  L[t] = L_F[t]*ER[t]
  K[t] = K[t-1] + I[t-1] - K[t-1]*delta
  Y[t] = K[t]^(alpha) * (A * L[t])^(1-alpha)
  I[t] = s*Y[t]
}

par(mfrow = c(2,2))
plot(Y/L_F)
plot(K/L_F)
plot(diff(Y/L_F)/((Y/L_F)[-1]))
plot(diff(K/L_F)/((K/L_F)[-1]))



##### Deterministic Model with technological growth #####

# We use labor augmenting type of technical progress

n_periods = 200
# State Variables
A   = rep(0, n_periods) # Total Factor Productivity
Y   = rep(0, n_periods) # Product
K   = rep(0, n_periods) # Capital
L_F = rep(0, n_periods) # Labor Force
L   = rep(0, n_periods) # Labor
I   = rep(0, n_periods) # Investment
ER  = rep(0.95, n_periods) # Employment rate

# Parameters
alpha = 0.3  # Growth elasticity for capital/Capital share of output
delta = 0.03 # Depreciation rate
n = 0.01     # Labor supply growth rate
s = 0.2      # Savings rate
g = 0.005     # Technological growth

# Initial positions
I[1] = 0.2 
Y[1] = 1
K[1] = 1
L_F[1] = 1
L[1] = L_F[1]*ER[1]
A[1] = 1

# Dynamic of the model
for(t in 2:n_periods){
  L_F[t] = L_F[t-1]*exp(n)
  L[t] = L_F[t]*ER[t]
  A[t] = A[t-1]*exp(g)
  K[t] = K[t-1] + I[t-1] - K[t-1]*delta
  Y[t] = K[t]^(alpha) * (A[t] * L[t])^(1-alpha)
  I[t] = s*Y[t]
}

par(mfrow = c(2,2))
plot(Y/L_F)
plot(K/L_F)
plot(diff(Y/L_F)/((Y/L_F)[-1]))
plot(diff(K/L_F)/((K/L_F)[-1]))


par(mfrow = c(2,2))
plot(Y/(A*L_F))
plot(K/(A*L_F))
plot(diff(Y/(A*L_F))/((Y/(A*L_F))[-1]))
plot(diff(K/(A*L_F))/((K/(A*L_F))[-1]))


##### Fitting model to data #####

library(rbcb)
library(ipeadatar)
library(xts)
library(zoo)

series_ipea <- available_series(language = c("br"))

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

par(mfrow = c(2,2))
plot(full_dataset$Y_data/full_dataset$L_data)
plot(full_dataset$K_data/full_dataset$L_data)
plot(diff(full_dataset$Y_data/full_dataset$L_data)/((full_dataset$Y_data/full_dataset$L_data)[-1]))
plot(diff(full_dataset$K_data/full_dataset$L_data)/((full_dataset$K_data/full_dataset$L_data)[-1]))


