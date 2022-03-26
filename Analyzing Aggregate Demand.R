# Build dataset

library(ipeadatar)
library(xts)
library(zoo)

series_ipea <- available_series(language = c("br"))

Y <- ipeadata(code = "SCN10_PIBP10", language = "br")
G <- ipeadata(code = "SCN10_CFGP10", language = "br")
C_2 <- ipeadata(code = "SCN10_CFISFLSFNP10", language = "br")
C_1 <- ipeadata(code = "SCN10_CFPPNEXCP10", language = "br")
I <- ipeadata(code = "SCN10_FBKP10", language = "br")
EXP <- ipeadata(code = "SCN10_XBSZP10", language = "br")
IMP <- ipeadata(code = "SCN10_MBSZP10", language = "br")

Y <- Y[,2:3]
G   <- G[,2:3]
C_2 <- C_2[,2:3]
C_1 <- C_1[,2:3]
I   <- I[,2:3]
EXP <- EXP[,2:3]
IMP <- IMP[,2:3]

colnames(Y)   <- c("date", "Y")
colnames(G)   <- c("date", "G")
colnames(C_2) <- c("date", "C_2")
colnames(C_1) <- c("date", "C_1")
colnames(I)   <- c("date", "I")
colnames(EXP) <- c("date", "EXP")
colnames(IMP) <- c("date", "IMP")


dataset <- merge(Y, C_1, by = "date")
dataset <- merge(dataset, C_2, by = "date")
dataset <- merge(dataset, G, by = "date")
dataset <- merge(dataset, I, by = "date")
dataset <- merge(dataset, EXP, by = "date")
dataset <- merge(dataset, IMP, by = "date")

dataset$estimated_Y <- rowSums(dataset[,3:7]) - dataset$IMP
dataset$Net_Exp <- dataset$EXP - dataset$IMP

dataset_shares <- as.data.frame(apply(dataset[,c(3:8,10)],2,function(x){x/dataset$estimated_Y}))

dataset_shares$date <- dataset$date

# Plot Shares of GDP

library(ggplot2)
library(reshape2)

df <- melt(dataset_shares[,-c(5:6)], id.vars="date")

ggplot(df, aes(x=date, y=value, fill=variable)) + 
  geom_area(alpha=0.6 , size=1, colour="black")


# Analyze growth
library(quantmod)

dataset_growth <- as.data.frame(apply(dataset[,3:10], 2, function(x){c(NA,diff(x))/dataset$estimated_Y}))

dataset_growth$date <- dataset$date
dataset_growth <- dataset_growth[-1,]

df_2 <- melt(dataset_growth[,-c(5:6)], id.vars="date")

ggplot(df_2[df_2$variable!="estimated_Y",], aes(fill=variable, y=value, x=date)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_line(data=df_2[df_2$variable=="estimated_Y",],aes(x=date,y=value))
