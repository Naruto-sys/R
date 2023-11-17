install.packages("data_ts.table")
library("data_ts.table")
library(openxlsx)
library(forecast)
library(tseries)
library(ggplot2)
library(forecastHybrid)


data_ts <- read.xlsx("data.xlsx", "Monthly Prices")
orange = data_ts[c("X41")][-c(1:6), ]
orange_num <- as.numeric(ts(orange, frequency = 10))
orange <- ts(orange, frequency = 10)
autoplot(orange) + theme_bw() + ggtitle("Orange history") + labs(x = "Time", y = "Orange price ($/kg)") 

nIter <- 20
errVec_RW <- c()
errVec_autoARIMA <- c()
errVec_ARIMA_manual <- c()
errVec_ETSmodel <- c()
errVec_hybridM <- c()

for (i in 1:nIter){
  train <- orange_num[1:(length(orange) - i)]
  test <- orange_num[(length(orange) - i + 1)]
  
  rw_f <- naive(train, h = 1)$mean
  
  autoARIMA_model <- auto.arima(train, allowdrift = F)
  autoARIMA_f <- forecast(autoARIMA_model, h = 1)$mean

  ARIMA_manual_model <- arima(train,  order = c(2,1,1),
                        seasonal = list(order = c(1, 0, 0)))
  ARIMA_manual_f <- forecast(ARIMA_manual_model, h = 1)$mean
  
  ETS_model <- ets(train, model = "ZZZ", damped = T)
  ETS_f <- forecast(ETS_model, h = 1)$mean
  
  hybrid_model <- hybridModel(train, models = "aetz")
  hybrid_f <- forecast(hybrid_model, h = 1)$mean
  
  errVec_RW[i] <- test - rw_f
  errVec_autoARIMA[i] <- test - autoARIMA_f
  errVec_ARIMA_manual[i] <- test - ARIMA_manual_f
  errVec_ETSmodel[i] <- test - ETS_f
  errVec_hybridM[i] <- test - hybrid_f
}

# MAE-s
maes = c(
  mean(abs(errVec_RW)),
  mean(abs(errVec_autoARIMA)),
  mean(abs(errVec_ARIMA_manual)),
  mean(abs(errVec_ETSmodel)),
  mean(abs(errVec_hybridM))
)

# RMSE-s
rmses = c(
  sqrt(mean(errVec_RW^2)),
  sqrt(mean(errVec_autoARIMA^2)),
  sqrt(mean(errVec_ARIMA_manual^2)),
  sqrt(mean(errVec_ETSmodel^2)),
  sqrt(mean(errVec_hybridM^2))
)

ErrorMatrix = data.table(
  Model = c("RW", "Arima(Auto)", "Arima(Manual)", "ETS", "Hybrid"),
  MAE = maes,
  RMSE = rmses
)

EM = data.table(
    Model = c("RW"),
    TestNum = 1:20,
    Error = errVec_RW,
    Model = c("Auto_Arima"),
    TestNum = 1:20,
    Error = errVec_autoARIMA,
    Model = c("Manual_Arima"),
    TestNum = 1:20,
    Error = errVec_ARIMA_manual,
    Model = c("ETS"),
    TestNum = 1:20,
    Error = errVec_ETSmodel,
    Model = c("Hybrid"),
    TestNum = 1:20,
    Error = errVec_hybridM
)


