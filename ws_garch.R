library(tidyverse) # обработка данных
library(fpp3) # работы с временными рядами
library(rio) # импорт данных
library(urca) # тесты на стационарность
library(ggplot2) # графики

# импорт данных
walmart <- import("Walmart.csv")
walmart <- mutate(walmart, trading_day = row_number())
walmart <- as_tsibble(walmart, index = trading_day)
# a) построим график цен акций walmart-а
autoplot(walmart, CLOSE)  + labs(x = "trading day", y = "price")

# b) протестируем ряд цен на стационарность с помощью ADF-теста
# H0: ряд нестационарен
# H1: ряд стационарен
summary(ur.df(walmart$CLOSE, type = "drift", selectlags = "AIC"))

# Так как DF_obs. = -1.2236 > DF_crit. = -2.86, то H0 не отвергается на уровне значимости 5%

# c) рассчитаем ряд лог-доходностей обыкновенных акций компании walmart и построим график
walmart$ret <- log(walmart$CLOSE) - log(dplyr::lag(walmart$CLOSE))
walmart <- walmart[-1,]
autoplot(walmart,ret) +
  labs(x = "trading day", y = "returns")

# d) проверим ряд доходностей на стационарность
# H0: ряд нестационарен
# H1: ряд стационарен
summary(ur.df(walmart$ret, type = "drift", selectlags = "AIC"))
# Так как DF_obs. = -27.6319 < DF_crit. = -2.86, то H1 не отвергается на уровне значимости 5%

# e) с помощью алгоритма Hyndman-Khandakar подберем оптимальную ARIMA модель для лог-доходности

arima_walmart <- model(walmart, auto = ARIMA(ret ~ 1 + pdq()))
report(arima_walmart$auto[[1]])

# f) сохраним остатки ARMA-модели
library(FinTS)

d <- augment(arima_walmart) %>% filter(.model == 'auto')
walmart$e <- d$.innov
summary(ur.df(walmart$e, type = "drift", selectlags = "AIC"))
# протестируем наличие GARCH-эффекта (ARCH test)
# H0: нет GARCH-эффекта
# H1: есть GARCH-эффека
ArchTest(walmart$e, lags = 2) 
# тест на наличие GARCH-эффекта в ряду доходностей
# Так как p-value = 0, гипотеза H1 не отвергается на любом разумном уровне значимости

# g) оценим стандартную GARCH-модель с нормальными ошибками
library(rugarch) # GARCH-модели
# сначала зададим спецификацию ARMA(p,q)/GARCH(s,r)-модели
# Оптимальной ARMA моделью оказалась модель 0, 1!

spec_11 <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean = TRUE),
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                     distribution.model = "norm")
sgarch_11 <- ugarchfit(spec_11, data = walmart$ret)
sgarch_11
spec_12 <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean = TRUE),
                     variance.model = list(garchOrder = c(1,2), model = "sGARCH"),
                     distribution.model = "norm")
sgarch_12 <- ugarchfit(spec_12, data = walmart$ret)
sgarch_12
spec_21 <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean = TRUE),
                     variance.model = list(garchOrder = c(2,1), model = "sGARCH"),
                     distribution.model = "norm")
sgarch_21 <- ugarchfit(spec_21, data = walmart$ret)
sgarch_21
spec_22 <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean = TRUE),
                      variance.model = list(garchOrder = c(2,2), model = "sGARCH"),
                      distribution.model = "norm")

sgarch_22 <- ugarchfit(spec_22, data = walmart$ret)
sgarch_22


# сравним по информационным критериям модели
infocriteria(sgarch_11)
infocriteria(sgarch_12)
infocriteria(sgarch_21)
infocriteria(sgarch_22)

# Лучшей согласно информационным критериям оказалась модель ARMA(0,1)/GARCH(1,1)

# h) расчет волатильности по GARCH-модели

vol <- sgarch_11@fit$sigma^2 # расчет волатильности (sigma^2)
vol_ann <- vol*(250)^(1/2)*100 # расчет волатильности в годовом выражении

plot(vol_ann, type = "l", xlab = "trading_day", ylab = "vol_ann")

# i) построим прогноз волатильности по GARCH-модели на 2 шага вперед

sigma_forec <- ugarchforecast(sgarch_11, walmart$ret, n.ahead = 2)
vol_forec = sigma_forec@forecast$sigmaFor^2
vol_forec
