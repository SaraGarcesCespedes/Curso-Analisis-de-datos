library(ggplot2)
library(dplyr)
library(zoo)
library(fpp2)
library(forecast)
library(smooth)



# Serie de tiempo pasajeros -----------------------------------------------

data(AirPassengers)
AP <- AirPassengers
AP

# clase del objeto
class(AP)
start(AP)
end(AP)
frequency(AP)

# graficar serie de tiempo
plot(AP)

# Descomposión de la serie de tiempo
# aditiva
desc_aditiva <- decompose(AP, type = "additive")
plot(desc_aditiva)

# multiplicativa
desc_mult <- decompose(AP, type = "multiplicative")
plot(desc_mult)

## Grafico de la serie de tiempo con su tendencia
library(fpp2)
autoplot(AP, series="Serie tiempo") + 
  autolayer(trendcycle(desc_mult), series="Tendencia") +
  labs(title = "Serie tiempo Pasajeros",      
       x = "Tiempo",
       y = "Pasajeros"
  ) + 
  theme_bw()

## Grafico de estacionalidad
ggseasonplot(AP)




# PRONOSTICOS -------------------------------------------------------------

# Usar meanf() para pronosticar el número de pasajeros para el siguiente año
pronostico1 <- meanf(AP, h = 12)

# Graficar pronostico
autoplot(pronostico1, xlab = "Mes", ylab = "Pasajeros")


# Usar naive() para pronosticar el número de pasajeros para el siguiente año
pronostico2 <- naive(AP, h = 12)

# Graficar pronostico
autoplot(pronostico2, xlab = "Mes", ylab = "Pasajeros")


# Usar snaive() para pronosticar el número de pasajeros para el siguiente año
pronostico3 <- snaive(AP, h = 12)

# Graficar pronostico
autoplot(pronostico3, xlab = "Mes", ylab = "Pasajeros")


# Usar sma() para pronosticar el número de pasajeros para el siguiente año
prom_moviles <- sma(AP, order = 6, h = 12)

# Pronosticar
pronostico4 <- forecast(prom_moviles)
print(pronostico4)
plot(pronostico4)

# Usar ses() para pronosticar el número de pasajeros para el siguiente año
pronostico5 <- ses(AP, h=12, alpha=0.2)
autoplot(pronostico5, xlab = "Mes", ylab = "Pasajeros")

# Usar HoltWinters() para pronosticar el número de pasajeros para el siguiente año
hw <- HoltWinters(AP, seasonal = "mult")
plot(hw)

pronostico6 <- predict(hw, n.ahead=12)
ts.plot(AP, pronostico6, lty=1:2)


# Evaluación de los métodos de pronóstico ---------------------------------
library(TSstudio)

# División de base de datos en train y test
split_AP <- ts_split(ts.obj = AP, sample.out = 12)
AP_train <- split_AP$train
AP_test <- split_AP$test

# Cantidad de periodos en base de datos
length(AP)
length(AP_train)
length(AP_test)


# Implemetar métodos con base de datos de entrenamiento
pronostico1_train <- meanf(AP_train, h = 12)
pronostico2_train <- naive(AP_train, h = 12)
pronostico3_train <- snaive(AP_train, h = 12)
prom_moviles_train <- sma(AP_train, order = 6, h = 12)
pronostico4_train <- forecast(prom_moviles_train)
pronostico5_train <- ses(AP_train, h = 12, alpha = 0.2)
hw_train <- HoltWinters(AP_train, seasonal = "mult")
pronostico6_train <- predict(hw_train, n.ahead=12)


# Indicadores de precisión de pronósticos
library(Metrics)

results <- as.data.frame(AP_test)
results$pronostico1 <- round(pronostico1_train$model$mu, 0)
results$pronostico2 <- round(pronostico2_train$model$future, 0)
results$pronostico3 <- round(pronostico3_train$model$future, 0)
results$pronostico4 <- round(pronostico4_train$model$forecast, 0)
results$pronostico5 <- round(pronostico5_train$mean, 0)
results$pronostico6 <- round(as.numeric(pronostico6_train), 0)

# RMSE
rmse(results$x, results$pronostico1)
rmse(results$x, results$pronostico2)
rmse(results$x, results$pronostico3)
rmse(results$x, results$pronostico4)
rmse(results$x, results$pronostico5)
rmse(results$x, results$pronostico6)



# Crear objetos de clase ts -----------------------------------------------
datos <- sample(1:50, 10)
ts(datos, frequency = 4, start = c(1959, 2)) # frequency 4 => Quarterly Data
ts(1:10, frequency = 12, start = 1990) # freq 12 => Monthly data. 
ts(datos, start=c(2009), end=c(2014), frequency=1) # Yearly Data
ts(datos, frequency=365, start=c(2017, 152)) # Daily data


# Modelos de series de tiempo ---------------------------------------------

# Modelo AR
AR <- arima(AP_train, order = c(1,0,0)) #order: (p,d,q)
print(AR)

# Prediccion con modelo AR
predict(AR)
predict_AR$pred

# Prediccion de los proximos 12 meses
predict_AR <- predict(AR, n.ahead = 12)$pred

# Grafico predicciones
ts.plot(AP_train, xlim = c(1949, 1961))
points(predict_AR, type = "l", col = 2)

# Modelo MA
MA <- arima(AP_train, order = c(0,0,1)) #order: (p,d,q)
print(MA)

# Prediccion con modelo MA
predict(MA)
predict_MA$pred

# Prediccion de los proximos 12 meses
predict_MA <- predict(MA, n.ahead = 12)$pred

# Grafico predicciones
ts.plot(AP_train, xlim = c(1949, 1961))
points(predict_MA, type = "l", col = 2)

# Modelo ARIMA
ARMA <- arima(AP_train, order = c(1,0,1)) #order: (p,d,q)
print(ARMA)

# Prediccion con modelo AR
predict(ARMA)
predict_ARMA$pred

# Prediccion de los proximos 12 meses
predict_ARMA <- predict(ARMA, n.ahead = 12)$pred

# Grafico predicciones
ts.plot(AP_train, xlim = c(1949, 1961))
points(predict_ARMA, type = "l", col = 2)

# Evaluacion precision de pronosticos
results <- as.data.frame(AP_test)
results$ar <- round(as.numeric(predict_AR), 0)
results$ma <- round(as.numeric(predict_MA), 0)
results$arma <- round(as.numeric(predict_ARMA), 0)

# RMSE
rmse(results$x, results$ar)
rmse(results$x, results$ma)
rmse(results$x, results$arma)
