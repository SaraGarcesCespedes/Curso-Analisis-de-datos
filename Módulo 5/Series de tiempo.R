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

# grafica

# Implemetar métodos con base de datos de entrenamiento
pronostico1_train <- meanf(AP_train, h = 12)
pronostico2_train <- naive(AP_train, h = 12)
pronostico3_train <- snaive(AP_train, h = 12)
prom_moviles_train <- sma(AP_train, order = 6, h = 12)
pronostico4_train <- forecast(prom_moviles_train)

# Indicadores de precisión de pronósticos
library(Metrics)

results <- as.data.frame(AP_test)
results$pronostico1 <- round(pronostico1_train$model$mu, 0)
results$pronostico2 <- round(pronostico2_train$model$mu, 0)
results$pronostico3 <- round(pronostico3_train$model$mu, 0)
results$pronostico4 <- round(pronostico4_train$model$forecast, 0)

# RMSE
rmse(results$x, results$pronostico1)
rmse(results$x, results$pronostico2)
rmse(results$x, results$pronostico3)
rmse(results$x, results$pronostico4)
