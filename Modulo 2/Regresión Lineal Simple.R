# Cargar paquetes
library(ggplot2)
library(dplyr)

# Creemos la base de datos
restaurante <- 1:10
poblacion <- c(2000, 6000, 8000, 8000, 12000, 16000, 20000, 
               20000, 22000, 26000)
ventas <- c(58000000, 105000000, 88000000, 118000000, 117000000,
            137000000, 157000000, 169000000, 149000000, 202000000)


df <- data.frame(restaurante = restaurante,
                 poblacion = poblacion,
                 ventas = ventas)

# grafico de dispersion poblacion vs ventas
ggplot(df, aes(x = poblacion, y = ventas)) +
  geom_point(size = 5) +
  labs(x = "Población Universidad", y = "Ventas Mensuales") +
  scale_x_continuous(breaks = seq(0, 26000, 5000)) +
  scale_y_continuous(breaks = seq(0, 202000000, 20000000))


# Ajustemos un modelo de regresión lineal a los datos usando la funcion lm
modelo_rls <- lm(ventas ~ poblacion, data = df)
modelo_rls$coefficients

# grafico de dispersion con recta de regresión estimada
ggplot(df, aes(x = poblacion, y = ventas)) +
  geom_point() +
  labs(x = "Población Universidad", y = "Ventas Mensuales") +
  scale_x_continuous(breaks = seq(0, 26000, 5000)) +
  scale_y_continuous(breaks = seq(0, 202000000, 20000000)) + 
  stat_smooth(method = "lm", se = FALSE, col = "red")

# grafico de dispersion con recta de regresión estimada con promedio de y
ggplot(df, aes(x = poblacion, y = ventas)) +
  geom_point() +
  labs(x = "Población Universidad", y = "Ventas Mensuales") +
  scale_x_continuous(breaks = seq(0, 26000, 5000)) +
  scale_y_continuous(breaks = seq(0, 202000000, 20000000)) + 
  stat_smooth(method = "lm", se = FALSE, col = "red") +
  geom_hline(yintercept=130000000, col = "blue")


# valor de coeficiente de determinación
r2 <- summary(modelo_rls)$r.squared
r2

# calcular coeficiente de correlación
coef_correlacio <- sqrt(r2)
coef_correlacio


# Prueba de hipotesis
summary(modelo_rls)

# Estimaciones con el modelo

# Estimacion puntual
predict(modelo_rls, data.frame(poblacion = 10000))

# Estimacion por intervalo
predict(modelo_rls, data.frame(poblacion = 10000), interval = "confidence")
