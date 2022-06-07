# Cargar paquetes
library(ggplot2)
library(dplyr)
library(readr)

# Creemos la base de datos
vendedores <- 1:15
antiguedad <- c(168, 106, 76, 10, 22, 12, 85, 111, 40, 51, 9, 12, 6, 56, 19)
bicicletas <- c(250, 296, 317, 300, 162, 150, 296, 308, 189, 235, 83, 112, 67, 
                280, 189)


df <- data.frame(vendedores = vendedores,
                 antiguedad = antiguedad,
                 bicicletas = bicicletas)

# grafico de dispersion poblacion vs ventas
ggplot(df, aes(x = antiguedad, y = bicicletas)) +
  geom_point(size = 3) +
  labs(x = "Antigüedad del vendedor (meses)", y = "Bicicletas vendidas")

# Ajustemos un modelo de regresión lineal a los datos usando la funcion lm
modelo_rls <- lm(bicicletas ~ antiguedad, data = df)
summary(modelo_rls)

# grafico de dispersion poblacion vs ventas
ggplot(df, aes(x = antiguedad, y = bicicletas)) +
  geom_point(size = 3) +
  labs(x = "Antigüedad del vendedor (meses)", y = "Bicicletas vendidas") +
  stat_smooth(method = "lm", se = FALSE, col = "red")


# Ajustemos un modelo de regresión polinomico de grado 2 a los datos usando la funcion lm
modelo_pol <- lm(bicicletas ~ antiguedad + I(antiguedad^2), data = df)
summary(modelo_pol)

# grafico de dispersion 
ggplot(df, aes(x = antiguedad, y = bicicletas)) +
  geom_point(size = 3) +
  labs(x = "Antigüedad del vendedor (meses)", y = "Bicicletas vendidas") +
  stat_smooth(method = "lm", se = FALSE, col = "red", formula = y ~ poly(x, 2, raw = TRUE))



# Regresion exponencial
ebola <- read_delim("Modulo 2/ebola.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
head(df)

# grafica del número de casos en el tiempo
ggplot(ebola) + 
  geom_point(aes(x=Day, y = totalSuspectCases)) +
  labs(x = "Día", y = "Total de casos de Ebola")


# grafica del número de casos en el tiempo: tranformación de variable Y
ggplot(ebola) + 
  geom_point(aes(x=Day, y = log(totalSuspectCases))) +
  labs(x = "Día", y = "log(Total de casos de Ebola)") 


# Ajustemos modelo de regresión lineal
lm_ebola = lm(log(totalSuspectCases) ~ Day, data=ebola)
summary(lm_ebola)

ggplot(ebola) + 
  geom_point(aes(x=Day, y = log(totalSuspectCases))) + 
  geom_abline(intercept = 4.54, slope = 0.0216, color='red') + 
  labs(x = "Día", y = "log(Total de casos de Ebola)")
