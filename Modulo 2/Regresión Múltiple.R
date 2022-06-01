# Cargar paquetes
library(ggplot2)
library(dplyr)
library(readxl)
library(GGally)

# Leer base de datos
df <- read_excel("datos_antioquia.xlsx")


# grafico de dispersion poblacion vs ventas
ggplot(df, aes(x = Poblacion, y = Homicidios)) +
  geom_point(size = 3) +
  labs(x = "Población", y = "Número de homicidios",
       title = "Población versus Número de homicidios por municipio") 



# Ajustemos un modelo de regresión lineal a los datos usando la funcion lm
modelo_rls <- lm(Homicidios ~ Poblacion, data = df)

# Resumen del modelo
summary(modelo_rls)

# Gráfica de ecuación de regresión estimada
ggplot(df, aes(x = Poblacion, y = Homicidios)) +
  geom_point(size = 3) +
  labs(x = "Población", y = "Número de homicidios",
       title = "Población versus Número de homicidios por municipio") +
  stat_smooth(method = "lm", se = FALSE, col = "red")

# Predicciones con el modelo
predict(modelo_rls, data.frame(Poblacion = 50836))

# REGRESION LINEAL MULTIPLE-------------------------------------------------------------------------------------------------------------

 
# Gráfica de correlaciones entre las variables numéricas de la base de datos
ggpairs(df %>% select(-Region, -Municipio), lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# Ajustemos un modelo de regresión múltiple a los datos usando la funcion lm
modelo_rm <- lm(Homicidios ~  Poblacion + Desertores_escolares, data = df)

# Resumen modelo
summary(modelo_rm)

# Estimaciones con el modelo

# Estimacion puntual
predict(modelo_rm, data.frame(Poblacion = 30000, Desertores_escolares = 450))

# Estimacion por intervalo
predict(modelo_rm, data.frame(Poblacion = 30000, Desertores_escolares = 450), interval = "confidence")

# Aumentemos el número de variables

# Ajustemos un modelo de regresión a los datos usando la funcion lm
modelo_desertores <- lm(Homicidios ~ Desertores_escolares, data = df)

# Resumen modelo
summary(modelo_desertores)

# Agreguemos una variable cualitativa
modelo_desertores <- lm(Homicidios ~ Desertores_escolares + Region, data = df)

# Resumen modelo
summary(modelo_desertores)

df$Region <- as.factor(df$Region)
df$Region <- relevel(df$Region, ref = "Uraba")
modelo <- lm(Homicidios ~ Desertores_escolares + Region, data = df) 
summary(modelo)

