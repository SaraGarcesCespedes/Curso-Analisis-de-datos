# Cargar paquetes
library(ggplot2)
library(dplyr)

# Leer base de datos
df <- read_excel("C:/Users/Imagemaker_PC/Documents/Personal/UDEA/datos_antioquia.xlsx")


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
# grafiquemos cada el tiempo 
library(GGally)
ggpairs(df %>% select(-Region, -Municipio), lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# Ajustemos un modelo de regresión múltiple a los datos usando la funcion lm
modelo_rm <- lm(Homicidios ~  Poblacion + Desertores_escolares, data = df)

# Coeficientes del modelo
modelo_rm$coefficients

# valor de coeficiente de determinación
r2 <- summary(modelo_rm)$r.squared
r2


# Ajustemos un modelo de regresión múltiple a los datos usando la funcion lm
modelo_rm <- lm(Homicidios ~ Desertores_escolares + Poblacion + Extorsiones + Lesiones_personales + Escenarios_deportivos, data = df)

# Coeficientes del modelo
modelo_rm$coefficients

# valor de coeficiente de determinación
r2 <- summary(modelo_rm)$r.squared
r2

summary(modelo_rm)
