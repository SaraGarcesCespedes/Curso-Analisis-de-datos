library(ggplot2)
library(agricolae)
library(dplyr)
library(car)

# creamos base de datos
diseno <- c(rep("A", 6), rep("B", 6))
lanzador <- rep(1:6, 1)
distancia <- c(6.48, 12.52, 6.28, 8.6, 9.62, 11.39, 8.91, 8.24, 6.76, 6.60, 9.63, 10.54)
orden <- c(3, 9, 2, 5, 12, 8, 4, 10, 1, 6, 11, 7)
df <- data.frame(diseno = diseno,
                 lanzador = as.character(lanzador),
                 distancia = distancia)
df

# Analisis de varianza ANOVA
anova <- aov(distancia ~ lanzador + diseno, data = df)
summary(anova)

# Diagramas de caja
ggplot(data = df, aes(x = distancia, y = diseno, fill = diseno)) +
  geom_boxplot() + 
  theme_bw() +
  labs(y = "Diseño de avión",
       x = "Distancia de vuelo (mts)") + 
  theme(legend.position="none")

# Pruebas múltiples
pruebas_multiples <- LSD.test(anova, "diseno")
pruebas_multiples

# VERIFICACION DE SUPUESTOS

# calcular los residuos
residuos <- residuals(anova)
residuos

# Supuesto de normalidad

# gráfico qqplot
EnvStats::qqPlot(residuos, #Un gráfico Cuantil-Cuantil de los residuos
                 pch =20, #Forma de los puntos
                 main="QQ-Plot de los residuos", #Título principal
                 xlab = "Cuantiles teóricos",  #Etiqueta eje x
                 ylab="Cuantiles observados de los residuos",
                 add.line = TRUE) #Etiqueta eje y

# Prueba shapiro-wilks
shapiro.test(residuos) 

# Supuesto de varianza constante

# gráfica de valores estimados versus residuos

# distancia promedio de vuelo de cada diseño
prom_distancia <- df %>% group_by(diseno) %>%
                         summarise(prom = mean(distancia))
prom_distancia

df$residuos <- residuos
df <- df %>% mutate(prom_distancia = ifelse(diseno == "A", 9.15, 8.45))

ggplot(data = df, aes(x = prom_distancia, y = residuos)) +
  geom_point() + 
  theme_bw() +
  labs(y = "Residuos",
       x = "Valores estimados") +
  geom_abline(intercept = 0, slope = 0, color='red') 


# prueba de Bartlett
bartlett.test(distancia ~ diseno, data = df)


# Supuesto de independencia
ggplot(data = df, aes(x = orden, y = residuos)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs(y = "Residuos",
       x = "Orden corridas experimentales") +
  geom_abline(intercept = 0, slope = 0, color='red') 

# prueba Durbin-Watson
durbinWatsonTest(anova) 


