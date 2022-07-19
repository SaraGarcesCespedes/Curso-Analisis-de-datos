library(ggplot2)
library(agricolae)
library(dplyr)

# creamos base de datos
metodo <- c(rep("A", 4), rep("B", 4), rep("C", 4), rep("D", 4))
operador <- rep(1:4, 4)
tiempo <- c(6, 9, 7, 8, 7, 10, 11, 8, 10, 16, 11, 14, 10, 13, 11, 9)
df <- data.frame(metodo = metodo,
                 operador = as.character(operador),
                 tiempo = tiempo)
df

# Analisis de varianza ANOVA
anova <- aov(tiempo ~ operador + metodo, data = df)
summary(anova)

# Diagramas de caja
ggplot(data = df, aes(x = tiempo, y = metodo, fill = metodo)) +
  geom_boxplot() + 
  theme_bw() +
  labs(y = "Método de ensamble",
       x = "Tiempo de ensamble") + 
  theme(legend.position="none")

# Pruebas múltiples
pruebas_multiples <- LSD.test(anova, "metodo")
pruebas_multiples

# Verificacion de supuestos
# VERIFICACION DE SUPUESTOS

# calcular los residuos
residuos <- residuals(anova)
residuos

# promedio de moscas muertas por tratamiento
prom_tratamientos <- df %>% group_by(marca) %>%
  summarise(prom_moscas = mean(numero_moscas))
prom_tratamientos

df$residuos <- residuos
df <- df %>% left_join(prom_tratamientos, by = "marca")

# histograma de residuos
ggplot(df, aes(x=residuos))+
  geom_histogram(breaks = seq(-12, 12, 3), fill="#702899", color="black", bins = 10) +
  labs(y = "Frecuencia") + 
  scale_x_continuous(breaks = seq(-12, 12, 3))

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
ggplot(data = df, aes(x = prom_moscas, y = residuos)) +
  geom_point() + 
  theme_bw() +
  labs(y = "Residuos",
       x = "Valores estimados") +
  geom_abline(intercept = 0, slope = 0, color='red') 


# prueba de Bartlett
bartlett.test(numero_moscas ~ marca, data = df)


# supuesto de independencia
ggplot(data = df, aes(x = orden_pruebas, y = residuos)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs(y = "Residuos",
       x = "Orden corridas experimentales") +
  geom_abline(intercept = 0, slope = 0, color='red') 

# prueba Durbin-Watson
durbinWatsonTest(anova) 


# otra forma de construir las gráficas
plot(anova)
