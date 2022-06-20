library(ggplot2)
library(agricolae)
library(dplyr)
library(EnvStats)
library(car)

# EXPERIMENTO METODOS DE ENSAMBLE
# orden aleatorio de las corridas experimentales
z <- c("A", "B", "C", "D")
sample(
  rep(z, 
      times = 4  # REPLICAR EL VECTOR Z 4 VECES
  ),
  size = 16,  # NUMERO DE ELEMENTOS QUE SE ESCOGEN
)  

# ANOVA EN R

# creamos base de datos
metodo <- c(rep("A", 4), rep("B", 4), rep("C", 4), rep("D", 4))
tiempo <- c(6, 8, 7, 8, 7, 9, 10, 8, 11, 16, 11, 13, 10, 12, 11, 9)
df <- data.frame(metodo = metodo,
                 tiempo = tiempo)
df

# Analisis de varianza ANOVA
anova <- aov(tiempo ~ metodo, data = df)
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

# Gráfico de medias
plot(pruebas_multiples)



# EJEMPLO VENENO DE MOSCAS

# creamos base de datos
marca <- c(rep("1", 6), rep("2", 6), rep("3", 6))
numero_moscas <- c(72, 65, 77, 75, 62, 73, 55, 59, 68, 70, 53, 50, 
                   64, 74, 61, 58, 51, 69)
orden_pruebas <- sample(1:18, size = 18, replace = FALSE)
df <- data.frame(marca = marca,
                 numero_moscas = numero_moscas,
                 orden_pruebas = orden_pruebas)
df


# Analisis de varianza ANOVA
anova <- aov(numero_moscas ~ marca, data = df)
summary(anova)

# Diagramas de caja
ggplot(data = df, aes(x = numero_moscas, y = marca, fill = marca)) +
  geom_boxplot() + 
  theme_bw() +
  labs(y = "Marca de veneno",
       x = "Número de moscas muertas") + 
  theme(legend.position="none")

# Pruebas múltiples
pruebas_multiples <- LSD.test(anova, "marca")
pruebas_multiples

# Gráfico de medias
plot(pruebas_multiples)

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
