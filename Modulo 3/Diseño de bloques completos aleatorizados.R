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
