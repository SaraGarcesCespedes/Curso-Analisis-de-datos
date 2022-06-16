library(ggplot2)

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
library(agricolae)
pruebas_multiples <- LSD.test(anova, "metodo")
pruebas_multiples

# Gráfico de medias
plot(pruebas_multiples)
