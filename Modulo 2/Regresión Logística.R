# Cargar paquetes
library(ggplot2)
library(dplyr)

# Creemos la base de datos
paciente <- 1:10
peso <- c(100, 73, 90, 90, 120, 128, 65, 58, 78, 84)
es_obeso <- c("Si", "No", "No", "Si", "Si", "Si", "No", "No", "No", "No")


df <- data.frame(paciente = paciente,
                 peso = peso,
                 es_obeso = es_obeso)

# grafico de dispersion poblacion vs ventas
ggplot(df, aes(x = peso, y = es_obeso)) +
  geom_point(size = 5) +
  labs(x = "Peso del paciente en Kg", y = "Es obeso")

# grafico con regresion logistica ajustada
df <- df %>% mutate(es_obeso = ifelse(es_obeso == "Si", 1, 0))

ggplot(df, aes(x = peso, y = es_obeso)) +
  geom_point(size = 5) +
  labs(x = "Peso del paciente en Kg", y = "Es obeso") +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE)
