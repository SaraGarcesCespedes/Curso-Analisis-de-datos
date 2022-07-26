library(ggplot2)
library(dplyr)
library(readxl)

# Leer base de datos ------------------------------------------------------

df <- read_excel("Modulo 4/Datos_MEDIFIS.xlsx")
df <- df %>% select(pes, est)

# Grafico de dispersión
ggplot(df, aes(est, pes)) +
  geom_point()
