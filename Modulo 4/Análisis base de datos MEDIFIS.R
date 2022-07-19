library(ggplot2)
library(dplyr)
library(readxl)



# Leer base de datos ------------------------------------------------------

df <- read_excel("Modulo 4/Datos_MEDIFIS.xlsx")

# Resumen base de datos ---------------------------------------------------

str(df)

# Datos faltantes ---------------------------------------------------------

sum(is.na(df))


# Análisis descriptivo de las variables -----------------------------------

# Métricas descriptivas
library(rstatix)

df %>% select(-observacion) %>%
       get_summary_stats(type = "common")

library(pastecs)
stat.desc(df %>% select(-observacion))[14, ]

             

# Coeficiente de Asimetría
library(moments)

df %>% select(-observacion) %>%
       summarise_if(is.numeric, skewness)

# Coeficiente de Curtosis

df %>% select(-observacion) %>%
       summarise_if(is.numeric, kurtosis)

# Histogramas
ggplot(df, aes(x=est))+
  geom_histogram(fill="#702899", color="black", bins = 10) +
  theme_bw() + 
  labs(y = "Frecuencia") +
  geom_vline(aes(xintercept = mean(est)),col='red',size=2)

ggplot(df, aes(x=lrt))+
  geom_histogram(fill="#702899", color="black", bins = 10) +
  theme_bw() + 
  labs(y = "Frecuencia") +
  geom_vline(aes(xintercept = mean(lrt)),col='red',size=2)

ggplot(df, aes(x=aes))+
  geom_histogram(fill="#702899", color="black", bins = 10) +
  theme_bw() + 
  labs(y = "Frecuencia") +
  geom_vline(aes(xintercept = mean(aes)),col='red',size=2)




