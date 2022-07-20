library(ggplot2)
library(dplyr)
library(readxl)



# Leer base de datos ------------------------------------------------------

df <- read_excel("Modulo 4/Datos_MEDIFIS.xlsx")
df <- df %>% select(-observacion)
# Resumen base de datos ---------------------------------------------------

str(df)

# Datos faltantes ---------------------------------------------------------

sum(is.na(df))


# Análisis descriptivo de las variables -----------------------------------

# Métricas descriptivas
library(rstatix)

df %>% get_summary_stats(type = "common")

library(pastecs)
stat.desc(df)[14, ]

             

# Coeficiente de Asimetría
library(moments)

df %>% summarise_if(is.numeric, skewness)

# Coeficiente de Curtosis

df %>% summarise_if(is.numeric, kurtosis)

# Calcular la varianza de una variable
var(df$pes)

# Calcular la covarianza entre dos variables
cov(df$est, df$pes)

# Matriz de varianzas y covarianzas
matriz_var_cov <- cov(df %>% select(-sexo))

# Variabilidad total
matriz_var_cov <- cov(df %>% select(-sexo, -pes))

diag(matriz_var_cov)
sum(diag(matriz_var_cov))

# Variabilidad promedio
mean(diag(matriz_var_cov))

# Varianza generalizada (Peña y Rodriguez)
det(matriz_var_cov)

# Desviacion típica generalizada (Peña y Rodriguez)
det(matriz_var_cov)^(1/2)

# Variabilidad promedio (Peña y Rodriguez)
det(matriz_var_cov)^(1/6)

# Desviación promedio (Peña y Rodriguez)
(det(matriz_var_cov)^(1/6))^(1/2)

# Correlación entre dos variables
cor(df$est, df$lpie)

# Matriz de correlación
cor(df %>% select(-sexo))
