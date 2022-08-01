library(ggplot2)
library(dplyr)
library(readxl)



# Leer base de datos ------------------------------------------------------

df_original <- read_excel("Modulo 4/Datos_MEDIFIS.xlsx")
df <- df_original %>% select(-observacion)
# Resumen base de datos ---------------------------------------------------

str(df)

# Datos faltantes ---------------------------------------------------------

sum(is.na(df))


# Análisis descriptivo de las variables -----------------------------------

df_new <- df %>% select(-sexo)

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


# Análisis multivariante --------------------------------------------------


# Calcular la covarianza entre dos variables
cov(df$est, df$pes)

# Matriz de varianzas y covarianzas
matriz_var_cov <- cov(df_new)

# Variabilidad total
matriz_var_cov_new <- cov(df_new)

diag(matriz_var_cov_new)
sum(diag(matriz_var_cov_new))

# Varianza generalizada 
det(matriz_var_cov_new)

# Correlación entre dos variables
cor(df_new$est, df_new$lpie)

# Matriz de correlación
cor(df_new)

# Graficar la matriz de correlaciones
library(ggcorrplot)

# calculate correlation matrix and round to 1 decimal place:
corr_matrix <- df_new %>% 
               cor() %>% 
               round(1)

ggcorrplot(corr_matrix, type = "lower", lab = T, show.legend = F)

# GRAFICOS ----------------------------------------------------------------

# grafico de puntos
estatura <- df_new %>% group_by(est) %>%
                       summarise(frecuencia = n())
ggplot(estatura, aes(x = est, y = frecuencia)) +
  geom_point()

# grafico de dispersion simple
ggplot(df_new, aes(est, pes)) +
  geom_point()

# grafico de dispersion con marginales
library(ggExtra)
p <- ggplot(df_new, aes(est, pes)) +
  geom_point()
ggExtra::ggMarginal(p, type = "boxplot")

# Diagrama de dispersión tridimensional
library(plotly)
plot_ly(x=df_new$est, y=df_new$pes, z=df_new$lpie, 
        type="scatter3d", mode="markers")


# Matriz de dispersión
pairs(df_new)

# Matriz de dispersión con histogramas
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}
pairs(df_new, diag.panel= panel.hist)

# Gráfico de estrellas 
# Estandarización de las variables
df_new_std <- scale(df_new)
stars(df_new_std, labels = df_original$observacion, key.loc=c(10,1.8))

# Gráfico de caras
library(aplpack)
faces(df_new_std, labels = df_original$observacion)


# Concepto de distancia ---------------------------------------------------

# Calcular distancia euclidiana
dist(df_new, method = 'euclidean')

# Calcular la distancia euclidiana ponderada
M <- diag(diag(matriz_var_cov))

dist_ponderada <- function(individual1, individual2, M) {
  dif <- as.numeric(df_new[individual1, ]) - as.numeric(df_new[individual2, ])
  distancia <- t(dif) %*% solve(M) %*% dif
  return(sqrt(distancia))
}

dist_ponderada(1, 2, M)

# Calcular distancia de Mahalanobis con respecto a la media
mahalanobis(df_new, colMeans(df_new), cov(df_new))

# Calcular la distancia de Mahalanobis entre cada individuo
library(distances)
distances(as.matrix(df_new), normalize = "mahalanobize")

# grafica de las distancias entre individuos
library(factoextra)
distance <- get_dist(df_new)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
