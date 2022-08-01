library(ggplot2)
library(dplyr)
library(readxl)


# Valores y vectores propios ----------------------------------------------

M <- matrix(c(1,-1,0,3,2,1,-2,3,4), ncol = 3)
M

cov_matrix <- cov(M)
cov_matrix

e <- eigen(cov_matrix)
e$values
e$vectors

eigen(cov_matrix)

# PRIMER COMPONENTE PRINCIPAL
# CP1 = -0.22*X1 -0.27*X2 + 0.93*X3
# CP2 = 0.85*X1 - 0.51*X2 + 0.06*X3
# CP3 = 0.46*X1 + 0.81*X2 + 0.34*X3

# Componentes principales -------------------------------------------------

df_original <- read_excel("Modulo 4/Datos_MEDIFIS.xlsx")
df <- df_original %>% select(-observacion, -sexo)

# Gráficos de dispersión
ggplot(df, aes(est, pes)) +
  geom_point()

# calculate correlation matrix and round to 1 decimal place:
corr_matrix <- df %>% 
  cor() %>% 
  round(1)

library(ggcorrplot)
ggcorrplot(corr_matrix, type = "lower", lab = T, show.legend = F)

# Calcular componentes principales
df.pca <- prcomp(df, scale. = TRUE, center = TRUE)

# Carga (grado de importancia) de cada variable con la respectiva CP
df.pca$rotation

# Porcentaje de variabilidad explicada por cada componente
summary(df.pca)

# valores propios
df.pca$sdev^2

# Gráfica del porcentaje de variabilidad explicada por cada CP
library(factoextra)
fviz_eig(df.pca, addlabels = TRUE)

# Puntaje de cada observación sobre los CP
df.pca$x

# Nueva base de datos con 2 componentes principales
df_new <- df.pca$x[, c(1, 2)]
df_new

# Gráfico de contribuciones
fviz_contrib(df.pca, 
             choice = "var", 
             axes = 1)
fviz_contrib(df.pca, 
             choice = "var", 
             axes = 2)

# Gráfico de individuos
fviz_pca_ind(df.pca,
             repel = TRUE # Avoid text overlapping
             )

fviz_pca_ind(df.pca,
             col.ind = as.factor(df_original$sexo),
             repel = TRUE,
             legend.title = "Sexo"
)

# Círculo de correlaciones
fviz_pca_var(df.pca,
             repel = TRUE)



