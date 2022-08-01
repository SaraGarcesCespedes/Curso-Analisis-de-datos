library(readxl)
library(factoextra)
library(cluster)

# EJEMPLO BASE DE DATOS clientes -----------------------------------------------      # Loading the data set
df <- read.csv("Modulo 4/Mall_Customers.csv", sep = ";")
df <- df %>% select(-CustomerID)

# matriz de correlaciones
corr_matrix <- cor(df)
corr_matrix

# Grafico de dispersion
ggplot(df, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point()

# Algoritmo k-medias
df <- scale(df)
k2 <- kmeans(df, centers = 2, nstart = 25, iter.max = 10)
str(k2)
k2

# Graficar clusters
fviz_cluster(k2, data = df)

# Variar el número de clusters
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# metodo codo: seleccion del numero de clusters
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

# Cluster final
set.seed(123)
final <- kmeans(df, 5, nstart = 25)
print(final)
fviz_cluster(final, data = df)


# EJEMPLO BASE DE DATOS ESTUDIANTES ---------------------------------------

# Leer base de datos 
df_original <- read_excel("Modulo 4/Datos_MEDIFIS.xlsx")
df <- df_original %>% select(-observacion, -sexo)
df <- scale(df)

# Algoritmo k-medias
k2 <- kmeans(df, centers = 2, nstart = 25, iter.max = 10)
str(k2)
k2

# Graficar clusters
fviz_cluster(k2, data = df)

# Variar el número de clusters
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# metodo codo: seleccion del numero de clusters
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")


