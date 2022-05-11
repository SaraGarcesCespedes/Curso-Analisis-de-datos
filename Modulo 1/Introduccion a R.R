

# Objetos en R ------------------------------------------------------------

# Tipos de objetos en R: vectores, matrices, listas, data frames, factores, arrays


# Vectores ----------------------------------------------------------------

# R tiene 5 tipos de vectores: numeric, character, integer, complex, logical

v1 <- 1
class(v1)

v2 <- "Hola"
class(v2)

v3 <- c("a", "b", "c")
class(v3)

v4 <- c(0.5, 0.6)
class(v4)

v5 <- c(10L, 11L, 15L) # Integer numbers without the L will be saved as doubles
class(v5)

v6 <- c(0.5 +4i, 0.5-4i)
class(v6)

v7 <- c(TRUE, FALSE)
class(v7)
length(v7)
is.vector(v7)

v8 <- c(0.5, "a")
class(v8)

v9 <- c(TRUE, 2)
class(v9)

# Operaciones sobre vectores
x <- 0:6
x
class(x)
sum(x)

x <- c("a", "b", "c")
x
class(x)
sum(x)


# Matrices ----------------------------------------------------------------

m1 <- matrix(nrow=2, ncol=3)
m1
class(m1)
dim(m1)

m2 <- matrix(1:6, nrow=2, ncol=3)
m2
class(m2)
dim(m2)

x <- c(1,3,6)
y <- c(10,11,12)
m3 <- rbind(x,y)
m3

m4 <- cbind(x,y)
m4

# Arrays ------------------------------------------------------------------

ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
ar


# Factores ----------------------------------------------------------------

# forma en la que R guarda información categorica, la cual puede tener un orden
genero <- c("male", "female", "female", "female", "male")
genero
genero <- factor(genero)
genero

class(genero)
unclass(genero)
table(genero)

genero2 <- factor(c("male", "female", "female", "female", "male"),
                 levels = c("male", "female"))
genero2
unclass(genero2)
as.character(genero2)


# Atributos de los objetos ------------------------------------------------

# Son una especie de "metadata" para los objetos, que permiten
# obtener descripciones de los objetos: names, dimnames, 
# dimensions, class, length, ...

# Names
x <- 1:3
attributes(x)
names(x) <- c("Medellín", "Cali", "Barranquilla")
names(x)
x
attributes(x)

# dim
attributes(m1)


# Transformar la clase de un objeto ---------------------------------------

as.numeric(FALSE)
as.logical(1)
as.character(1)



# Listas ------------------------------------------------------------------

lista1 <- list(1, "a", "TRUE", 1+4i)
lista1
class(lista1)

lista2 <- list(100:130, "R", list(TRUE, FALSE))
lista2     


# Data frames -------------------------------------------------------------

df <- data.frame(frec = 1:4, nombre = c("Primero","Segundo","Tercero","Cuarto"))
df
attributes(df)
nrow(df)
ncol(df)
class(df)
str(df)


# Datos faltantes ---------------------------------------------------------

x <- c(1, 5, NA, 10)
is.na(x)



# Algunas funciones base de R ---------------------------------------------

vector <- c(1, 2, 3, 4, 3.75)

mean(x = vector)
round(x = mean(vector), digits = 1)
min(x = vector)
max(x = vector)
factorial(x = 3)
lanzo_TR
sample(x = 1:4, size = 2)
sample(x = vector, size = 2, replace = TRUE)


# Escribir nuestras propias funciones -------------------------------------

# Estructura de una funcion
my_function <- function() {}

# Crear funcion para simular el lanzamiento de un dado dos veces y sumar las
# cantidades obtenidas

roll <- function() {
  valores_dado <- 1:6
  dado <- sample(valores_dado, size = 2, replace = TRUE)
  sum(dado)
}

roll()

# Argumentos de una función
roll2 <- function() {
  dado <- sample(lados, size = 2, replace = TRUE)
  sum(dado)
}
roll2()

roll2 <- function(lados) {
  dado <- sample(lados, size = 2, replace = TRUE)
  sum(dado)
}

roll2(lados = 1:4)
roll2(lados = 1:6)
roll2(1:20)

roll2 <- function(lados = 1:6) {
  dado <- sample(lados, size = 2, replace = TRUE)
  sum(dado)
}

roll2()



# Instalar paquetes -------------------------------------------------------

install.packages("ggplot2") # al instalar un paquete lo guardamos en el disco duro
library(ggplot2) # para usar el paquete debemos cargarlo en la sesión actual


# Graficas en R -----------------------------------------------------------

# Grafico de dispersion
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x

y <- x^3
y

plot(x, y)


HISTqplot(x, y)

# Histograma
x <- c(1, 2, 2, 2, 3, 3)
hist(x)
hist(x, breaks = c(0, 1, 2, 3))
qplot(x, binwidth = 1)
