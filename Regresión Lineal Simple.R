# Creemos la base de datos

restaurante <- 1:10
poblacion <- c(2000, 6000, 8000, 8000, 12000, 16000, 20000, 
               20000, 22000, 26000)
ventas <- c(58000000, 105000000, 88000000, 118000000, 117000000,
            137000000, 157000000, 169000000, 149000000, 202000000)


df <- data.frame(restaurante = restaurante,
                 poblacion = poblacion,
                 ventas = ventas)

df
