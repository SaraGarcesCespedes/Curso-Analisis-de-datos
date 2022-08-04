library(ggplot2)

# Datos serie de tiempo
semana <- 1:12
ventas <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)
df <- data.frame(semana = semana,
                 ventas = ventas)

# Graficar serie de tiempo
ggplot(df, aes(x = semana, y = ventas)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous(breaks = seq(0,25,1)) +
  scale_x_continuous(breaks = seq(0,12,1)) 
  
  
