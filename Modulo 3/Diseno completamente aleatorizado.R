# EXPERIMENTO METODOS DE ENSAMBLE
# orden aleatorio de las corridas experimentales
z <- c("A", "B", "C", "D")
sample(
  rep(z, 
      times = 4  # REPLICAR EL VECTOR Z 4 VECES
  ),
  size = 16,  # NUMERO DE ELEMENTOS QUE SE ESCOGEN
)  
