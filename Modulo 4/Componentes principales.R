library(ggplot2)
library(dplyr)
library(readxl)

# Calculas valores y vectores propios de una matriz

M <- matrix(c(1,-1,0,3,2,1,-2,3,4), ncol = 3)
M

cov_matrix <- cov(M)
cov_matrix

e <- eigen(cov_matrix)
e$values
e$vectors




