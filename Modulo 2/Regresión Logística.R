# Cargar paquetes
library(ggplot2)
library(dplyr)
library(readxl)

# Creemos la base de datos
paciente <- 1:10
peso <- c(100, 73, 90, 90, 120, 128, 65, 58, 78, 84)
es_obeso <- c("Si", "No", "No", "Si", "Si", "Si", "No", "No", "No", "No")


df <- data.frame(paciente = paciente,
                 peso = peso,
                 es_obeso = es_obeso)

# grafico de dispersion poblacion vs ventas
ggplot(df, aes(x = peso, y = es_obeso)) +
  geom_point(size = 5) +
  labs(x = "Peso del paciente en Kg", y = "Es obeso")

# grafico con regresion logistica ajustada
df <- df %>% mutate(es_obeso = ifelse(es_obeso == "Si", 1, 0))

ggplot(df, aes(x = peso, y = es_obeso)) +
  geom_point(size = 5) +
  labs(x = "Peso del paciente en Kg", y = "Es obeso") +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE)


# Datos IBM---------------------------------------------------------------------

# Leer base de datos
df <- read_excel("Modulo 2/datos_IBM.xlsx")
df2 <- df %>% mutate(Resignation = ifelse(Resignation == "Yes", 1, 0))

# Ajustemos un modelo de regresión logística con la variable MonthlyIncome
ggplot(df2, aes(x = MonthlyIncome, y = Resignation)) +
  geom_point(size = 5) +
  labs(x = "Salario Mensual", y = "Renuncia")

model <- glm(Resignation ~MonthlyIncome, family = binomial(), data = df2)
summary(model)


# Ajustemos un modelo de regresión logística con todas las variables
model2 <- glm(Resignation ~., family = binomial(), data = df2)
summary(model2)

# Chi-Squared Test
summary_model <- summary(model)
difference_deviance <- summary_model$null.deviance - summary_model$deviance

p_value <- 1 - pchisq(difference_deviance, 8)
p_value

# Predicciones del modelo para los primeros 5 empleados
glm.probs <- predict(model, type = "response")
glm.probs[1:5]

glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
glm.pred[1:5]

# Accuracy del modelo
results <- cbind(df$Resignation, glm.pred)
table(glm.pred, df$Resignation)

accuracy_model <- mean(glm.pred == df$Resignation)
accuracy_model

# Ajustemos un modelo de regresión logística con menos variables
model3 <- glm(Resignation ~Age + MaritalStatus + YearsInCurrentRole + YearsSinceLastPromotion, family = binomial(), data = df2)
summary(model3)
AIC(model3)
BIC(model3)


