# Cargar paquetes
library(ggplot2)
library(dplyr)
library(readxl)
library(Hmisc)
library(fastDummies)
library(GGally)


# Leer bases de datos
train_df <- read_excel("Modulo 2/titanic_train.xlsx")
test_df <- read_excel("Modulo 2/titanic_test.xlsx")


# Exploremos la base de datos de entrenamiento
str(train_df)
train_df$Age <- as.numeric(train_df$Age)
train_df$Survived <- as.character(train_df$Survived)
train_df$Pclass <- as.character(train_df$Pclass)
train_df$Fare <- as.numeric(train_df$Fare)

# Survived
train_df %>% group_by(Survived) %>%
  count()

# Datos faltantes
sum(is.na(train_df))
colSums(is.na(train_df))

# Age
ggplot(train_df, aes(x=Age)) + geom_histogram()

# imputacion de valores faltantes
train_df$Age <- impute(train_df$Age, median)
train_df$Age <- as.numeric(train_df$Age)

# Embarked
train_df %>% group_by(Embarked) %>%
  count()

# imputacion de valores faltantes
train_df <- train_df %>% mutate(Embarked = ifelse(is.na(Embarked), "S", Embarked))

# Creemos una nueva variable que indique si la persona viaja sola o no

# SibSp       
train_df %>% group_by(SibSp) %>%
  count()

# Parch        
train_df %>% group_by(Parch) %>%
  count()

train_df <- train_df %>% mutate(Travel_alone = ifelse(train_df$SibSp + train_df$Parch == 0, 1, 0))
train_df$Travel_alone <- as.character(train_df$Travel_alone)
train_df %>% group_by(Travel_alone) %>%
  count()

train_df <- train_df %>% select(-SibSp, -Parch, -Name, -Ticket, -Cabin,
                                -PassengerId)

# Análisis de variables
# Age
ggplot(train_df, aes(x = Age, fill = Survived)) +
geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))

# Fare
ggplot(train_df, aes(x = Fare, fill = Survived)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))

# Travel_alone
train_df %>% group_by(Travel_alone, Survived) %>%
             count() %>%
             ggplot(aes(fill=Survived, y=n, x=Travel_alone)) +
             geom_bar(position='dodge', stat='identity')

# Sex
train_df %>% group_by(Sex, Survived) %>%
  count() %>%
  ggplot(aes(fill=Survived, y=n, x=Sex)) +
  geom_bar(position='dodge', stat='identity')

# Pclass
train_df %>% group_by(Pclass, Survived) %>%
  count() %>%
  ggplot(aes(fill=Survived, y=n, x=Pclass)) +
  geom_bar(position='dodge', stat='identity')


# Ajustemos modelo de regresión logistica
train_df$Survived <- as.numeric(train_df$Survived)
modelo <- glm(Survived ~., data = train_df, family = binomial())
summary(modelo)

# Predicciones del modelo para los primeros 5 pasajeros
glm.probs <- predict(modelo, type = "response")
glm.probs[1:5]

glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
glm.pred[1:5]

# Accuracy del modelo
table(glm.pred, train_df$Survived)

accuracy_model <- mean(glm.pred == train_df$Survived)
accuracy_model

# Hagamos predicciones con el modelo usando la base de datos de test

# preprocesamiento de la base de datos
test_df$Age <- as.numeric(test_df$Age)
test_df$Pclass <- as.character(test_df$Pclass)
test_df$Fare <- as.numeric(test_df$Fare)

colSums(is.na(test_df))

test_df$Age <- impute(test_df$Age, median)
test_df$Fare <- impute(test_df$Fare, median)

test_df <- test_df %>% mutate(Travel_alone = ifelse(SibSp + Parch == 0, 1, 0))
test_df$Travel_alone <- as.character(test_df$Travel_alone)
test_df <- test_df %>% select(-SibSp, -Parch, -Name, -Ticket, -Cabin,
                              -PassengerId)


# prediccion
predict(modelo, test_df, type = "response")



