
# Instalar librerias ------------------------------------------------------

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("plotly")
install.packages("RColorBrewer")

# Cargar librerias --------------------------------------------------------
library(readr) # Proporciona una forma rápida y amigable de leer datos
library(dplyr) # Manipula datos de una forma más fácil
library(ggplot2) # Crea visualizaciones elegantes de datos
library(stringr) # Permite manipular strings
library(plotly) # Crea gráficos interactivos
library(RColorBrewer) # Proporciona varias paletas de colores


# Leer base de datos ------------------------------------------------------

df <- read.csv2("https://raw.githubusercontent.com/SaraGarcesCespedes/Curso-Analisis-de-datos/main/Modulo%201/Encuesta_Juventud.csv", 
                header = TRUE, sep = ",", dec = ",", stringsAsFactors = TRUE)

# Describir la base de datos ----------------------------------------------

# ver los primeros registros de df
head(df)

# ver los útlimo registros de df
tail(df)

# ver el número de columnas y de filas de df
dim(df)

# ver las columnas de df
colnames(df)

# ver un resumen de df
str(df)

# Resumen de las variables
summary(df)

# Organizar la base de datos ----------------------------------------------

# ¿Como funciona el operador Pipe?
# Cambiar el nombre de una o mas columnas

# Funcion rename()
rename(df, Genero = "Sexo", 
       ParticipacionActividadesComunitarias = "ActividadesComunitarias")

# Funcion rename usando el operador pipe
df <- df %>% rename(Genero = "Sexo", 
                    ParticipacionActividadesComunitarias = "ActividadesComunitarias")

# Poner todos los nombres de columnas en minusculas
colnames(df) <- str_to_lower(colnames(df))


# Crear nuevas variables o modificar las existentes

# Funcion mutate()

# valores unicos de la variable nivelestudio

unique(df$nivelestudio)
#unique(df[, "nivelestudio"])

primaria <- c("Grado 1", "Grado 2", "Grado 3", "Grado 4", "Grado 5")
bachillerato <- c("Grado 6", "Grado 7", "Grado 8", "Grado 9", "Grado 10",
                  "Grado 11")
superior <- c("Tecnica incompleta o en curso", "Tecnica Completa", "Tecnologia Completa",
              "Universitaria completa", "Tecnologia Incompleta o en curso", 
              "Universitaria Incompleta o en curso")


df <- df %>% mutate(nivelestudio_nuevo = case_when(
                    nivelestudio %in% primaria ~ "EDUCACION BASICA",
                    nivelestudio %in% bachillerato ~ "EDUCACION MEDIA",
                    nivelestudio %in% superior ~ "EDUCACION SUPERIOR",
                    nivelestudio == "No sabe/ No informa" ~ "No sabe/ No informa"
                    ))

unique(df$nivelestudio_nuevo)

# Crear nueva columna con rango de edades
min(df$edad)
max(df$edad)

df <- df %>% mutate(rangoedad = case_when(between(edad, 14, 17) ~ "14-17",
                                          between(edad, 18, 21) ~ "18-21", 
                                          between(edad, 22, 26) ~ "22-26",))

unique(df$rangoedad)

# Solucionar errores en la base de datos
unique(df$genero)
unique(df$estudia)

# Poner todas las columnas categoricas en minuscula
df <- mutate_if(df, is.factor, str_to_lower)
unique(df$estudia)

# Modificar columna genero
unique(df$genero)

df <- df %>% mutate(genero = case_when(genero == "femenino" ~ "mujer",
                                       genero == "maculino" ~ "hombre",
                                       genero == "hombre" ~ "hombre",
                                       genero == "mujer" ~ "mujer"))

unique(df$genero)


# Cambiar el orden de las columnas

# Funcion relocate()
df <- df %>% relocate(nivelestudio_nuevo)

# .after y .before
df <- df %>% relocate(nivelestudio_nuevo, .after = nivelestudio) %>% 
             relocate(estudia, .before = nivelestudio)

# Omitiendo valores faltantes

is.na(df)
sum(is.na(df))

df <- na.omit(df)


# Seleccionar y eliminar columnas de la base de datos

# Funcion select()

df2 <- df %>% select(edad, genero, estadocivil)

df3 <- df %>% select(-rangoedad, -nivelestudio)

# Filtrar segun condicion

# Funcion filter()
df_mujer <- df %>% filter(genero == "mujer")
df_hombre <- df %>% filter(genero == "hombre")




# Analizar la base de datos -----------------------------------------------



# Conteo por variable

# Cantidad de hombres y de mujeres 
df %>% group_by(genero) %>%
       summarise(conteo = n())
       

# Cantidad de personas por rango de edad
df %>% group_by(rangoedad) %>%
       summarise(conteo = n())

# Cantidad de personas y promedio de edad por estado civil
df %>% group_by(estadocivil) %>%
       summarise(conteo = n(),
                 prom_edad = mean(edad)) %>%
       arrange(-conteo)

# Ver consumo de drogas diferenciado por genero
df %>% group_by(genero, consumodrogas) %>%
       summarise(conteo = n())
       

# Construir graficos con ggplot2 ------------------------------------------

# Graficos de dispersion

# Grafico de gastosemanalrumba vs edad vs genero

ggplot(data = df, aes(x = edad, y = gastosemanalrumba)) + # Capa base
       geom_point() + #Capa de puntos
       facet_wrap(~ genero) +
       stat_smooth(method = "lm", se = FALSE, col = "red") +
       scale_y_continuous("Gasto semanal en Rumba", breaks = seq(0,125000,10000)) +
       theme_bw()

ggplot(data = df, aes(x = edad, y = gastosemanalrumba, color = genero)) +
  geom_point()

# Añadir interactividad
Plot = ggplot(data = df, aes(x = edad, y = gastosemanalrumba, color = genero)) +
  geom_point()

ggplotly(Plot)

# Grafico de gastosemanalrumba vs edad vs estrato

ggplot(data = df, aes(x = edad, y = gastosemanalrumba, colour = genero)) +
  geom_point(size = 2) + 
  theme_dark() +
  scale_x_continuous(breaks = seq(14,26,2)) +
  labs(y = "Gasto semanal en Rumba") +
  facet_wrap(~ estrato) 

# Boxplots

ggplot(data = df, aes(x = genero, y = gastosemanalrumba)) +
       geom_boxplot()


ggplot(data = df, aes(x = genero, y = gastosemanalrumba, fill = genero)) +
  geom_boxplot() + 
  theme_bw() + #Tema de fondo del gráfico
  theme(legend.position="top", plot.title = element_text(face = "bold", 
                                                         size =12,
                                                         hjust = 0.5), 
        plot.subtitle = element_text(size = 10, hjust = 0.5)) + 
  labs(y = "Gastos semanales en pesos",
       title = "Gastos semanales en rumba vs sexo", subtitle = "Taller ggplot2",
       caption = "Encuesta Juventud | MEDATA")


# Histogramas

# Histograma de edad
ggplot(df, aes(x=edad))+
  geom_histogram(breaks = seq(14,26,1), fill="#702899", color="black", bins = 10) +
  theme_bw() + 
  labs(y = "Frecuencia") + 
  scale_x_continuous(breaks = seq(14,26,2))

# Histograma de edad con mensaje
ggplot(df, aes(x=edad))+
  geom_histogram(breaks = seq(14,26,1), fill="#702899", color="black", bins = 10) +
  theme_bw() + labs(y = "Frecuencia") + scale_x_continuous(breaks = seq(14,26,2)) +
  geom_vline(xintercept = 18,linetype="dashed", size=1.5, colour="#b82b14") +
  annotate("text", label = "Mayores de edad", x = 22, y = 1600, size = 6, 
           colour = "#b82b14") 

# Histograma de edad diferenciado por colores
ggplot(df, aes(x=edad, colour = genero, fill = genero))+
  geom_histogram(breaks = seq(14,26,1), bins = 10, position = "dodge", alpha = 0.7)+
  theme_bw() + labs(y = "Frecuencia") + scale_x_continuous(breaks = seq(14,26,2)) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2")


# Graficos de barras

# Grafico valoracionoportunidadesdelaciudadparaestudiar

# Forma 1
df %>% 
  ggplot(aes(x= valoracionoportunidadesdelaciudadparaestudiar)) + 
  geom_bar(stat = "count", width = 0.5, fill = "#702899") +
  theme_bw() + 
  labs(x = "Oportunidades para estudiar")

# Forma 2
df %>% group_by(valoracionoportunidadesdelaciudadparaestudiar) %>% 
  summarise(Frecuencia = n()) %>% 
  ggplot(aes(x= valoracionoportunidadesdelaciudadparaestudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() + 
  labs(x = "Oportunidades para estudiar") 

# Grafico de barras horizontal
df %>% group_by(valoracionoportunidadesdelaciudadparaestudiar) %>% 
  summarise(Frecuencia = n()) %>% 
  ggplot(aes(x= valoracionoportunidadesdelaciudadparaestudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() + 
  labs(x = "Oportunidades para estudiar") + 
  coord_flip()
  
# grafico valoracionoportunidadesdelaciudadparaestudiar mejorado
df %>% group_by(valoracionoportunidadesdelaciudadparaestudiar) %>% 
  summarise(Frecuencia = n()) %>% 
  ggplot(aes(x= valoracionoportunidadesdelaciudadparaestudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() +
  labs(x = "Oportunidades para estudiar")+
  geom_text(size = 3.5,aes(label = Frecuencia), vjust = -0.5)+ 
  scale_y_continuous(breaks = seq(0,4000,500)) +
  scale_x_discrete(limits = c("muy desfavorable", "desfavorable",
                              "ni favorable/ ni desfavorable",
                              "favorable", "muy favorable"))

# grafico valoracionoportunidadesdelaciudadparaestudiar por genero
df %>% group_by(valoracionoportunidadesdelaciudadparaestudiar, genero) %>%
  summarise(Frecuencia = n()) %>% 
  ggplot(aes(x= valoracionoportunidadesdelaciudadparaestudiar, y= Frecuencia)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "#702899") +
  theme_bw() +
  labs(x = "Oportunidades para estudiar")+
  geom_text(size = 3.5,aes(label = Frecuencia), vjust = -0.5)+ 
  scale_y_continuous(breaks = seq(0,4000,500)) +
  scale_x_discrete(limits = c("muy desfavorable", "desfavorable",
                              "ni favorable/ ni desfavorable",
                              "favorable", "muy favorable"),
                   labels = c("muy \ndesfavorable", "desfavorable",
                              "ni favorable \nni desfavorable",
                              "favorable", "muy \nfavorable")
                   ) +
  facet_wrap(~ genero) 

# grafico de barras estudia vs trabaja
df %>% group_by(estudia, trabaja) %>%
  summarise(Frecuencia = n()) %>% 
  ggplot(aes(x= estudia, y= Frecuencia, fill = trabaja)) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  theme_bw()




