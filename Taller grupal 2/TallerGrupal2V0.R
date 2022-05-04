if (!require('caret')) install.packages('caret') #modelos supervisados
if (!require('corrplot')) install.packages('corrplot') 
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('glmnet')) install.packages('glmnet')# redes elasticas en el modelo lineal generalizado
if (!require('corrr')) install.packages('corrr')#correlaciones en unas tablas
if (!require('Metrics')) install.packages('Metrics')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('ggpubr')) install.packages('ggpubr') 
if (!require('GGally')) install.packages('GGally')

#Lectura e inspección de estructura de los datos

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
telcoprueba <- read_excel("testelco.xlsx")
telco <- read_excel("traintelco.xlsx")


telco %>% glimpse()
## Nombres de las columnas
telco %>% colnames()

## Dimensión del conjunto numero de filas y columnas
dim(telco)

telco%>% View()
## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(telco)

summary(telco)

histo <- telco %>% ggplot(aes(x=facturación  , colour = facturación)) +
  geom_histogram() +
  ggtitle('Histograma \nnomina') +
  labs(y = "", x = "nomina") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
histo