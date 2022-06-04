if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('party')) install.packages('party') #arboles
if (!require('rpart')) install.packages('rpart') #arboles
if (!require('rpart.plot')) install.packages('rpart.plot') #arboles
if (!require('caret')) install.packages('caret')
if (!require('MLmetrics')) install.packages('MLmetrics')
if (!require('ROCR')) install.packages('ROCR')

#-------------------------------------------------------------------------------------
# Lectura de datos
#-------------------------------------------------------------------------------------
# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
caso <- read_excel("caso1.xlsx")

citas %>% glimpse()

## Dimensión del conjunto numero de filas y columnas
dim(caso)


## Nombres de las columnas
colnames(caso)

## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(caso)


summary(caso)

balance <-table(caso$compra)
prop.table(balance)
#-------------------------------------------------------------------------------------
# Análisis descriptivo
#-------------------------------------------------------------------------------------

#Diagrama de barras exploración genero
ggplot(caso) +
  geom_bar(mapping = aes(x = Genero, colour = Genero))

#Diagrama de barras exploración estadocivil
ggplot(caso) +
  geom_bar(mapping = aes(x = estadocivil, colour = estadocivil))

#Diagrama de barras exploración TIPO_AFILIACION
ggplot(caso) +
  geom_bar(mapping = aes(x = edad, colour = edad))

#Diagrama de barras exploración vivienda
ggplot(caso) +
  geom_bar(mapping = aes(x = ViviendaPropia, colour = ViviendaPropia))

#Diagrama de barras exploración vivienda
ggplot(caso) +
  geom_bar(mapping = aes(x = compra, colour = compra))

ggplot(data = caso,
       aes(y = compra, x = edad, 
           color = factor (Genero))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = caso,
       aes(y = compra, x = ViviendaPropia, 
           color = factor (Genero))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = caso,
       aes(y = compra, x = estadocivil, 
           color = factor (Genero))) +
  geom_point(alpha = 0.5, position = "jitter")


ggplot(data = caso,
       aes(y = compra, x = edad, 
           color = factor (estadocivil))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = caso,
       aes(y = compra, x = edad, 
           color = factor (ViviendaPropia))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = caso,
       aes(y = compra, x = estadocivil, 
           color = factor (ViviendaPropia))) +
  geom_point(alpha = 0.5, position = "jitter")
#-------------------------------------------------------------------------------------
# Preparación base de datos
#-------------------------------------------------------------------------------------
caso$Genero <- as.factor(caso$Genero)
caso$estadocivil <- as.factor(caso$estadocivil)
caso$edad <- as.factor(caso$edad)
caso$ViviendaPropia <- as.factor(caso$ViviendaPropia)
caso$compra <- as.factor(caso$compra)

sample <- sample.int(nrow(caso), floor(.75*nrow(caso)))
caso.train <- caso[sample, ]
caso.test <- caso[-sample, ]
#-------------------------------------------------------------------------------------
# Arboles
#-------------------------------------------------------------------------------------

#Árbol
set.seed(3435)
arbol <- rpart(compra ~ ., data = caso.train)
rpart.plot(arbol) 
rpart.rules(arbol)

unique(citas2$compra)

#Resumen del árbol
summary(arbol)


arbolnew <- rpart(compra ~ ., data = caso.train,
                  control = rpart.control(xval = 10, 
                                          minbucket = 5, 
                                          minisplit = 30, 
                                          cp = 0.0041))
rpart.plot(arbolnew)
summary(arbolnew)

pr <- predict(arbol, caso.test, type = "class")
head(caso.test)
head(pr)

pr1 <- predict(arbolnew, caso.test, type = "class")

#matriz de confusi?n (clase mayoritaria)

conf1 <- confusionMatrix(pr,caso.test$compra)
conf2 <- confusionMatrix(pr1,caso.test$compra)

conf1
conf2

