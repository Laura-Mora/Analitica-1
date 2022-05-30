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
citas <- read_excel("citaschallenge.xlsx")
citasprueba <- read_excel("citaschallenge.xlsx", sheet = "test2013")
citas %>% glimpse()

## Dimensión del conjunto numero de filas y columnas
dim(citas)
dim(citasprueba)

## Nombres de las columnas
colnames(citas)

## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(citas)
str(citasprueba)

summary(citas)
summary(citasprueba)
#-------------------------------------------------------------------------------------
# Análisis descriptivo
#-------------------------------------------------------------------------------------

## Revisión de las variables escalares
describe(citas[,c(2,6,7)])
describe(citasprueba[,c(2,6)])

#Diagrama de barras exploración especialidad
ggplot(citas) +
  geom_bar(mapping = aes(x = ESPECIALIDAD, colour = ESPECIALIDAD))

#Diagrama de barras exploración GENERO
ggplot(citas) +
  geom_bar(mapping = aes(x = GENERO, colour = GENERO))

#Diagrama de barras exploración TIPO_AFILIACION
ggplot(citas) +
  geom_bar(mapping = aes(x = TIPO_AFILIACION, colour = TIPO_AFILIACION))

#boxplot para ver la posible presencia de atípicos
#Edad
boxedad<- citas%>%ggplot(aes(y=EDAD, colour = EDAD))+
  geom_boxplot()+
  ggtitle("Boxplot EDAD")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

hist1 <- citas %>% ggplot(aes(x=EDAD, colour = EDAD)) +
  geom_histogram() +
  ggtitle('Histograma \nEDAD') +
  labs(y = "", x = "EDAD") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist2 <- citas %>% ggplot(aes(x=ESTAFINAL, colour = ESTAFINAL)) +
  geom_histogram() +
  ggtitle('Histograma ESTAFINAL') +
  labs(y = "", x = "ESTAFINAL") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggpubr::ggarrange(boxedad, hist1, hist2, nrow = 1, ncol = 3)

ggplot(data = citas,
       aes(y = ESTAFINAL, x = EDAD, 
           color = factor (GENERO))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = EDAD, 
           color = factor (TIPO_AFILIACION))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = GENERO, 
           color = factor (TIPO_AFILIACION))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = TIPO_AFILIACION, 
           color = factor (ESPECIALIDAD))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = EDAD, 
           color = factor (ESPECIALIDAD))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = GENERO, 
           color = factor (ESPECIALIDAD))) +
  geom_point(alpha = 0.5, position = "jitter")


citas$weekDay <- as.factor(weekdays(as.Date(citas$FECHA_CITA))) 

#Diagrama de barras exploración weekDay
ggplot(citas) +
  geom_bar(mapping = aes(x = weekDay, colour = weekDay))

ggplot(data = citas,
       aes(y = ESTAFINAL, x = GENERO, 
           color = factor (weekDay))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = weekDay, 
           color = factor (GENERO))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = EDAD, 
           color = factor (weekDay))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = weekDay, 
           color = factor (ESPECIALIDAD))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = TIPO_AFILIACION, 
           color = factor (weekDay))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = weekDay, 
           color = factor (TIPO_AFILIACION))) +
  geom_point(alpha = 0.5, position = "jitter")

citas$hourDay <- as.factor(format(as.POSIXct(citas$FECHA_CITA), format = "%H"))

ggplot(citas) +
  geom_bar(mapping = aes(x = hourDay, colour = hourDay))

ggplot(data = citas,
       aes(y = ESTAFINAL, x = GENERO, 
           color = factor (hourDay))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = hourDay, 
           color = factor (GENERO))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = EDAD, 
           color = factor (hourDay))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = hourDay, 
           color = factor (ESPECIALIDAD))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = hourDay, 
           color = factor (TIPO_AFILIACION))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = hourDay, 
           color = factor (TIPO_AFILIACION))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = citas,
       aes(y = ESTAFINAL, x = hourDay, 
           color = factor (weekDay))) +
  geom_point(alpha = 0.5, position = "jitter")


#-------------------------------------------------------------------------------------
# Preparación base de datos
#-------------------------------------------------------------------------------------
citas$GENERO <- as.factor(citas$GENERO)
citas$TIPO_AFILIACION <- as.factor(citas$TIPO_AFILIACION)
citas$ESTAFINAL <- as.factor(citas$ESTAFINAL)

top_3<-(citas %>% count(citas$ESPECIALIDAD, sort= TRUE))[3:32,]
colnames(top_3)[1]<-"especialidad"
citas <- citas %>%                              
  mutate(citas$Especialidad_new <- replace(citas$ESPECIALIDAD, citas$ESPECIALIDAD %in% top_3$especialidad, "OTRO"))
colnames(citas)[8]<-"especialidadNuevos"

citas <- citas %>%                              
  mutate(citas$Asistio <- case_when(citas$ESTAFINAL == 2 ~ "SI",citas$ESTAFINAL != 2 ~ "NO"))
colnames(citas)[9]<-"ASISTENCIA"

borrar <- c("ESPECIALIDAD","ESTAFINAL","id")

citas2 <- citas[ , !(names(citas) %in% borrar)]

borrar <- c("ESPECIALIDAD","id")

citas3 <- citas[ , !(names(citas) %in% borrar)]

#-------------------------------------------------------------------------------------
# Arboles
#-------------------------------------------------------------------------------------

#Convertir a factor
citas2 <- citas2 %>% 
  mutate_if(is.character, as.factor)


#Árbol
set.seed(3435)
arbol <- rpart(ASISTENCIA ~ ., data = citas2)
rpart.plot(arbol) 
rpart.rules(arbol)

unique(citas2$ASISTENCIA)

#Resumen del ?rbol
summary(arbol)

#EL ?rbol completo
ELarbol <-  rpart(ASISTENCIA ~ ., data = citas2, cp = 0.001)
rpart.plot(ELarbol)
printcp(ELarbol)

#Convertir a factor
citas3 <- citas3 %>% 
  mutate_if(is.character, as.factor)


#Árbol
set.seed(3435)
arbol <- rpart(ESTAFINAL ~ ., data = citas3)
rpart.plot(arbol) 
rpart.rules(arbol)

unique(citas3$ESTAFINAL)

#Resumen del ?rbol
summary(arbol)

#EL ?rbol completo
ELarbol <-  rpart(ESTAFINAL ~ ., data = citas3, cp = 0.01)
rpart.plot(ELarbol)
printcp(ELarbol)
