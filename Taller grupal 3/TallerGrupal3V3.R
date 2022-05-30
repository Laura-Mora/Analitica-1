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
#--Lectura de datos--
#-------------------------------------------------------------------------------------
# Lectura de los datos que deben estar en la ruta que genera el siguiente c贸digo
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
citas <- read_excel("citaschallenge.xlsx")
citasprueba <- read_excel("citaschallenge.xlsx", sheet = "test2013")
citas %>% glimpse()

## Dimensi贸n del conjunto numero de filas y columnas
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
#-An谩lisis descriptivo---
#-------------------------------------------------------------------------------------

## Revisi贸n de las variables escalares
describe(citas[,c(2,6,7)])
describe(citasprueba[,c(2,6)])

#Diagrama de barras exploraci贸n especialidad
ggplot(citas) +
  geom_bar(mapping = aes(x = ESPECIALIDAD, colour = ESPECIALIDAD))

#Diagrama de barras exploraci贸n GENERO
ggplot(citas) +
  geom_bar(mapping = aes(x = GENERO, colour = GENERO))

#Diagrama de barras exploraci贸n TIPO_AFILIACION
ggplot(citas) +
  geom_bar(mapping = aes(x = TIPO_AFILIACION, colour = TIPO_AFILIACION))

#boxplot para ver la posible presencia de at铆picos
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

#Diagrama de barras exploraci贸n weekDay
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
#--------------------------Reducir Especialidades----------------------------------------
#-------------------------------------------------------------------------------------

if (!require('stringi')) install.packages('stringi')

#---------------------------Ranking Especialidades-----------------------------------------
especialidades <- citas %>% select(ESPECIALIDAD,ESTAFINAL)

dummy_espec <- dummyVars("~.",data=especialidades)
train_espec <- as.data.frame(predict(dummy_espec,newdata=especialidades))


X.train <- as.matrix(select(train_espec,-ESTAFINAL))
Y.train <- as.matrix(select(train_espec,ESTAFINAL))


modelo.ranking <- glmnet(x=X.train,y=Y.train, family="multinomial",lambda = 0,
                         type.multinomial = "grouped")
mod_Ranking.nnet <- nnet::multinom(
  ESTAFINAL ~ .,
  data = train_espec
)

mod_Ranking.nnet


#------------------------Reducci髇------------------------------------------------------

citas_redux <- citas %>% select(-id,-FECHA_CITA)

citas_redux$ESPECIALIDAD <- as.character(citas_redux$ESPECIALIDAD)

#reemplazar especialidades no significativas con otra especialidad.

citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"MEDICINA GENERAL","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"PEDIATRIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"GASTROENTEROLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"FONOAUDIOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"FISIOTERAPIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"ODONTOLOGIA PEDIATRICA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"OPTOMETRIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"CIRUGIA MAXILOFACIAL","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"CIRUGIA DE LA MANO","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"PSIQUIATRIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"REHABILITACION ORAL","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"OTORRINOLARINGOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"FISIATRIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"NEUROLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"UROLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"OFTALMOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"DERMATOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"PERIODONCIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"GINECOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"PSICOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"ODONTOLOGIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD," Y OBSTETRICIA","")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"CIRUGIA GENERAL","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"ACUPUNTURA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"ORTODONCIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"ENDODONCIA","OTRA ESPEC")
citas_redux$ESPECIALIDAD = stri_replace_all_fixed(citas_redux$ESPECIALIDAD,"MEDICINA INTERNA","OTRA ESPEC")

citas_redux$ESPECIALIDAD <- as.factor(citas_redux$ESPECIALIDAD) 

#-------------------------------------------------------------------------------------
#---Preparaci贸n base de datos----
#-------------------------------------------------------------------------------------
citas_redux$GENERO <- as.factor(citas_redux$GENERO)
citas_redux$TIPO_AFILIACION <- as.factor(citas_redux$TIPO_AFILIACION)
citas_redux$ESTAFINAL <- as.factor(citas_redux$ESTAFINAL)


citas_asistio <- citas_redux %>%
  mutate(asistio = ifelse(citas_redux$ESTAFINAL ==2,1,0))

citas_asistio$asistio <- as.factor(citas_asistio$asistio)

#creando subset de las 3 clases para medir performance

class_1 <- as.data.frame(as.matrix(citas_redux$ESTAFINAL))
class_1$V1[class_1$V1 != "1"] <- 0

class_2 <- as.data.frame(as.matrix(citas_redux$ESTAFINAL))
class_2$V1[class_2$V1 != "2"] <- 0
class_2$V1[class_2$V1 == "2"] <- 1

class_3 <- as.data.frame(as.matrix(citas_redux$ESTAFINAL))
class_3$V1[class_3$V1 != "3"] <- 0
class_3$V1[class_3$V1 == "3"] <- 1


#-------------------------------------------------------------------------------------
#----Ingenuo Naive Bayes----
#-------------------------------------------------------------------------------------

set_in_use <- citas_redux


if (!require('e1071')) install.packages('e1071')

naivetest <- naiveBayes(ESTAFINAL~., set_in_use)

prednaiveprob <- as.data.frame(predict(naivetest, set_in_use, type = "raw"))

pred_naive_Class_1 <- prednaiveprob %>% select(1)
pred_naive_Class_2 <- prednaiveprob %>% select(2)
pred_naive_Class_3 <- prednaiveprob %>% select(3)

# Curva ROC
# Objeto de predicciones y de la curva

#Clase 3
pred_naive_3=prediction(pred_naive_Class_3,class_3)

pred_naive_3_ROC=performance(pred_naive_3,"tpr","fpr") 
plot(pred_naive_3_ROC, main="ROC plot Naive 3")

auc_naive_3 <- ROCR::performance(pred_naive_3, measure = "auc")
auc_naive_3 <- auc_naive_3@y.values[[1]]

#Clase 2
pred_naive_2=prediction(pred_naive_Class_2,class_2)

pred_naive_2_ROC=performance(pred_naive_2,"tpr","fpr") 
plot(pred_naive_2_ROC, main="ROC plot Naive 2")

auc_naive_2 <- ROCR::performance(pred_naive_2, measure = "auc")
auc_naive_2 <- auc_naive_2@y.values[[1]]


#Clase 1
pred_naive_1=prediction(pred_naive_Class_1,class_1)

pred_naive_1_ROC=performance(pred_naive_1,"tpr","fpr") 
plot(pred_naive_1_ROC, main="ROC plot Naive 1")

auc_naive_1 <- ROCR::performance(pred_naive_1, measure = "auc")
auc_naive_1 <- auc_naive_1@y.values[[1]]

auc_naive_3
auc_naive_2
auc_naive_1

pred_naive_Clases <- predict(naivetest, set_in_use, type = "class")

conf_M_naive <- confusionMatrix(pred_naive_Clases,as.factor(set_in_use$ESTAFINAL))
conf_M_naive


#-------------------------------------------------------------------------------------
#----Arboles-------
#-------------------------------------------------------------------------------------

#Sin variable asist韔


set_in_use <- citas_redux


arbol_inicial <- rpart(ESTAFINAL ~ ., data = citas_redux,
                       control = rpart.control(xval = 10, 
                                               cp = 0.0001))
rpart.plot(arbol_inicial) 
rpart.rules(arbol_inicial)


#Evaluar arbol

pred_arbol_inicial <-  as.data.frame(predict(arbol_inicial, set_in_use, type = "prob"))

pred_arbol_inicial_Class_1 <- pred_arbol_inicial %>% select(1)
pred_arbol_inicial_Class_2 <- pred_arbol_inicial %>% select(2)
pred_arbol_inicial_Class_3 <- pred_arbol_inicial %>% select(3)


# Curva ROC
# Objeto de predicciones y de la curva

#Clase 3

pred_arbol_inicial_3 <- prediction(pred_arbol_inicial_Class_3,class_3)

pred_arbol_inicial_3_ROC <- ROCR::performance(pred_arbol_inicial_3,"tpr","fpr") 
plot(pred_arbol_inicial_3_ROC, main="ROC plot Arboles Clase 3")

auc_arbol_inicial_3 <- ROCR::performance(pred_arbol_inicial_3, measure = "auc")
auc_arbol_inicial_3 <- auc_arbol_inicial_3@y.values[[1]]

#Clase 2

pred_arbol_inicial_2 <- prediction(pred_arbol_inicial_Class_2,class_2)

pred_arbol_inicial_2_ROC <- ROCR::performance(pred_arbol_inicial_2,"tpr","fpr") 
plot(pred_arbol_inicial_2_ROC, main="ROC plot Arboles Clase 2")

auc_arbol_inicial_2 <- ROCR::performance(pred_arbol_inicial_2, measure = "auc")
auc_arbol_inicial_2 <- auc_arbol_inicial_2@y.values[[1]]


#Clase 1

pred_arbol_inicial_1 <- prediction(pred_arbol_inicial_Class_1,class_1)

pred_arbol_inicial_1_ROC <- ROCR::performance(pred_arbol_inicial_1,"tpr","fpr") 
plot(pred_arbol_inicial_1_ROC, main="ROC plot Arboles Clase 1")

auc_arbol_inicial_1 <- ROCR::performance(pred_arbol_inicial_1, measure = "auc")
auc_arbol_inicial_1 <- auc_arbol_inicial_1@y.values[[1]]


pred_arbol_inicial_Clases <- predict(arbol_inicial, set_in_use, type = "class")

table(pred_arbol_inicial_Clases)
table(as.factor(set_in_use$ESTAFINAL))

conf_M_Arbol_inicial <- confusionMatrix(pred_arbol_inicial_Clases,as.factor(set_in_use$ESTAFINAL))

conf_M_Arbol_inicial$byClass

conf_M_Arbol_inicial


#Variable Asistio

set_in_use <- citas_asistio


arbol_inicial <- rpart(ESTAFINAL ~ ., data = citas_asistio,
                   control = rpart.control(xval = 10, 
                                           cp = 0.001))
rpart.plot(arbol_inicial) 
rpart.rules(arbol_inicial)


#Evaluar arbol

pred_arbol_inicial <-  as.data.frame(predict(arbol_inicial, set_in_use, type = "prob"))

pred_arbol_inicial_Class_1 <- pred_arbol_inicial %>% select(1)
pred_arbol_inicial_Class_2 <- pred_arbol_inicial %>% select(2)
pred_arbol_inicial_Class_3 <- pred_arbol_inicial %>% select(3)



# Curva ROC
# Objeto de predicciones y de la curva

#Clase 3

pred_arbol_inicial_3 <- prediction(pred_arbol_inicial_Class_3,class_3)

pred_arbol_inicial_3_ROC <- ROCR::performance(pred_arbol_inicial_3,"tpr","fpr") 
plot(pred_arbol_inicial_3_ROC, main="ROC plot Arboles Clase 3")

auc_arbol_inicial_3 <- ROCR::performance(pred_arbol_inicial_3, measure = "auc")
auc_arbol_inicial_3 <- auc_arbol_inicial_3@y.values[[1]]

#Clase 2

pred_arbol_inicial_2 <- prediction(pred_arbol_inicial_Class_2,class_2)

pred_arbol_inicial_2_ROC <- ROCR::performance(pred_arbol_inicial_2,"tpr","fpr") 
plot(pred_arbol_inicial_2_ROC, main="ROC plot Arboles Clase 2")

auc_arbol_inicial_2 <- ROCR::performance(pred_arbol_inicial_2, measure = "auc")
auc_arbol_inicial_2 <- auc_arbol_inicial_2@y.values[[1]]


#Clase 1

pred_arbol_inicial_1 <- prediction(pred_arbol_inicial_Class_1,class_1)

pred_arbol_inicial_1_ROC <- ROCR::performance(pred_arbol_inicial_1,"tpr","fpr") 
plot(pred_arbol_inicial_1_ROC, main="ROC plot Arboles Clase 1")

auc_arbol_inicial_1 <- ROCR::performance(pred_arbol_inicial_1, measure = "auc")
auc_arbol_inicial_1 <- auc_arbol_inicial_1@y.values[[1]]


pred_arbol_inicial_Clases <- predict(arbol_inicial, set_in_use, type = "class")

table(pred_arbol_inicial_Clases)
table(as.factor(set_in_use$ESTAFINAL))

conf_M_Arbol_inicial <- confusionMatrix(pred_arbol_inicial_Clases,as.factor(set_in_use$ESTAFINAL))

conf_M_Arbol_inicial$byClass

conf_M_Arbol_inicial






# #Convertir a factor
# citas2 <- citas2 %>% 
#   mutate_if(is.character, as.factor)
# 
# 
# #arbol
# set.seed(3435)
# arbol <- rpart(ASISTENCIA ~ ., data = citas2)
# rpart.plot(arbol) 
# rpart.rules(arbol)
# 
# unique(citas2$ASISTENCIA)
# 
# #Resumen del arbol
# summary(arbol)
# 
# #EL arbol completo
# ELarbol <-  rpart(ASISTENCIA ~ ., data = citas2, cp = 0.001)
# rpart.plot(ELarbol)
# printcp(ELarbol)
# 
# #Convertir a factor
# citas3 <- citas3 %>% 
#   mutate_if(is.character, as.factor)
# 
# 
# #arbol
# set.seed(3435)
# arbol <- rpart(ESTAFINAL ~ ., data = citas3)
# rpart.plot(arbol) 
# rpart.rules(arbol)
# 
# unique(citas3$ESTAFINAL)
# 
# #Resumen del arbol
# summary(arbol)
# 
# #EL ?rbol completo
# ELarbol <-  rpart(ESTAFINAL ~ ., data = citas3, cp = 0.01)
# rpart.plot(ELarbol)
# printcp(ELarbol)
