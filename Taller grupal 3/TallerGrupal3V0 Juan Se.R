if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('MLmetrics')) install.packages('MLmetrics')
if (!require('ROCR')) install.packages('ROCR')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('caret')) install.packages('caret')

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
# An谩lisis descriptivo
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

#---------- Descripci髇 conjunto de Prueba---------------------------------------

citas$GENERO <- as.factor(citas$GENERO)
citas$ESPECIALIDAD <- as.factor(citas$ESPECIALIDAD)
citas$TIPO_AFILIACION <- as.factor(citas$TIPO_AFILIACION)
citas$weekDay <- as.factor(weekdays(as.Date(citas$FECHA_CITA))) 




# #1 Cancelada
# #2 cumplida
# #3 incumplida
# 
# table(citas$ESTAFINAL,citas$weekDay)
# table(citas$ESTAFINAL,citas$GENERO)
# table(citas$ESTAFINAL,citas$TIPO_AFILIACION)
# table(citas$ESTAFINAL,citas$ESPECIALIDAD)


#-----------Construcci髇 Dataset-------------------------------------------------

#-----1. Definir que variables van a ser usados-------
#--------excluir varibable especialidad mientras encuentro uyna forma de juntarlas

train_no_espec <- citas  %>%  select(-ESPECIALIDAD,-FECHA_CITA,-id)

# Crea un set completo de variables dummy
dummy_train_no_espec <- dummyVars("~.",data=train_no_espec)
train_no_espec <- as.data.frame(predict(dummy_train_no_espec,newdata=train_no_espec))

train_no_espec <- train_no_espec %>% 
  mutate(ESTAFINAL = as.factor(ESTAFINAL),
         ESTAFINAL = factor(ESTAFINAL, ordered = TRUE,
                           levels = c("3", "1","2")))
#-----2. Montar modelo ingenuo------------------------
#-----KNN---------------------------------------------

# Par醡etros de validaci髇 cruzada

set_in_use <- train_no_espec

class_1 <- as.data.frame(as.matrix(set_in_use$ESTAFINAL))
class_1$V1[class_1$V1 != "1"] <- 0

class_2 <- as.data.frame(as.matrix(set_in_use$ESTAFINAL))
class_2$V1[class_2$V1 != "2"] <- 0
class_2$V1[class_2$V1 == "2"] <- 1

class_3 <- as.data.frame(as.matrix(set_in_use$ESTAFINAL))
class_3$V1[class_3$V1 != "3"] <- 0
class_3$V1[class_3$V1 == "3"] <- 1


cross <- trainControl(method = "cv", number = 10, allowParallel = TRUE, verboseIter = FALSE)
modeloknn1 <- train(ESTAFINAL~., method = "knn",
                    tuneGrid = expand.grid(k = 1:30),
                    trControl = cross, 
                    metric = "Accuracy",
                    preProcess = c("center","scale"), 
                    tuneLength = 20,
                    data = set_in_use)

modeloknn1
plot(modeloknn1)
resultados <- modeloknn1$results
resultados

predmod1 <- predict(modeloknn1, set_in_use, type = "prob")

predClass_3 <- predmod1 %>% select(1)
predClass_2 <- predmod1 %>% select(3)
predClass_1 <- predmod1 %>% select(2)

# Curva ROC
# Objeto de predicciones y de la curva

#Clase 3
pred_knn_3=prediction(predClass_3,class_3)

pred_knn_3_ROC=performance(pred_knn_3,"tpr","fpr") 
plot(pred_knn_3_ROC, main="ROC plot")

#Clase 2
pred_knn_2=prediction(predClass_2,class_2)

pred_knn_2_ROC=performance(pred_knn_2,"tpr","fpr") 
plot(pred_knn_2_ROC, main="ROC plot")


#Clase 1
pred_knn_1=prediction(predClass_1,class_1)

pred_knn_1_ROC=performance(pred_knn_1,"tpr","fpr") 
plot(pred_knn_1_ROC, main="ROC plot")

#-----3. Montar regresi髇 logistica Multinomial------

current_train<- train_no_espec %>% select(-TIPO_AFILIACION.CONVENIO,-weekDay.Friday,-GENERO.MASCULINO)

help(glm)
modelo.logit <- glm(ESTAFINAL~., family=binomial, current_train)
summary(modelo.logit)

steplogit <- step(modelo.logit, direction="both", trace=0)
summary(steplogit)

#-----4. Montar Arboles de decisi髇----------------------------------


#----------Definir especialidades m醩 importantes-------------------


especialidades <- citas %>% select(ESPECIALIDAD,ESTAFINAL)

especialidades <- especialidades %>% 
  mutate(ESTAFINAL = as.factor(ESTAFINAL),
         ESTAFINAL = factor(ESTAFINAL, ordered = TRUE,
                            levels = c("3", "1","2")))

dummy_espec <- dummyVars("~.",data=especialidades)
train_espec <- as.data.frame(predict(dummy_espec,newdata=especialidades))


if (!require('party')) install.packages('party')
if (!require('rpart')) install.packages('rpart')
if (!require('rpart.plot')) install.packages('rpart.plot')


arbol <- rpart(ESTAFINAL  ~ ., data = train_espec,cp = 0.0001)
rpart.plot(arbol) 
rpart.rules(arbol)


modelo.logit_esp <- glm(ESTAFINAL~., family=binomial, especialidades)
summary(modelo.logit_esp)

step.logit_esp<- step(modelo.logit_esp, direction="both", trace=0)
summary(step.logit_esp)

u_esp <- as.data.frame(unique(especialidades$ESPECIALIDAD))

write.csv(u_esp, "u_esp.csv",row.names = FALSE)




#data set reduciendo las especialidades




