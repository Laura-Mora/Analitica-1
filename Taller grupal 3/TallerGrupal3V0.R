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


#-------------------------------------------------------------------------------------
# Preparación base de datos
#-------------------------------------------------------------------------------------
citas$GENERO <- as.factor(citas$GENERO)
citas$TIPO_AFILIACION <- as.factor(citas$TIPO_AFILIACION)

top_3<-(citas %>% count(citas$ESPECIALIDAD, sort= TRUE))[4:32,]
colnames(top_3)[1]<-"especialidad"
citas <- citas %>%                              
  mutate(citas$Especialidad_new <- replace(citas$ESPECIALIDAD, citas$ESPECIALIDAD %in% top_3$especialidad, "OTRO"))
colnames(citas)[8]<-"especialidadNuevos"

borrar <- c("ESPECIALIDAD")

citas2 <- citas[ , !(names(citas) %in% borrar)]

#-------------------------------------------------------------------------------------
# KNN (se queda corriendo y no pasa nada F, son muuuuuuuchos datos para el pobre KNN) 
#-------------------------------------------------------------------------------------
# Crea un set completo de variables dummy
citaswin <- dummyVars("~.",data=citas2)
citasfin <- as.data.frame(predict(citaswin,newdata=citas))

# Convertir y organizar como factor los retrasos
citasfin2 <- citasfin %>% 
  mutate(ESTAFINAL = as.factor(ESTAFINAL))

set.seed(1545867) 

# Haciendo la partici?n de los datos

sample <- sample.int(nrow(citasfin2), floor(.75*nrow(citasfin2)))
citas.train <- citasfin2[sample, ]
citas.test <- citasfin2[-sample, ]

# Par?metros de validaci?n cruzada
cross <- trainControl(method = "cv", number = 10)
modeloknn1 <- train(ESTAFINAL~., method = "knn",
                    tuneGrid = expand.grid(k = 1:30),
                    trControl = cross, 
                    metric = "Accuracy",
                    data = citas.train)
modeloknn1
plot(modeloknn1)
resultados <- modeloknn1$results
resultados

levels(citas.train$ESTAFINAL) <- make.names(levels(factor(citas.train$ESTAFINAL)))

# Par?metros de validaci?n cruzada 
cross <- trainControl(method = "cv", number = 5,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)
modeloknn2 <- train(ESTAFINAL~., method = "knn",
                    tuneGrid = expand.grid(k=1:30),
                    trControl = cross, 
                    metric = "ROC",
                    data = citas.train)
modeloknn2
plot(modeloknn2)
modeloknn2$results

# del modelo sintonizado en test
predmod1 <- predict(modeloknn2, citas.test, type = "prob")
pronknn1 <- ifelse(predmod1$X1 > 0.5, 1, 0)
confknn1 <- confusionMatrix(as.factor(pronknn1),
                            citas.test$ESTAFINAL, positive = "1")
confknn1$table
confknn1$byClass

# Curva ROC
# Objeto de predicciones y de la curva
pr <- prediction(as.numeric(pronknn1), 
                 as.numeric(citas.test$ESTAFINAL))
curvaROC <- performance(pr, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC)
# Calcular el AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
# Ver el AUC
auc

# El modelo con un k escogido
modeloknn3 <- train(ESTAFINAL~., method = "knn",
                    tuneGrid = expand.grid(k = 8:8),
                    trControl = cross, 
                    metric = "ROC",
                    data = citas.train)
modeloknn3$results

# Desempe?o del modelo en test
predmod2 <- predict(modeloknn3, cartera.test, type = "prob")
pronknn2 <- ifelse(predmod2$X1 > 0.5, 1, 0)

confknn2 <- confusionMatrix(as.factor(pronknn2),
                            citas.test$ESTAFINAL, positive = "1")
confknn2$table
confknn2$byClass
confknn1$byClass


# Curva ROC
# Objeto de predicciones y de la curva
pr1 <- prediction(as.numeric(pronknn2), 
                  as.numeric(citas.test$ESTAFINAL))
curvaROC1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC1)
# Calcular el AUC
auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
# Ver el AUC
auc1

#-------------------------------------------------------------------------------------
# Bayes
#-------------------------------------------------------------------------------------
if (!require('e1071')) install.packages('e1071')
if (!require('caret')) install.packages('caret')
if (!require('ROCR')) install.packages('ROCR')

# Análisis de independencia
# H0: las variables son independientes
# H1: las variables son dependientes

chisq.test(table(citas$GENERO, citas$EDAD))
chisq.test(table(citas$GENERO, citas$TIPO_AFILIACION))
chisq.test(table(citas$GENERO, citas$ESTAFINAL))
chisq.test(table(citas$GENERO, citas$ESPECIALIDAD))
chisq.test(table(citas$GENERO, citas$FECHA_CITA))

chisq.test(table(citas$EDAD, citas$ESPECIALIDAD))
chisq.test(table(citas$EDAD, citas$TIPO_AFILIACION))
chisq.test(table(citas$EDAD, citas$FECHA_CITA))
chisq.test(table(citas$EDAD, citas$ESTAFINAL))

chisq.test(table(citas$TIPO_AFILIACION, citas$ESPECIALIDAD))
chisq.test(table(citas$TIPO_AFILIACION, citas$FECHA_CITA))
chisq.test(table(citas$TIPO_AFILIACION, citas$ESTAFINAL))

chisq.test(table(citas$ESTAFINAL, citas$ESPECIALIDAD))
chisq.test(table(citas$ESTAFINAL, citas$FECHA_CITA)) 

chisq.test(table(citas$FECHA_CITA, citas$ESPECIALIDAD))

# Haciendo la partición de la base
set.seed(19854)
sample <- sample.int(nrow(citas), floor(.7*nrow(citas)))
citas.train <- citas[sample, ]
citas.test <- citas[-sample, ]

prop.table(table(citas.train$ESTAFINAL))

# Corriendo el algoritmo
naivetest <- naiveBayes(ESTAFINAL~., citas.train)
naivetest

# Tasas base
naivetest$apriori
prop.table(naivetest$apriori)

# Probabilidades marginales
naivetest$tables$GENERO
naivetest$tables$EDAD
naivetest$tables$ESPECIALIDAD
naivetest$tables$TIPO_AFILIACION
naivetest$tables$FECHA_CITA

# Guarda las probabilidades (para curva ROC)
prednaiveprob <- predict(naivetest, citas.test, type = "raw")
prednaiveprob[1,]
# Guarda el pron?stico
# prednaive<-predict(naivetest, cereal.test)

prednaive <- colnames(prednaiveprob)[max.col(prednaiveprob, 
                                             ties.method = "first")]
prednaive %>% glimpse()

# Generar la matriz de confusi?n
naiveconf <- confusionMatrix(as.factor(prednaive),
                             as.factor(citas.test$ESTAFINAL))
naiveconf$table
naiveconf$byClass

#1 Cancelada
#2 cumplida
#3 incumplida

# AUC para la predicci?n de cada clase
# Crear resultados 1 o 0 para la predicción de Cancelada
cancelada <- ifelse(citas.test$ESTAFINAL == 1, 1, 0)

# Crear el objeto de predicci?n
predcor <- prediction(prednaiveprob[,1], cancelada)

curvaROC <- performance(predcor, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC, main = "Curva ROC - Cancelada")
# Calcular el AUC
auc <- performance(predcor, measure = "auc")
auc <- auc@y.values[[1]]
# Ver el AUC
auc

# Crear resultados 1 o 0 para la predicción de Cumplida
cumplida <- ifelse(citas.test$ESTAFINAL == 2, 1, 0)

# Crear el objeto de predicci?n
predcor2 <- prediction(prednaiveprob[,1], cumplida)

curvaROC2 <- performance(predcor2, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC2, main = "Curva ROC - Cumplida")
# Calcular el AUC
auc2 <- performance(predcor2, measure = "auc")
auc2 <- auc2@y.values[[1]]
# Ver el AUC
auc2

# Crear resultados 1 o 0 para la predicción de incumplida
incumplida <- ifelse(citas.test$ESTAFINAL == 3, 1, 0)

# Crear el objeto de predicci?n
predcor3 <- prediction(prednaiveprob[,1], incumplida)

curvaROC3 <- performance(predcor3, measure = "tpr", x.measure = "fpr")
# Curva
plot(curvaROC3, main = "Curva ROC - incumplida")
# Calcular el AUC
auc3 <- performance(predcor3, measure = "auc")
auc3 <- auc3@y.values[[1]]
# Ver el AUC
auc3

# Análisis de cutoffs

# Guardar los recall y puntos de corte
tablafinan <- performance(predcor2, measure = "rec")
cutoffs <- unlist(tablafinan@x.values)
recalls <- unlist(tablafinan@y.values)
# Guardar las precisiones
tablafina2 <- performance(predcor2, measure = "prec")
precisions <- unlist(tablafina2@y.values)
# Guardar F scores
tablafina3 <- performance(predcor2, measure = "f")
fscores <- unlist(tablafina3@y.values)
# Crear la tabla conjunta
tablacruce <- as.data.frame(cbind(cutoffs, precisions, recalls, fscores))
# Ver precisiones versus recall
tablacruce %>% ggplot(aes(x = precisions, y = recalls)) +
  geom_point() +
  theme_minimal()
# Ver el cutoff que maximiza F score
tablacruce %>% ggplot(aes(x = cutoffs, y = fscores)) +
  geom_point() + 
  theme_minimal()

tablacruce[which.max(tablacruce$fscores),]

# Calcular las ganancias
tabla <- table(citas.test$ESTAFINAL)
prop.table(tabla)[3]

tablacruce <- tablacruce %>% 
  mutate(wins = prop.table(tabla)[3]*recalls*(250-(100/precisions)))
# Pintar las ganancias versus los cutoffs
tablacruce %>% ggplot(aes(x = cutoffs, y = wins)) +
  geom_point() +
  theme_minimal()

# Ver las ganancias frente a la precisi?n
tablacruce %>% ggplot(aes(x = precisions, y = wins)) +
  geom_point() +
  theme_minimal()

# Cutoff donde la ganancia es m?xima
tablacruce[which.max(tablacruce$wins),]



#-------------------------------------------------------------------------------------
# Arboles
#-------------------------------------------------------------------------------------

#Convertir a factor
citas2 <- citas %>% 
  mutate_if(is.character, as.factor)

cerealtest <- cerealtest %>% 
  mutate_if(is.character, as.factor)

#Árbol
set.seed(3435)
arbol <- rpart(ESTAFINAL ~ ., data = citas2)
rpart.plot(arbol) 
rpart.rules(arbol)

unique(citas2$ESTAFINAL)

#Resumen del ?rbol
summary(arbol)

#EL ?rbol completo
ELarbol <-  rpart(ESTAFINAL ~ ., data = citas2, cp = 0.001)
rpart.plot(ELarbol)
printcp(ELarbol)


