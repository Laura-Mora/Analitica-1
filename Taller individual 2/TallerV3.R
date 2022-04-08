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
ropa <- read_excel("Datos Taller individual 2 - 2210 (Estudiantes).xlsx")
ropaprueba <- read_excel("Datos Taller individual 2 - 2210 (Estudiantes).xlsx",sheet = "Test")


ropa %>% glimpse()
## Nombres de las columnas
ropa %>% colnames()

## Dimensión del conjunto numero de filas y columnas
dim(ropa)

ropa%>% View()
## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(ropa)

summary(ropa)

#-------------------------------------------------------------------------------------
# Análisis descriptivo
#-------------------------------------------------------------------------------------

## Revisión de las variables escalares
describe(ropa[,2:8,12])

# Cálculo de CV's (coeficiente de variación)
CV <- function(var){(sd(var)/mean(var))*100}
apply(ropa[,2:8,12],2, CV)

#Diagrama de barras exploración  tamamer
ggplot(ropa) +
  geom_bar(mapping = aes(x =  tamamer, colour =  tamamer))

#Diagrama de barras exploración  idmercado
ggplot(ropa) +
  geom_bar(mapping = aes(x =  idmercado, colour =  idmercado))

#Diagrama de barras exploración  idmercado
ggplot(ropa) +
  geom_bar(mapping = aes(x =  promo, colour =  promo))

#boxplot para ver la posible presencia de atípicos
#edadloc
boxedadloc<- ropa%>%ggplot(aes(y=edadloc, colour = edadloc))+
  geom_boxplot()+
  ggtitle("edadloc")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#correo
boxcorreo<- ropa%>%ggplot(aes(y=correo, colour = correo))+
  geom_boxplot()+
  ggtitle("Boxplot correo")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#paginas
boxpaginas<- ropa%>%ggplot(aes(y=paginas, colour = paginas))+
  geom_boxplot()+
  ggtitle("Boxplot paginas")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#telefono
boxtelefono<- ropa%>%ggplot(aes(y=telefono, colour = telefono))+
  geom_boxplot()+
  ggtitle("Boxplot telefono")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#servicio
boxservicio<- ropa%>%ggplot(aes(y=servicio, colour = servicio))+
  geom_boxplot()+
  ggtitle("Boxplot servicio")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#nomina
boxnomina<- ropa%>%ggplot(aes(y=nomina, colour = nomina))+
  geom_boxplot()+
  ggtitle("Boxplot nomina")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#ropamujer
boxropamujer<- ropa%>%ggplot(aes(y=ropamujer, colour = ropamujer))+
  geom_boxplot()+
  ggtitle("Boxplot ropamujer")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))
  
ggarrange(boxedadloc, boxcorreo, boxpaginas, boxtelefono, boxservicio, boxnomina, boxropamujer, nrow = 3, ncol = 3)

rm(boxedadloc, boxcorreo, boxpaginas, boxtelefono, boxservicio, boxnomina, boxropamujer)

# Creamos los histogramas
hist1 <- ropa %>% ggplot(aes(x=edadloc, colour = edadloc)) +
  geom_histogram() +
  ggtitle('Histograma \nedadloc') +
  labs(y = "", x = "edadloc") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist2 <- ropa %>% ggplot(aes(x=correo, colour = correo)) +
  geom_histogram() +
  ggtitle('Histograma \ncorreo') +
  labs(y = "", x = "correo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist3 <- ropa %>% ggplot(aes(x=paginas, colour = paginas)) +
  geom_histogram() +
  ggtitle('Histograma \npaginas') +
  labs(y = "", x = "paginas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist4 <- ropa %>% ggplot(aes(x=telefono, colour = telefono)) +
  geom_histogram() +
  ggtitle('Histograma \ntelefono') +
  labs(y = "", x = "telefono") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist5 <- ropa %>% ggplot(aes(x=servicio, colour = servicio)) +
  geom_histogram() +
  ggtitle('Histograma \nservicio') +
  labs(y = "", x = "servicio") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist6 <- ropa %>% ggplot(aes(x=nomina, colour = nomina)) +
  geom_histogram() +
  ggtitle('Histograma \nnomina') +
  labs(y = "", x = "nomina") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist7 <- ropa %>% ggplot(aes(x=ropamujer, colour = ropamujer)) +
  geom_histogram() +
  ggtitle('Histograma \nropamujer') +
  labs(y = "", x = "ropamujer") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist1, hist2, hist3, hist4, hist5, hist6, hist7, nrow = 3, ncol = 3)

# Limpieza de environment
rm(hist1, hist2, hist3, hist4, hist5, hist6, hist7)

# Relaciones numericas con variable a predecir versión según promo
ggplot(data = ropa,
       aes(y = ropamujer, x = servicio, 
           color = factor (promo))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = nomina, 
           color = factor (promo))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = edadloc, 
           color = factor (promo))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = correo, 
           color = factor (promo))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = paginas, 
           color = factor (promo))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = telefono, 
           color = factor (promo))) +
  geom_point(alpha = 0.5, position = "jitter")

# Relaciones numericas con variable a predecir versión según tamamer
ggplot(data = ropa,
       aes(y = ropamujer, x = servicio, 
           color = factor (tamamer))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = nomina, 
           color = factor (tamamer))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = edadloc, 
           color = factor (tamamer))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = correo, 
           color = factor (tamamer))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = paginas, 
           color = factor (tamamer))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = telefono, 
           color = factor (tamamer))) +
  geom_point(alpha = 0.5, position = "jitter")

# Relaciones numericas con variable a predecir versión según idmercado
ggplot(data = ropa,
       aes(y = ropamujer, x = servicio, 
           color = factor (idmercado))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = nomina, 
           color = factor (idmercado))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = edadloc, 
           color = factor (idmercado))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = correo, 
           color = factor (idmercado))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = paginas, 
           color = factor (idmercado))) +
  geom_point(alpha = 0.5, position = "jitter")

ggplot(data = ropa,
       aes(y = ropamujer, x = telefono, 
           color = factor (idmercado))) +
  geom_point(alpha = 0.5, position = "jitter")

#Resumen 
ggpairs(ropa, alpha=0.5, position= "jitter") 

matrizcor <- cor(ropa[,c(1:9,11:12)])
corrplot(matrizcor, method="square", tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),addCoef.col = "black", 
         number.cex=0.7,type = "upper", diag = FALSE)

#-------------------------------------------------------------------------------------
# Preparación de datos
#-------------------------------------------------------------------------------------

ropa <- ropa %>%
  mutate(peso<-telefono*paginas*correo)

ropaprueba <- ropaprueba %>%
  mutate(peso<-telefono*paginas*correo)
#remover el idloc, nomina, idmercado, promo
ropa <- ropa %>% 
  dplyr::select(-c(idloc, nomina, idmercado, telefono, edadloc,paginas,correo,promo,tamamer))
ropa %>% dim()
ropa %>% View()

#crear variables dummy (binarias)
ropawin <- dummyVars(~.,data=ropa) #dice como estas variables son las que hay que pasar a dummy
ropafin <- as.data.frame(predict(ropawin,newdata=ropa)) #pasa esas variables a dummy
ropafin %>% View()


#Eliminar una categor?a por variable para evitar multicolinealidad (lo mejor si uno es juicioso es quitar la que tenga más 1 pero da la misma con lo que uno quite)
ropafin2 <- ropafin

ropafin2 %>% View()

#veamos la dimension final de la base de datos
ropafin2 %>% dim()

#retiro la variable a predecir
predictores <- ropafin2 %>% 
  dplyr::select(-ropamujer)

#Gráfico de correlaciones
corpred <- cor(predictores)
corrplot(corpred, method="square",tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),
         type = "upper", tl.pos = 'n')

#Filtrando variables altamente correlacionadas
corfil <- predictores %>% 
  correlate(use = "pairwise.complete.obs") %>% 
  shave() %>% 
  stretch(na.rm = TRUE) %>% 
  filter(between(r, 0.6, 1)|between(r,-1,-0.6))
corfil %>% View()

#Componentes principales
componentescar <- prcomp(predictores, center=TRUE, scale.=TRUE)
summary(componentescar)

##Entrenamiento y validaci?n 80-20
set.seed(49584) 
sample <- sample.int(nrow(ropafin2), floor(.8*nrow(ropafin2)))
ropa.train <- ropafin2[sample, ]
ropa.test <- ropafin2[-sample, ]

# haciendo el modelo sencillo y el step
modeloaug <- lm(ropamujer~., data=ropa.train)
modelocarstep <- step(modeloaug, direction="both", trace=0)
summary(modelocarstep)

# Separando predictoras en las bases

predic.train <- ropa.train %>% 
  dplyr::select(-ropamujer) %>% 
  as.matrix()

ropam.train <- ropa.train %>% 
  dplyr::select(ropamujer) %>% 
  as.matrix()

predic.test <- ropa.test %>% 
  dplyr::select(-ropamujer) %>% 
  as.matrix()

ropam.test <- ropa.test %>% 
  dplyr::select(ropamujer) %>% 
  as.matrix()

# Corriendo los modelos de regularizaci?n
fitlasso <- glmnet(predic.train,ropam.train,alpha = 1)
fitridge <- glmnet(predic.train,ropam.train,alpha = 0)

#ver la variacion de los coeficientes con el lambda
plot(fitlasso, xvar="lambda")
plot(fitridge, xvar="lambda")

#------------------------------------
# Validaci?n cruzada
#------------------------------------

#Ridge por default hace 100 lambdas
foundridge<-cv.glmnet(predic.train, 
                      ropam.train,
                      alpha=0,
                      nfolds=5)

# Lambdas
foundridge$lambda

# Errores promedio de validaci?n cruzada RMSE
foundridge$cvm

# Estimaci?n de errores est?ndar de cvm
foundridge$cvsd

# cvm m?nimo
min(foundridge$cvm)

# L?mites
foundridge$cvm[1]+foundridge$cvsd[1]
foundridge$cvm[1]-foundridge$cvsd[1]
foundridge$cvup[1]
foundridge$cvlo[1]

#gr?fico lambda vs mse 
#puntos rojos cvm y les pinta su intervalo
#a la derecha estan los lambdas más grandes entonces esta más sesgado
#primera linea dice donde el lambda es el minimo
#la segunda linea puntuada es el ultimo lambda donde su RMSE promedio sigue estando en el rango del minimo, esta a menos de una desviación estandar del minimo, se recomienda coger este lamba -> tiene la penalidad mas fuerte
plot(foundridge)
abline(h=c(min(foundridge$cvm),foundridge$cvup[which(foundridge$lambda==foundridge$lambda.min)]), 
       lty=c(2,2), col=c("blue", "blue"))

#veo los lambdas m?nimo y 1se
foundridge$lambda.1se #Lambda m?s grande cuyo valor est? dentro de un error estandar 
#desde m?nimo
foundridge$lambda.min

#veamos los logaritmos
log(foundridge$lambda.min)
log(foundridge$lambda.1se)

# Coeficientes para lambda.1se
coef(foundridge,s=foundridge$lambda.1se)

# Lasso
foundlasso <- cv.glmnet(predic.train, 
                        ropam.train,
                        alpha=1,
                        nfolds=5)

#gr?fico lambda vs mse
plot(foundlasso)

#veo los lambdas m?nimo y 1se
foundlasso$lambda.1se
foundlasso$lambda.min

#logaritmos
log(foundlasso$lambda.min)
log(foundlasso$lambda.1se)

#Fila que corresponde al lambda.min
which(foundlasso$lambda==foundlasso$lambda.min)
foundlasso$cvup[which(foundlasso$lambda==foundlasso$lambda.min)]

#Fila que corresponde al lambda.1se
which(foundlasso$lambda==foundlasso$lambda.1se)
foundlasso$cvm[which(foundlasso$lambda==foundlasso$lambda.1se)]

#Cantidad de coeficientes no nulos para cada lambda
foundlasso$nzero[which(foundlasso$lambda==foundlasso$lambda.min)]
foundlasso$nzero[which(foundlasso$lambda==foundlasso$lambda.1se)]

# Coeficientes para lambda.min
coef(fitlasso,s=foundlasso$lambda.min)
# Coeficientes para lambda.1se
coef(fitlasso, s=foundlasso$lambda.1se)

#------------------------------------
# Redes el?sticas
#------------------------------------

# aqu? se hacen redes el?sticas de 0.1 a 0.9
for (i in 1:9){ 
  assign(paste("found", i, sep=""), cv.glmnet(predic.train, ropam.train, nfolds=5, 
                                              alpha=i/10))
}

#obtengo los valores del m?nimo mse

redes <- tibble(
  i = 0:10,
  alfa = i/10,
  min_cvm = c(min(foundridge$cvm), min(found1$cvm), min(found2$cvm),
              min(found3$cvm), min(found4$cvm), min(found5$cvm),
              min(found6$cvm), min(found7$cvm), min(found8$cvm),
              min(found9$cvm), min(foundlasso$cvm))
)

redes


#Corro el modelo con la red de menor cvm
elastic8 <- glmnet(predic.train,ropam.train,alpha = 0.8)
coef(elastic8,s=found8$lambda.min)
coef(elastic8,s=found8$lambda.1se)

# Predicciones sobre el test set
# Red el?stica
pred_red <- predict.glmnet(elastic8, predic.test, s=found8$lambda.min)
pred_red_se <- predict.glmnet(elastic8, predic.test, s=found8$lambda.1se)
# Ridge
pred_ridge <- predict.glmnet(fitridge, predic.test, s=foundridge$lambda.min)
pred_ridge_se <- predict.glmnet(fitridge, predic.test, s=foundridge$lambda.1se)
# Lasso
pred_lasso <- predict.glmnet(fitlasso, predic.test, s=foundlasso$lambda.min)
pred_lasso_se <- predict.glmnet(fitlasso, predic.test, s=foundlasso$lambda.1se)
# Stepwise
pred_step <- predict(modelocarstep,ropa.test)

pred_prueba_pez <- predict(modelocarstep,ropapruebafin2)

# C?lculo  del RMSE para cada modelo
rmseelastic <- rmse(ropam.test[,1],pred_red)
rmseelastic_se <- rmse(ropam.test[,1],pred_red_se)
rmseridge <- rmse(ropam.test[,1],pred_ridge)
rmseridge_se <- rmse(ropam.test[,1],pred_ridge_se)
rmselasso <- rmse(ropam.test[,1],pred_lasso)
rmselasso_se <- rmse(ropam.test[,1],pred_lasso_se)
rmsestep <- rmse(ropam.test[,1],pred_step)

rmseelastic
rmseelastic_se
rmseridge
rmseridge_se
rmselasso
rmselasso_se
rmsestep

1000*(rmselasso_se-rmseridge)
(rmselasso_se-rmseridge/rmseridge)

resultadoeelastic_se <- cbind(ropaprueba,pred_prueba_pez)
resultadoeelastic_se <- resultadoeelastic_se[,-c(2:12)]
resultadoeelastic_se$ropamujer <- resultadoeelastic_se$pred_prueba_pez
resultadoeelastic_se <- resultadoeelastic_se[,-c(2)]
write.table(resultadoeelastic_se,"submissionV2.csv",col.names = T, row.names = F,sep = ',',dec = '.')
