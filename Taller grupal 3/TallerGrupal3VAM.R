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
if (!require('plyr')) install.packages('plyr')
if (!require('lubridate')) install.packages('lubridate')

#-------------------------------------------------------------------------------------
# Lectura de datos
#-------------------------------------------------------------------------------------
# Lectura de los datos que deben estar en la ruta que genera el siguiente código

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
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
citas$ESTAFINAL<-as.factor(citas$ESTAFINAL)
citas$GENERO<-as.factor(citas$GENERO)
citas$ESPECIALIDAD<-as.factor(citas$ESPECIALIDAD)
citas$TIPO_AFILIACION<-as.factor(citas$TIPO_AFILIACION)
citas<-citas %>% mutate(hora=as.numeric(hour(citas$FECHA_CITA)),dia=weekdays(citas$FECHA_CITA))
citas<-citas %>% mutate(hora=as.numeric(hour(citas$FECHA_CITA)),dia=weekdays(citas$FECHA_CITA))
citas$hora<-as.factor(citas$hora)
citas$dia<-as.factor(citas$dia)

citasprueba$ESTAFINAL<-as.factor(citasprueba$ESTAFINAL)

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


## Histogramas EDAD 

mu <- ddply(citas, "ESTAFINAL", summarise, grp.mean=mean(EDAD))

ggplot(citas, aes(x=EDAD, color=ESTAFINAL))+
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=ESTAFINAL),linetype="dashed")

#Diagrama de barras Por Dia de la Semana 

ggplot(citas) +
  geom_bar(mapping = aes(x = dia, fill =ESTAFINAL ))

#Compartivo 
ggplot(citas, aes(dia, fill=ESTAFINAL))+ 
  geom_bar(position = "fill")+
  labs(x="Dia de la Semana", y="")+
  theme_minimal()

#Diagrama de barras Por Hora de la Semana
ggplot(citas) +
  geom_bar(mapping = aes(x = hora, fill = ESTAFINAL))

#Compartivo 
ggplot(citas, aes(hora, fill=ESTAFINAL))+ 
  geom_bar(position = "fill")+
  labs(x="Dia de la Semana", y="")+
  theme_minimal()


#Diagrama de barras Por Tipo de Afiliacion
ggplot(citas) +
  geom_bar(mapping = aes(x = TIPO_AFILIACION, fill = ESTAFINAL))

#Compartivo 
ggplot(citas, aes(TIPO_AFILIACION, fill=ESTAFINAL))+ 
  geom_bar(position = "fill")+
  labs(x="Tipo de Afiliaci�n", y="")+
  theme_minimal()

#Diagrama de barras Por Especialidad 
ggplot(citas) +
  geom_bar(mapping = aes(x = ESPECIALIDAD , fill = ESTAFINAL))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Compartivo 
ggplot(citas, aes(ESPECIALIDAD, fill=ESTAFINAL))+ 
  geom_bar(position = "fill")+
  labs(x="Epecialidad", y="")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Diagrama de barras Por Genero 
ggplot(citas) +
  geom_bar(mapping = aes(x = GENERO , fill = ESTAFINAL))

#Compartivo 
ggplot(citas, aes(GENERO, fill=ESTAFINAL))+ 
  geom_bar(position = "fill")+
  labs(x="Tipo de Afiliaci�n", y="")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#boxplot para ver la posible presencia de atípicos
#Edad
boxedad<- citas%>%ggplot(aes(y=EDAD, x=ESTAFINAL))+
  geom_boxplot()+
  ggtitle("Boxplot EDAD")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

  
mu <- ddply(citas, "ESTAFINAL", summarise, grp.mean=mean(EDAD))
  
hist1 <- ggplot(citas, aes(x=EDAD, color=ESTAFINAL))+
    geom_histogram(fill="white", alpha=0.5, position="identity")+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=ESTAFINAL),linetype="dashed")+
    ggtitle('Histograma \nEDAD') 
  
ggpubr::ggarrange(boxedad, hist1, nrow = 1, ncol = 2)

# ggplot(data = citas,
#        aes(y = ESTAFINAL, x = EDAD, 
#            color = factor (GENERO))) +
#   geom_point(alpha = 0.5, position = "jitter")
# 
# ggplot(data = citas,
#        aes(y = ESTAFINAL, x = EDAD, 
#            color = factor (TIPO_AFILIACION))) +
#   geom_point(alpha = 0.5, position = "jitter")
# 
# ggplot(data = citas,
#        aes(y = ESTAFINAL, x = GENERO, 
#            color = factor (TIPO_AFILIACION))) +
#   geom_point(alpha = 0.5, position = "jitter")
# 
# ggplot(data = citas,
#        aes(y = ESTAFINAL, x = TIPO_AFILIACION, 
#            color = factor (ESPECIALIDAD))) +
#   geom_point(alpha = 0.5, position = "jitter")
# 
# ggplot(data = citas,
#        aes(y = ESTAFINAL, x = EDAD, 
#            color = factor (ESPECIALIDAD))) +
#   geom_point(alpha = 0.5, position = "jitter")
# 
# ggplot(data = citas,
#        aes(y = ESTAFINAL, x = GENERO, 
#            color = factor (ESPECIALIDAD))) +
#   geom_point(alpha = 0.5, position = "jitter")
# 

###Elimintas FECHA y ID y Agrupa Especialidades empleando la Probabilidad A Priori Condicional Dada Especialidad 
## Alta PA [0.5,1] Media, PA [0.25,0.5) , Baja [0,0.25) 

citas_clean <- citas[,c(1:4,7:9)]

citas_clean <- citas_clean %>% 
  relocate(ESTAFINAL, .after=dia)

citas_clean$ESPECIALIDAD <- as.character(citas_clean$ESPECIALIDAD)


if (!require('stringi')) install.packages('stringi')

citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"RADIOLOGIA","ALTA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"CARDIOLOGIA","ALTA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"ACUPUNTURA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"CIRUGIA MAXILOFACIAL","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"ENDODONCIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"GINECOLOGIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"FONOAUDIOLOGIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"MEDICINA INTERNA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"NUTRICION","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"ORTODONCIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"PEDIATRIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"PERIODONCIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"PSICOLOGIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"CIRUGIA DE LA MANO","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"CIRUGIA GENERAL","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"DERMATOLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"FISIATRIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"FISIOTERAPIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"MEDICINA GENERAL","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"ODONTOLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"OPTOMETRIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"OFTALMOLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"NEUROLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"REHABILITACION ORAL","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"UROLOGIA","MEDIA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"TERAPIA OCUPACIONAL","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"GASTROENTEROLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"OTORRINOLARINGOLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"PSIQUIATRIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"Y OBSTETRICIA","")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"PEDIATRICA","")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"ORTOPEDIA Y TRAUMATOLOGIA","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"BAJA ","BAJA")
citas_clean$ESPECIALIDAD = stri_replace_all_fixed(citas_clean$ESPECIALIDAD,"MEDIA ","MEDIA")
unique(citas_clean$ESPECIALIDAD)

citas_clean$ESPECIALIDAD <- as.factor(citas_clean$ESPECIALIDAD) 
citas_clean<-as.data.frame(citas_clean)

#Diagrama de barras Por Especialidad 
ggplot(citas_clean) +
  geom_bar(mapping = aes(x = ESPECIALIDAD , fill = ESTAFINAL))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Compartivo 
ggplot(citas_clean, aes(ESPECIALIDAD, fill=ESTAFINAL))+ 
  geom_bar(position = "fill")+
  labs(x="Epecialidad", y="")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Crea la base de Train y Test Base
set.seed(4234234)
sample <- sample.int(nrow(citas_clean), 0.3*nrow(citas_clean),replace=FALSE)
citas.testB<- as.data.frame(citas_clean[sample,])
citas.train <- as.data.frame(citas_clean[-sample,])

## Crea las Bases de TEST 

SizeofTestBags=10
set.seed(2415008) 
semillasTest <- sample(0:342412, SizeofTestBags, replace=FALSE)
dataTeSTPBag <-list() ## Crea una Base de TesT

## Crea un Conjunto de Bases de Test con Remplazo
for (i in 1:length(semillasTest)){ 
  set.seed(semillasTest[i])
  sample <- sample.int(nrow(citas.testB), nrow(citasprueba),replace=FALSE)
  dataTeSTPBag[[i]]<- as.data.frame(citas.testB[sample,])
}



###ENTRENAMIENTO DE MODELOS####


MODELOS<- list()
predclass<- list()
predprob <- list()
confM<-list()
f1SCM<-list()
##ARBOL##


MODELOS[[1]] <- rpart(ESTAFINAL ~ ESPECIALIDAD+EDAD+hora+dia, data = citas.train,
                      control = rpart.control(xval = 10, 
                                              cp = 0.0001))

MODELOS[[2]] <- rpart(ESTAFINAL ~ ., data = citas.train,
                      control = rpart.control(xval = 10, 
                                              cp = 0.0001))


MODELOS[[3]] <- rpart(ESTAFINAL ~ ., data = citas.train,
                      control = rpart.control(xval = 10, 
                                              cp = 0.00001))


##Validaci�n 


Nmodels=3
FPM <- array(0,c(Nmodels,SizeofTestBags))
for (j in 1:SizeofTestBags){
  for (i in 1:Nmodels){
    predclass[[i]] <- predict(MODELOS[[i]], dataTeSTPBag[[j]], type = "class")
    perd <- predict(MODELOS[[i]],newdata=dataTeSTPBag[[j]],type="prob")
    confM[[i]] <- confusionMatrix(predclass[[i]],as.factor(dataTeSTPBag[[j]]$ESTAFINAL))
    f1SCM[[i]]<- confM[[i]]$byClass[,7]
    fp<- 0.5*f1SCM[[i]][3]+0.3*f1SCM[[i]][1]+0.2*f1SCM[[i]][2]
    FPM[i,j]<-fp
    print(paste ("El f ponderado del modelo",i,"En el Test",j,"Es=",fp))
  }
}

STADP=matrix(0,Nmodels,4)

for (i in 1:Nmodels){
  STADP[i,1]=mean(FPM[i,])
  STADP[i,2]=sd(FPM[i,])
  STADP[i,3]=min(FPM[i,])
  STADP[i,4]=max(FPM[i,])
}

STADP<-as.data.frame(STADP)
colnames(STADP)<-c("Media","Desviacion","Minimo","Maximo")
view(STADP)
