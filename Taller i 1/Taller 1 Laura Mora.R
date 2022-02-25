if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('cluster')) install.packages('cluster') #cluster
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('ggpubr')) install.packages('ggpubr') #gráficos centros cluster
if (!require('GGally')) install.packages('GGally') #gráficos centros cluster

# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
banco <- read_excel("infoclientebanca.xlsx")
banco %>% glimpse()

## Dimensión del conjunto numero de filas y columnas
dim(banco)

## Nombres de las columnas
colnames(banco)

## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(banco)

summary(banco)
#-------------------------------------------------------------------------------------
# Análisis descriptivo
#-------------------------------------------------------------------------------------

## Revisión de las variables escalares
describe(banco[,3:25])

# Cálculo de CV's (coeficiente de variación)
CV <- function(var){(sd(var)/mean(var))*100}
apply(banco[,3:25],2, CV)

#Diagrama de barras exploración grupo_de_cliente
ggplot(banco) +
  geom_bar(mapping = aes(x = grupo_de_cliente, colour = grupo_de_cliente))

#Diagrama de barras exploración Sitio_consumo_masfrecuente, no tan útil hay demasiados datos diferentes
ggplot(banco) +
  geom_bar(mapping = aes(x = Sitio_consumo_masfrecuente))

#boxplot para ver la posible presencia de atípicos
#Número de transacciones
boxnumt<- banco%>%ggplot(aes(y=Numero_de_transacciones, colour = Numero_de_transacciones))+
  geom_boxplot()+
  ggtitle("Boxplot Numer_de_transacciones")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#Promedio por transacción
boxpromt<- banco%>%ggplot(aes(y=promedio_por_transaccion, colour = promedio_por_transaccion))+
  geom_boxplot()+
  ggtitle("Boxplot promedio_por_transaccion")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#Transacción minima
boxtmin<- banco%>%ggplot(aes(y=transaccion_minima, colour = transaccion_minima))+
  geom_boxplot()+
  ggtitle("Boxplot transaccion_minima")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#Transacción máxima
boxtmax<- banco%>%ggplot(aes(y=transaccion_maxima, colour = transaccion_maxima))+
  geom_boxplot()+
  ggtitle("Boxplot transaccion_maxima")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))
  
#Desviación estandar por transacción
boxdet<- banco%>%ggplot(aes(y=desviacion_estandar_por_transaccion, colour = desviacion_estandar_por_transaccion))+
  geom_boxplot()+
  ggtitle("Boxplot desviacion_estandar_por_transaccion")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))
  
ggarrange(boxnumt, boxpromt, boxtmin, boxtmax, boxdet, nrow = 2, ncol = 3)

#porcentaje_visa_nacional
boxvn<- banco%>%ggplot(aes(y=porcentaje_visa_nacional, colour = porcentaje_visa_nacional))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_visa_nacional")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_visa_internacional
boxvi<- banco%>%ggplot(aes(y=porcentaje_visa_internacional, colour = porcentaje_visa_internacional))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_visa_internacional")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_mastercard_nacional
boxmn<- banco%>%ggplot(aes(y=porcentaje_mastercard_nacional, colour = porcentaje_mastercard_nacional))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_mastercard_nacional")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_mastercard_internacional
boxmai<- banco%>%ggplot(aes(y=porcentaje_mastercard_internacional, colour = porcentaje_mastercard_internacional))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_mastercard_internacional")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#Porcentaje_otrafranquicia_nacional
boxofn<- banco%>%ggplot( aes(y=Porcentaje_otrafranquicia_nacional, colour = Porcentaje_otrafranquicia_nacional))+
  geom_boxplot()+
  ggtitle("Boxplot Porcentaje_otrafranquicia_nacional")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_otrafranquicia_internacional
boxofi<- banco%>%ggplot(aes(y=porcentaje_otrafranquicia_internacional, colour = porcentaje_otrafranquicia_internacional))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_otrafranquicia_internacional")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_nacional_total
boxnt<- banco%>%ggplot( aes(y=porcentaje_nacional_total, colour = porcentaje_nacional_total))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_nacional_total")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_internacional_total
boxit<- banco%>%ggplot(aes(y=porcentaje_internacional_total, colour = porcentaje_internacional_total))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_internacional_total")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(boxvn, boxvi, boxmn, boxmai, boxofn, boxofi, boxnt, boxit, nrow = 3, ncol = 3)

#porcentaje_manana
boxmanana<- banco%>%ggplot(aes(y=porcentaje_manana, colour = porcentaje_manana))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_manana")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_tarde
boxtarde<- banco%>%ggplot(aes(y=porcentaje_tarde, colour = porcentaje_tarde))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_tarde")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))
  
#pocentaje_noche
boxnoche<- banco%>%ggplot(aes(y=porcentaje_noche, colour = porcentaje_noche))+
  geom_boxplot()+
  ggtitle("Boxplot porcentaje_noche")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(boxnoche, boxtarde, boxmanana, nrow = 1, ncol = 3)

#porcDOMINGO
boxdomi<- banco%>%ggplot(aes(y=porcDOMINGO, colour = porcDOMINGO))+
  geom_boxplot()+
  ggtitle("Boxplot DOMINGO")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcLUNES
boxlun<- banco%>%ggplot(aes(y=porcLUNES, colour = porcLUNES))+
  geom_boxplot()+
  ggtitle("Boxplot LUNES")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcMARTES
boxmar<- banco%>%ggplot(aes(y=porcMARTES, colour = porcMARTES))+
  geom_boxplot()+
  ggtitle("Boxplot MARTES")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcMIERCOLES
boxmier<- banco%>%ggplot(aes(y=porcMIERCOLES, colour = porcMIERCOLES))+
  geom_boxplot()+
  ggtitle("Boxplot MIERCOLES")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcJUEVES
boxjuev<- banco%>%ggplot( aes(y=porcJUEVES, colour = porcJUEVES))+
  geom_boxplot()+
  ggtitle("Boxplot JUEVES")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcVIERNES
boxvier<- banco %>%ggplot(aes(y=porcVIERNES, colour = porcVIERNES))+
  geom_boxplot()+
  ggtitle("Boxplot VIERNES")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcSABADO
boxSab<- banco %>% ggplot(aes(y=porcSABADO, colour = porcSABADO))+
  geom_boxplot()+
  ggtitle("Boxplot SABADO")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(boxdomi, boxlun, boxmar, boxmier, boxjuev, boxvier, boxSab, nrow = 3, ncol = 3)

# Creamos los histogramas
hist1 <- banco %>% ggplot(aes(x=Numero_de_transacciones, colour = Numero_de_transacciones)) +
  geom_histogram() +
  ggtitle('Histograma \nNumero_de_transacciones') +
  labs(y = "", x = "Numero_de_transacciones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist2 <- banco %>% ggplot(aes(x=promedio_por_transaccion, colour = promedio_por_transaccion)) +
  geom_histogram() +
  ggtitle('Histograma \npromedio_por_transaccion') +
  labs(y = "", x = "promedio_por_transaccion") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist3 <- banco %>% ggplot(aes(x=transaccion_minima, colour = transaccion_minima)) +
  geom_histogram() +
  ggtitle('Histograma \ntransaccion_minima') +
  labs(y = "", x = "transaccion_minima") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist4 <- banco %>% ggplot(aes(x=transaccion_maxima, colour = transaccion_maxima)) +
  geom_histogram() +
  ggtitle('Histograma \ntransaccion_maxima') +
  labs(y = "", x = "transaccion_maxima") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist5 <- banco %>% ggplot(aes(x=desviacion_estandar_por_transaccion, colour = desviacion_estandar_por_transaccion)) +
  geom_histogram() +
  ggtitle('Histograma \ndesviacion_estandar_por_transaccion') +
  labs(y = "", x = "desviacion_estandar_por_transaccion") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist1, hist2, hist3, hist4, hist5, nrow = 2, ncol = 3)

# Creamos los histogramas
hist6 <- banco %>% ggplot(aes(x=porcentaje_visa_nacional)) +
  geom_histogram() +
  ggtitle('Histograma \n % visa_nacional') +
  labs(y = "", x = " % visa_nacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist7 <- banco %>% ggplot(aes(x=porcentaje_visa_internacional)) +
  geom_histogram() +
  ggtitle('Histograma \n%_visa_internacional') +
  labs(y = "", x = "%visa_internacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist8 <- banco %>% ggplot(aes(x=porcentaje_visa_internacional)) +
  geom_histogram() +
  ggtitle('Histograma \n%visa_internacional') +
  labs(y = "", x = "%visa_internacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist9 <- banco %>% ggplot(aes(x=porcentaje_mastercard_internacional)) +
  geom_histogram() +
  ggtitle('Histograma \n%mastercard_internacional') +
  labs(y = "", x = "%mastercard_internacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist10 <- banco %>% ggplot(aes(x=Porcentaje_otrafranquicia_nacional)) +
  geom_histogram() +
  ggtitle('Histograma \n%_otrafranquicia_nacional') +
  labs(y = "", x = "% otrafranquicia_nacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist11 <- banco %>% ggplot(aes(x=porcentaje_otrafranquicia_internacional)) +
  geom_histogram() +
  ggtitle('Histograma \n%_otrafranquicia_internacional') +
  labs(y = "", x = "% otrafranquicia_internacional") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist6, hist7, hist8, hist9, hist10, hist11, nrow = 2, ncol = 3)

# Creamos los histogramas
hist12 <- banco %>% ggplot(aes(x=porcentaje_nacional_total)) +
  geom_histogram() +
  ggtitle('Histograma \n%_nacional_total') +
  labs(y = "", x = "porcentaje_nacional_total") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist13 <- banco %>% ggplot(aes(x=porcentaje_internacional_total)) +
  geom_histogram() +
  ggtitle('Histograma \n%_internacional_total') +
  labs(y = "", x = "%_internacional_total") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist14 <- banco %>% ggplot(aes(x=porcentaje_manana)) +
  geom_histogram() +
  ggtitle('Histograma \n%_manana') +
  labs(y = "", x = "%_manana") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist15 <- banco %>% ggplot(aes(x=porcentaje_tarde)) +
  geom_histogram() +
  ggtitle('Histograma \n%_tarde') +
  labs(y = "", x = "%_tarde") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist16 <- banco %>% ggplot(aes(x=porcentaje_noche)) +
  geom_histogram() +
  ggtitle('Histograma \n%_noche') +
  labs(y = "", x = "%_noche") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist12, hist13, hist14, hist15, hist16, nrow = 2, ncol = 3)

# Creamos los histogramas
hist17 <- banco %>% ggplot(aes(x=porcDOMINGO)) +
  geom_histogram() +
  ggtitle('Histograma \n% domingo') +
  labs(y = "", x = "porcDOMINGO") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist18 <- banco %>% ggplot(aes(x=porcLUNES)) +
  geom_histogram() +
  ggtitle('Histograma \n% lunes') +
  labs(y = "", x = "porcLUNES") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist19 <- banco %>% ggplot(aes(x=porcMARTES)) +
  geom_histogram() +
  ggtitle('Histograma \n% martes') +
  labs(y = "", x = "% martes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist20 <- banco %>% ggplot(aes(x=porcMIERCOLES)) +
  geom_histogram() +
  ggtitle('Histograma \n%miercoles') +
  labs(y = "", x = "%miercoles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist21 <- banco %>% ggplot(aes(x=porcJUEVES)) +
  geom_histogram() +
  ggtitle('Histograma \n%jueves') +
  labs(y = "", x = "%jueves") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist22 <- banco %>% ggplot(aes(x=porcVIERNES)) +
  geom_histogram() +
  ggtitle('Histograma \n%viernes') +
  labs(y = "", x = "%viernes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist23 <- banco %>% ggplot(aes(x=porcSABADO)) +
  geom_histogram() +
  ggtitle('Histograma \n%sabado') +
  labs(y = "", x = "%sabado") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist17, hist18, hist19, hist20, hist21, hist22, hist23, nrow = 3, ncol = 3)

#-------------------------------------------------------------------------------------
# Preparación de datos
#-------------------------------------------------------------------------------------
#TODO: YA pasar a una escala decente las cuatro variables de transacciones, revisar kurtosis y eso de estas mismas variables y adaptarlas
#TODO: YA revisar kurtosis de las de porcentajes y si es necesario adaptarlas
#TODO: YA 4 data sets para pasar a clustering, los 4 deben tener las variables de transacciones, uno con todas las variables, otro con los porcentajes de franquicias, otro con los porcentajes en horarios y el otro con los porcentajes diarios
#TODO: YA Hacer las gráficas de el dataset mixto resultados 
#TODO: YA sumar algunos porcentajes, fines de semana, entre horarios y así, para dejar un data set más chiquito

#Crear atributos
#Porcentaje de fines de semana viernes, sabado, domingo
banco <- banco %>% 
  mutate(porcFinde=porcVIERNES+porcSABADO)
#Porcentaje horarios mañana, tarde
banco <- banco %>% 
  mutate(porcHorario=porcentaje_noche+porcentaje_manana)

top_5<-(banco %>% count(banco$Sitio_consumo_masfrecuente, sort= TRUE))[6:109,]
colnames(top_5)[1]<-"sitio"
banco <- banco %>%                              
  mutate(banco$Sitio_consumo_masfrecuente_new <- replace(banco$Sitio_consumo_masfrecuente, banco$Sitio_consumo_masfrecuente %in% top_5$sitio, "Otro"))
colnames(banco)[29]<-"sitiosNuevos"

#reducción de atípicos
##obtener las variables a reducir
#variables a redución transacción
minibancoT <- as.data.frame(banco[,c(3:7)])
#variables a arreglar según franquicia
minibancoF <- as.data.frame(banco[,c(8:15)])
#variables a arreglar según horario
minibancoH <- as.data.frame(banco[,c(16:18)])
#variables a arreglar según día
minibancoD <- as.data.frame(banco[,c(19:24)])
#variables a arreglar mixto
minibancoMix <- as.data.frame(banco[,c(3:4,14,27:28)])

#obtener el logaritmo de 1+x

microbancoT <- apply(minibancoT,2,log1p)
microbancoF <- apply(minibancoF,2,log1p)
microbancoH <- apply(minibancoH,2,log1p)
microbancoD <- apply(minibancoD,2,log1p)
microbancoMix <- apply(minibancoMix,2,log1p)

#ensamble final
bancocuasifin <- as.data.frame(cbind(microbancoT,microbancoF, microbancoH, microbancoD))
bancocuasifinTF <- as.data.frame(cbind(microbancoT,microbancoF))
bancocuasifinTH <- as.data.frame(cbind(microbancoT,microbancoH))
bancocuasifinTD <- as.data.frame(cbind(microbancoT,microbancoD))

#estandarizar
bancofin <- as.data.frame(scale(bancocuasifin))
bancofinTF <- as.data.frame(scale(bancocuasifinTF))
bancofinTH <- as.data.frame(scale(bancocuasifinTH))
bancofinTD <- as.data.frame(scale(bancocuasifinTD))
bancofinMix <- as.data.frame(scale(microbancoMix))

#TODO: gráficas boxplot e histogramas de los últimos ensambles en el bancoMix porque es el que se presenta
#Numero_de_transacciones
boxmix<- bancofinMix%>%ggplot(aes(y=Numero_de_transacciones, colour = Numero_de_transacciones))+
  geom_boxplot()+
  ggtitle("Boxplot Num transacciones")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#promedio_por_transaccion  
boxmix2<- bancofinMix%>%ggplot(aes(y=promedio_por_transaccion, colour = promedio_por_transaccion))+
  geom_boxplot()+
  ggtitle("Boxplot Prom transacciones")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcentaje_nacional_total 
boxmix3<- bancofinMix%>%ggplot(aes(y=porcentaje_nacional_total , colour = porcentaje_nacional_total))+
  geom_boxplot()+
  ggtitle("Boxplot % nacional total")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))


#porcFinde
boxmix5<- bancofinMix%>%ggplot(aes(y=porcFinde, colour = porcFinde))+
  geom_boxplot()+
  ggtitle("Boxplot % viernes + sabado")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

#porcHorario
boxmix6<- bancofinMix%>%ggplot(aes(y=porcHorario, colour = porcHorario))+
  geom_boxplot()+
  ggtitle("Boxplot % mañana y noche")+
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(boxmix, boxmix2, boxmix3, boxmix5, boxmix6, nrow = 2, ncol = 3)

#revisar descriptivos

describe(bancofin)
describe(bancofinTF)
describe(bancofinTH)
describe(bancofinTD)
describe(bancofinMix)

#y los histogramas

histo1 <- bancofinMix %>% ggplot(aes(x=porcentaje_nacional_total)) +
  geom_histogram() +
  ggtitle('Histograma \nporcentaje nacional total') +
  labs(y = "", x = "porcentaje nacionaltotal") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo2 <- bancofinMix %>% ggplot(aes(x=promedio_por_transaccion)) +
  geom_histogram() +
  ggtitle('Histograma \npromedio por transacción') +
  labs(y = "", x = "promedio por transacción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo3 <- bancofinMix %>% ggplot(aes(x=Numero_de_transacciones)) +
  geom_histogram() +
  ggtitle('Histograma \nNúmero de transacciones') +
  labs(y = "", x = "Número de transacciones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


histo5 <- bancofinMix %>% ggplot(aes(x=porcFinde)) +
  geom_histogram() +
  ggtitle('Histograma \n porcFinde') +
  labs(y = "", x = "porcFinde") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

histo6 <- bancofinMix %>% ggplot(aes(x=porcHorario)) +
  geom_histogram() +
  ggtitle('Histograma \n porcHorario') +
  labs(y = "", x = "porcHorario") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(histo1, histo2, histo3, histo5, histo6, nrow = 2, ncol = 3)



#-------------------------------------------------------------------------------------
# Modelación
#-------------------------------------------------------------------------------------

### Escoger el número de clústeres

#El data set con todos los atributos -> CABUM con TF-> CABUM, hace CABUM con todas las mezclas pensadas
#lo puedo dejar solo con el codo y basado en eso elegir el número de clusters a usar porque los otros no corren por la cantidad de datos que hay 
#Para quedarme con cual idea usar de los 4 diseños para clusters basarme en el contexto en el que se esta trabajndo y los objetivos ya planteados 

#Clusters con TODOS LOS DATOS bancofin
#utilizo una semilla para replicar resultados
set.seed(5935)
#cálculo la suma de cuadrados total
wss <- (nrow(bancofin)-1)*sum(apply(bancofin,2,var))
#cálculo para cada solución de clustering 
for (i in 2:15) wss[i] <- sum(kmeans(bancofin,
                                     centers=i, nstart=10)$withinss)
# Gráfico de codo
# Con ggplot
sumas <- as.data.frame(cbind(wss, k = seq(1,15, by=1)))

sumas %>% ggplot(aes(x=k, y=wss)) +
  geom_point() + 
  geom_line() +
  labs(x = "Número de clústeres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo BancoFin") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Clusters con solo transacciones y franquicias  bancofinTF
#utilizo una semilla para replicar resultados
set.seed(5935)
#cálculo la suma de cuadrados total
wssTF <- (nrow(bancofinTF)-1)*sum(apply(bancofinTF,2,var))
#cálculo para cada solución de clustering 
for (i in 2:15) wssTF[i] <- sum(kmeans(bancofinTF,
                                     centers=i, nstart=10)$withinss)
# Gráfico de codo
# Con ggplot
sumasTF <- as.data.frame(cbind(wssTF, k = seq(1,15, by=1)))

sumasTF %>% ggplot(aes(x=k, y=wssTF)) +
  geom_point() + 
  geom_line() +
  labs(x = "Número de clústeres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo BancoFin transacciones franquicias") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Clusters con solo transacciones y horarios bancofinTH
#utilizo una semilla para replicar resultados
set.seed(5935)
#cálculo la suma de cuadrados total
wssTH <- (nrow(bancofinTH)-1)*sum(apply(bancofinTH,2,var))
#cálculo para cada solución de clustering 
for (i in 2:15) wssTH[i] <- sum(kmeans(bancofinTH,
                                       centers=i, nstart=10)$withinss)
# Gráfico de codo
# Con ggplot
sumasTH <- as.data.frame(cbind(wssTH, k = seq(1,15, by=1)))

sumasTH %>% ggplot(aes(x=k, y=wssTH)) +
  geom_point() + 
  geom_line() +
  labs(x = "Número de clústeres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo BancoFin transacciones horarios") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Clusters con solo transacciones y días
#utilizo una semilla para replicar resultados
set.seed(5935)
#cálculo la suma de cuadrados total
wssTD <- (nrow(bancofinTD)-1)*sum(apply(bancofinTD,2,var))
#cálculo para cada soluci?n de clustering 
for (i in 2:15) wssTD[i] <- sum(kmeans(bancofinTD,
                                       centers=i, nstart=10)$withinss)
# Gráfico de codo
# Con ggplot
sumasTD <- as.data.frame(cbind(wssTD, k = seq(1,15, by=1)))

sumasTD %>% ggplot(aes(x=k, y=wssTD)) +
  geom_point() + 
  geom_line() +
  labs(x = "N?mero de cl?steres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo BancoFin transacciones días") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Clusters con atributos mixtos 
#utilizo una semilla para replicar resultados
set.seed(5935)
#cálculo la suma de cuadrados total
wssMix <- (nrow(bancofinMix)-1)*sum(apply(bancofinMix,2,var))
#cálculo para cada solución de clustering 
for (i in 2:15) wssMix[i] <- sum(kmeans(bancofinMix,
                                       centers=i, nstart=10)$withinss)
# Gráfico de codo
# Con ggplot
sumasMix <- as.data.frame(cbind(wssMix, k = seq(1,15, by=1)))

sumasMix %>% ggplot(aes(x=k, y=wssMix)) +
  geom_point() + 
  geom_line() +
  labs(x = "N?mero de cl?steres", y = "Suma de cuadrados within") +
  ggtitle("Diagrama de codo BancoFinmix") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### ---------------Ejecución--------------

#Clusters con TODOS LOS DATOS bancofin
#ejecución de k-means
set.seed(45390)
cellcluster<-kmeans(bancofin,centers=9,nstart=10,iter.max=20)
#tamaño de grupos
cellcluster$size
#número de iteraciones
cellcluster$iter
#centros de grupos
cellcluster$centers

#Gráfico de calor de centros
#Mapa de calor
centrosg <- as.data.frame(cellcluster$centers)
centrosg$grupo <- as.factor(rownames(centrosg))
centrosheat <- reshape2::melt(centrosg) #quiero que me traiga melt del paquete melt
colnames(centrosheat) <- c("grupo","variable","centroide")
centrosheat %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

# Gráfico de perfiles de centros
grupos <- c(1,2,3,4,5,6,7,8,9)

centros <- as.data.frame(cbind(grupos, cellcluster$centers))
centros$grupos <- as.factor(centros$grupos)

ggparcoord(centros, columns = 2:22, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gr?fico de centroides")+
  labs(y="")

#Clusters con solo transacciones y franquicias  bancofinTF
#ejecución de k-means
set.seed(45390)
cellclusterTF<-kmeans(bancofinTF,centers=3,nstart=10,iter.max=20)
#tamaño de grupos
cellclusterTF$size
#número de iteraciones
cellclusterTF$iter
#centros de grupos
cellclusterTF$centers

#Gráfico de calor de centros
#Mapa de calor
centrosgTF <- as.data.frame(cellclusterTF$centers)
centrosgTF$grupo <- as.factor(rownames(centrosgTF))
centrosheatTF <- reshape2::melt(centrosgTF) #quiero que me traiga melt del paquete melt
colnames(centrosheatTF) <- c("grupo","variable","centroide")
centrosheatTF %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

# Gráfico de perfiles de centros
gruposTF <- c(1,2,3)

centrosTF <- as.data.frame(cbind(grupos, cellclusterTF$centers))
centrosTF$grupos <- as.factor(centrosTF$grupos)

ggparcoord(centrosTF, columns = 2:13, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gr?fico de centroides")+
  labs(y="")

#Clusters con solo transacciones y horarios bancofinTH
#ejecución de k-means
set.seed(45390)
cellclusterTH<-kmeans(bancofinTH,centers=4,nstart=10,iter.max=20)
#tamaño de grupos
cellclusterTH$size
#número de iteraciones
cellclusterTH$iter
#centros de grupos
cellclusterTH$centers

#Gráfico de calor de centros
#Mapa de calor
centrosgTH <- as.data.frame(cellclusterTH$centers)
centrosgTH$grupo <- as.factor(rownames(centrosgTH))
centrosheatTH <- reshape2::melt(centrosgTH) #quiero que me traiga melt del paquete melt
colnames(centrosheatTH) <- c("grupo","variable","centroide")
centrosheatTH %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

# Gráfico de perfiles de centros
gruposTH <- c(1,2,3,4)

centrosTH <- as.data.frame(cbind(grupos, cellclusterTH$centers))
centrosTH$grupos <- as.factor(centrosTH$grupos)

ggparcoord(centrosTH, columns = 2:7, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gr?fico de centroides")+
  labs(y="")

#Clusters con solo transacciones y días bancofinTD
#ejecución de k-means
set.seed(45390)
cellclusterTD<-kmeans(bancofinTD,centers=5,nstart=10,iter.max=20)
#tamaño de grupos
cellclusterTD$size
#número de iteraciones
cellclusterTD$iter
#centros de grupos
cellclusterTD$centers

#Gráfico de calor de centros
#Mapa de calor
centrosgTD <- as.data.frame(cellclusterTD$centers)
centrosgTD$grupo <- as.factor(rownames(centrosgTD))
centrosheatTD <- reshape2::melt(centrosgTD) #quiero que me traiga melt del paquete melt
colnames(centrosheatTD) <- c("grupo","variable","centroide")
centrosheatTD %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

# Gráfico de perfiles de centros
gruposTD <- c(1,2,3,4,5)

centrosTD <- as.data.frame(cbind(grupos, cellclusterTD$centers))
centrosTD$grupos <- as.factor(centrosTD$grupos)

ggparcoord(centrosTD, columns = 2:7, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gráfico de centroides")+
  labs(y="")

#Clusters con solo transacciones y días bancofinMix
#ejecución de k-means
set.seed(45390)
cellclusterMix<-kmeans(bancofinMix,centers=4,nstart=10,iter.max=20)
#tamañoo de grupos
cellclusterMix$size
#número de iteraciones
cellclusterMix$iter
#centros de grupos
cellclusterMix$centers

#Gráfico de calor de centros
#Mapa de calor
centrosgMix <- as.data.frame(cellclusterMix$centers)
centrosgMix$grupo <- as.factor(rownames(centrosgMix))
centrosheatMix <- reshape2::melt(centrosgMix) #quiero que me traiga melt del paquete melt
colnames(centrosheatMix) <- c("grupo","variable","centroide")
centrosheatMix %>% 
  ggplot(aes(x=grupo,y=variable,fill=centroide, label=sprintf("%0.2f", round(centroide, digits=2))))+
  geom_tile()+
  scale_fill_distiller(palette="RdBu")+
  geom_text()

# Gráfico de perfiles de centros
gruposMix <- c(1,2,3,4)

centrosMix <- as.data.frame(cbind(grupos, cellclusterMix$centers))
centrosMix$grupos <- as.factor(centrosTD$grupos)

ggparcoord(centrosMix, columns = 2:6, groupColumn = 1, scale = "globalminmax")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Gráfico de centroides")+
  labs(y="")

#------------------------------------------------------------------------------------
# Perfilamiento
#------------------------------------------------------------------------------------

#guardar el cluster de pertenencia
banco$grupo <- cellclusterMix$cluster

#ggplot(data = banco) +
#  geom_mosaic(aes(x = product(grupo_de_cliente), fill = grupo))
mosaic(~grupo + grupo_de_cliente ,data=banco, 
       legend=TRUE, shade=TRUE)

mosaic(~grupo + sitiosNuevos, data = banco,
       legend=TRUE, shade=TRUE,labeling = labeling_list)

#Descriptivos segmentados
banco %>% 
  group_by(grupo) %>% 
  summarise(numTransacciones_medio=mean(Numero_de_transacciones),
            desv_numTransacciones = sd(Numero_de_transacciones),
            promTransaccion_media = mean(promedio_por_transaccion),
            desv_promTransaccion= sd(promedio_por_transaccion))

ggplot(banco) +
  geom_bar(mapping = aes(x = grupo, colour = grupo))

#validar resultados- consistencia
kclusters <- clusterboot(bancofinMix,B=10,clustermethod=kmeansCBI,k=4,seed=4)
#la validación del resultado. >0.75 o .85 muy bueno; <.6 malo
kclusters$bootmean

