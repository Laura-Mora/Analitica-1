## Lectura del archivo cartera 
## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R

getwd()

## Copiar los archivos descargados en la ruta que surge de correr el 
## comando anterior


cartera<-read.csv("cartera.csv", header=TRUE, sep=";", dec=",")

## Instalación de paquetes
install.packages("readxl")
paquetes<-c("psych","e1071")
install.packages(paquetes)

## Carga de paquetes
library(readxl)

## Lectura del archivo en excel
telco<-read_excel("services2.xls")

## visualización de la parte superior del archivo
head(telco)

## Revisar la estructura del objeto que se tiene y los diferentes 
## tipos de datos mediante el comando str
str(cartera)

## Revisar el tipo de dato de una variable en particular
## mediante el comando class
class(cartera$yearsadress)
class(cartera$retrasos)

## Cambio en el tipo de dato de las variables "yearadress" y "retrasos"
## mediante los comandos as.numeric() y as.factor()
cartera$yearsadress<-as.numeric(cartera$yearsadress)
cartera$retrasos<-as.factor(cartera$retrasos)

#######

## Resumen general de las variables del objeto telco
summary(telco)

## grafico de la variable bill
plot(telco$bill)

## Hacer que R entienda el nombre de las variables de un data frame
## como si fueran objetos separados
attach(telco)

####### Medidas de Tendencia Central

## cálculo del promedio de la variable bill que pertenece al data frame
## telco
mean(bill)

## cálculo de la mediana de la variable bill que pertenece al data frame
## telco
median(bill)

## cálculo del promedio recortado de la variable bill que pertenece 
##al data frame telco  
mean(bill, trim=0.2)

####### Medidas de dispersión

## Cálculo del rango 
range(bill)
## Cálculo de la varianza 
var(bill)
## Cálculo de la desviación estándar 
sd(bill)
## Cálculo del rango intercuartílico
IQR(bill)
## Cálculo del MAD
mad(bill)


## Coeficiente de variación específico para la variable bill
cv=(100*sd(bill))/mean(bill)
cv

## función del coeficiente de variación
CV <- function(var){(sd(var)/mean(var))*100}
CV(bill)


## Realiza el cálculo del CV para cada columna del data frame telco
apply(telco,2,CV)

### Carga del paquete psych
library(psych)
## Uso de la función describe que realiza múltiples descriptivos
describe(telco)

## Revisión del objeto que crea la función describe
telcostat<-describe(telco)
str(telco)

### Adición del CV a los descriptivos previos
cvs<-apply(telco,2,CV)
telcostat2<-cbind(telcostat,cvs)
## Cálculo de la media geometrica de los CV
geometric.mean(telcostat2$cvs)


########

### Revisión de los cuantiles de la variable bill
quantile(bill)
quantile(bill, c(.10,.45,.90))

## Cálculo de los cuantiles para las columnas del data frame telco
cuantiles<-apply(telco,2,quantile)
cuantiles

####### Gráficos univariados

### Histograma
hist(bill)
### Boxplot
boxplot(bill)
### QQ norm
qqnorm(minutes)


## Revisión de la estructura de los datos de gráficos
boxguarda<-boxplot(bill)
str(boxguarda)

## Extracción de los datos atípicos
boxguarda$out

### Descripción de las variables categóricas
table(cartera$tipoips)

### gráfica de la tabla de contingencia
tabla1<-table(cartera$tipoips)
barplot(tabla1, main="Cantidad de IPS", xlab="Tipo de IPS")

### Cálculo de proporción de las frecuencias de la tabla 
prop.table(tabla1)

####### Cruces básicos de variables

plot(minutes, bill)

### boxplot dividido por una variable categórica
boxplot(MESES~retrasos, data=cartera)

###### Cruce de variables categóricas
table(cartera$tipoips, cartera$retrasos)

## Dejar de usar las variables del data frame telco
detach(telco)

## Usar los nombres de las variables del data frame cartera como objetos
attach(cartera)

## Carga de la librería gmodels
install.packages("gmodels")
library("gmodels")
### Una mejor creación de tablas cruzadas
CrossTable(tipoips, retrasos)

######
install.packages("vcd")
library("vcd")

## Gráfico de mosaico para tablas cruzadas
mosaic(~tipoips + retrasos,data=cartera, 
       legend=TRUE, shade=TRUE)

