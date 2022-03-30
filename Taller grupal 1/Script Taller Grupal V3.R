if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('cluster')) install.packages('cluster')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('ggpubr')) install.packages('ggpubr')
if (!require('GGally')) install.packages('GGally')
if (!require('skimr')) install.packages('skimr')
if (!require('esquisse')) install.packages('esquisse')
if (!require('e1071')) install.packages('e1071')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('dplyr')) install.packages('dplyr')
if (!require('gridExtra')) install.packages('gridExtra')
if (!require('reshape2')) install.packages('tidyr')
if (!require('arules')) install.packages('arules')
if (!require('Matrix')) install.packages('Matrix')  
if (!require('forcats')) install.packages('forcats') 
if (!require('Matrix')) install.packages('Matrix')  
if (!require('scales')) install.packages('scales') 
if (!require('treemapify')) install.packages('treemapify')
if (!require('writexl')) install.packages('writexl')

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------ Leer datos ---------------------------------------- #

aisles <- read.csv("aisles.csv", header=TRUE, sep=",", dec=",")
dim(aisles)
str(aisles)

departments <- read.csv("departments.csv", header=TRUE, sep=",", dec=",")
dim(departments)
str(departments)

products <- read.csv("products.csv", header=TRUE, sep=",", dec=",")
dim(products)
str(products)

orders <- read.csv("orders.csv", header=TRUE, sep=",", dec=",")
dim(orders)
#str(orders)

memory.limit()
memory.limit(9999999999)
memory.limit()

order_products__prior <- read.csv("order_products__prior.csv", header=TRUE, sep=",", dec=",")
#str(order_products__prior)

order_products__train <- read.csv("order_products__train.csv", header=TRUE, sep=",", dec=",")
#str(order_products__train)


#----------------------------------------------------------------------------------------

###----------------------- MERGE BASES -----------------------------------------###

# Merge para traer el nombre del pasillo donde esta el producto
aux_prod_aisles <- merge(x=products,y=aisles,by="aisle_id",all.x=TRUE)
rm(aisles,products)
gc()
# Merge para traer el nombre deldepartamento al que pertenece el producto
aux_prod_aisles_dept <- merge(x=aux_prod_aisles,y=departments, by="department_id",all.x=TRUE)
rm(aux_prod_aisles,departments)
gc()
# Union Prior y Train
aux_prod_order <- rbind(order_products__prior[,c(1,2)],order_products__train[,c(1,2)])
rm(order_products__prior,order_products__train)
gc()

# Agregamos el departamento y nombre de producto a las ordenes detalle
detalle_order <-left_join(aux_prod_order,aux_prod_aisles_dept %>% dplyr::select(product_id,department,product_name),by = "product_id")
rm(aux_prod_order)
gc()


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
###----------------------- Analisis Exploratorio -----------------------------------------###

# 1----

# Numero de ordenes por departamento
total_ordenes_depto <- detalle_order [,c(1,3,4)] %>%
  distinct(department,order_id) %>%
  group_by(department) %>% 
  summarise(count=n()) %>%
  rename("Numero_de_Ordenes"="count") %>%
  arrange(desc(Numero_de_Ordenes))
head(total_ordenes_depto)

# Numero de Productos por departamento
total_productos_depto <- aux_prod_aisles_dept %>%
  group_by(department) %>% 
  summarise(n=n()) %>%
  rename("Numero_de_Productos"="n") %>%
  arrange(desc(Numero_de_Productos))
head(total_productos_depto)

# Total ordenes unicas
total_ordenes_unicas <- detalle_order %>%
  distinct(order_id) %>%
  summarise(count=n())
total_ordenes_unicas

# Union info total productos y total ordenes por departamento
total_ordenes_productos_por_depto <- left_join(total_ordenes_depto,total_productos_depto %>% dplyr::select(department,Numero_de_Productos),by = "department")
head(total_ordenes_productos_por_depto)


# Grafica total productos vs total ordenes por departamento
ggp <- ggplot(total_ordenes_productos_por_depto,aes(x=reorder(department,Numero_de_Ordenes))) +
  geom_bar(mapping = aes(y=Numero_de_Ordenes/total_ordenes_unicas$count),stat='identity',fill="blue",width=0.6)+
  geom_bar(mapping = aes(y=Numero_de_Productos/sum(Numero_de_Productos)),stat='identity',fill="red",width=0.3)+
  xlab("departamento")+
  ylab("")+
  coord_flip()+
  theme_minimal(base_size=25)
ggp

tabla_grafica_nprod_nordenes_dept <- total_ordenes_productos_por_depto %>% mutate(PorcProductos=Numero_de_Productos/sum(Numero_de_Productos)) %>% mutate(PorcOrdenes=Numero_de_Ordenes/total_ordenes_unicas$count)
head(tabla_grafica_nprod_nordenes_dept)


# Limpieza de environment
rm(tabla_grafica_nprod_nordenes_dept,ggp,total_ordenes_productos_por_depto,total_ordenes_unicas)
rm(total_ordenes_depto,total_productos_depto)
gc()

#2---------------------------------------------

# Top 10 productos 
total_orders_producto <- detalle_order %>% 
  group_by(product_name,department) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  rename("Numero_de_Ordenes"="count") %>%
  arrange(desc(Numero_de_Ordenes))

# Grafica top 10 productos en mas ordenes.
ggplot(total_orders_producto[1:10,],aes(x=reorder(product_name,Numero_de_Ordenes),fill=department)) +
  geom_bar(mapping = aes(y=Numero_de_Ordenes),stat='identity',width=0.6)+
  xlab("product_name")+
  ylab("")+
  coord_flip()+
  theme_minimal(base_size=35)+
  scale_y_continuous(name="Numero_de_Ordenes", labels = scales::comma)+
  theme(axis.text.y = element_text(face="bold"))

#3--------------------------------------------

# Mejor producto respecto a numero de ordenes, por departamento
mejor_prod_depto <- Reduce(rbind,by(total_orders_producto,total_orders_producto["department"],head,n = 1))

# Grafica producto top por departamento
ggplot(mejor_prod_depto,aes(x=reorder(product_name,Numero_de_Ordenes),fill=department)) +
  geom_bar(mapping = aes(y=Numero_de_Ordenes),stat='identity',width=0.6)+
  xlab("product_name")+
  ylab("")+
  coord_flip()+
  theme_minimal(base_size=25)+
  scale_y_continuous(name="Numero_de_Ordenes", labels = scales::comma)+
  theme(axis.text.y = element_text(face="bold"))

# Limpieza de environment
rm(mejor_prod_depto,total_orders_producto )
gc()

#4--------------------------------------------

#Frecuencia de Ordenes por cliente
total_orders_usuario <- orders %>% 
  group_by(user_id) %>% 
  summarize(count = n()) %>% 
  rename("Numero_de_Ordenes"="count")

# Numero de usuarios que tienen la misma frecuencia de compra
total_orders <- total_orders_usuario %>%
  group_by(Numero_de_Ordenes)

# Grafica usuarios con igual numero de compras
ggplot(total_orders, aes(x = Numero_de_Ordenes)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),colour = 4, fill = "blue", binwidth = 10)+
  stat_bin(aes(y=(..count..)/sum(..count..), label=paste0(round((..count..)/sum(..count..)*100,2),"%")), geom="text",binwidth = 10, vjust=-1.5,size=5) +
  labs(x="Numero de Ordenes", y="Frecuencia")+
  theme_minimal(base_size=25)

# Limpieza de environment
rm(total_orders_usuario,total_orders)
gc()

#5--------------------------------------------

#Frecuencia de Ordenes por dia de la semana
total_orders_dia <- orders %>% 
  group_by(order_dow) %>% 
  summarize(count = n()) %>% 
  rename("Numero_de_Ordenes"="count")

# Tree map ordenes de compra por dia
ggplot(total_orders_dia, aes(area = Numero_de_Ordenes, fill = Numero_de_Ordenes, label= paste(order_dow,"",Numero_de_Ordenes,"Ordenes",sep="\n"))) +
  geom_treemap()+
  geom_treemap_text(colour="white", place="centre")+
  theme(legend.position = "none")

rm(total_orders_dia)
gc()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
###-------------------------- APRIORI DEPARTAMENTOS SIN FILTOS -----------------------------------###

# Paso 1 sacar lista de ordenes con los respectivos departamentos relacionados
prueba <- detalle_order  %>% group_by(order_id,department) %>% group_keys()

# Transformacio en tipo transaccional para dejar un solo registro por orden y una agrupacion de departamentos en esa orden
datos_split_depto <- split(x = prueba$department, f = prueba$order_id)
transacciones_depto <- as(datos_split_depto, Class = "transactions")

# Modelo apriori para departamentos
rules_depto <- apriori(transacciones_depto,parameter = list (supp=0.1,conf=0.5,target="rules"))

summary(rules_depto)

inspect(head(sort(rules_depto,by="lift"),n=15))

# Limpieza de environment
rm(datos_split_depto,transacciones_depto,prueba)
gc()


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
###--------------------------------- PREPARACION DE DATOS -----------------------------###


# Agrupamos todos los productos relacionados con yogurt y los llamamos igual

comodin_yogurt <- filter(aux_prod_aisles_dept,grepl("Yogurt",product_name),
                         department!="personal care",
                         department !="canned goods",
                         department !="other",
                         department !="beverages") %>%  mutate(product_name="Yogurt")

# Reemplazamos los nombres de los porductos en la base principal
detalle_order <- detalle_order %>%
  mutate(detalle_order$product_name <- replace(detalle_order$product_name, detalle_order$product_id %in% comodin_yogurt$product_id, "Yogurt"))

detalle_order<-detalle_order[,-4]

colnames(detalle_order)[4]<-"product_name"

# Limpieza de environment
rm(comodin_yogurt)
gc()

# --------------- ANALISIS YOGURT -------------- #

# Lista de ordenes donde hay productos Yogurt
orders_yogurt <- filter(detalle_order,product_name=="Yogurt") %>% distinct(order_id) %>% group_by(order_id) %>% group_keys()

# Numero total de ordenes donde hay yogurt
orders_yogurt %>% 
  summarize(count_distinct_orders = n_distinct(order_id))

# Detalle lista de ordenes donde se llevo yogurt
detalle_order_yogurt <- subset(detalle_order, order_id %in% orders_yogurt$order_id) %>% group_by(order_id,product_name)%>% group_keys()


# Top 10 productos dentro de la lista de ordenes donde se compro yogurt
total_orders_producto <- detalle_order_yogurt %>% 
  group_by(product_name) %>% 
  summarize(count = n()) %>% 
  top_n(15, wt = count) %>%
  rename("Numero_de_Ordenes"="count") %>%
  arrange(desc(Numero_de_Ordenes))

# Grafica top 10 productos en mas ordenes donde tambie se llevo yogurt.
ggplot(total_orders_producto[1:15,],aes(x=reorder(product_name,Numero_de_Ordenes))) +
  geom_bar(mapping = aes(y=Numero_de_Ordenes),stat='identity',width=0.6,fill="blue")+
  xlab("product_name")+
  ylab("")+
  coord_flip()+
  theme_minimal(base_size=25)+
  scale_y_continuous(name="Numero_de_Ordenes", labels = scales::comma)+
  theme(axis.text.y = element_text(face="bold"))

# Lista de clientes que compran yogurt
merge_client_yogurt <- subset(orders, order_id %in% orders_yogurt$order_id) %>% group_by(order_id,user_id)%>% group_keys()

# Total clientes que compran yogurt
merge_client_yogurt %>% 
  summarize(count_distinct_users = n_distinct(user_id))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
###-------------------------- APRIORI PRODUCTOS ---------------------------------------###


### Paso 1 sacar lista de ordenes que tengan productos de los departamento de interes ###


# Seleccionar todas las ordenes que tengan o productos de los departamentos produce,frozen,dairy,pantry
ordenes_aux <- detalle_order%>%
  filter(department=="produce" | department=="frozen"| department=="dairy eggs" | department=="pantry") %>%
  group_by(order_id,product_name,department) %>%
  group_keys()

ordenes_aux <- unite(ordenes_aux,Orden_Producto,c(2:3), sep = " - ", remove = TRUE)

# Paso 2 Transformacio en tipo transaccional para dejar un solo registro por orden y una agrupacion de productos en esa orden
datos_split <- split(x = ordenes_aux$Orden_Producto, f = ordenes_aux$order_id)

transacciones <- as(datos_split, Class = "transactions")

str(transacciones)


# Paso 3 Ejecutamos apriori
rules2 <- apriori(data=transacciones,parameter = list (support=0.001,confidence=0.3,target="rules"))

inspect(head(sort(rules2,by="lift"),n=50))


# Filtramos las reglas para solo ver las que tengan yogurt en consecuente

itemsets_filtrado1 <- arules::subset(rules2,
                                     subset = rhs %pin% "Yogurt")
itemsets_filtrado1

inspect(head(sort(itemsets_filtrado1,by="lift"),n=15))


df_reglas <- as(itemsets_filtrado1, Class = "data.frame")

rm(rules2)
rm(transacciones,datos_split,filtrado1_ordenes)
gc()



