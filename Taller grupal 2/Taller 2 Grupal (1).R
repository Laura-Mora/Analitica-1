if (!require('readxl')) install.packages('readxl')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('MLmetrics')) install.packages('MLmetrics')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('caret')) install.packages('caret')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('ROCR')) install.packages('ROCR')
if (!require('psych')) install.packages('psych')
if (!require('lift')) install.packages('lift')
if (!require('fastDummies')) install.packages('fastDummies')
if (!require('ROSE')) install.packages('ROSE')
if (!require('performanceEstimation')) install.packages('performanceEstimation')
if (!require('glmnet')) install.packages('glmnet')

#install.packages('devtools')
devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
if (!require('catboost')) install.packages('catboost')

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
train <- read_excel("traintelco.xlsx")
test <- read_excel("testelco.xlsx")
train %>% glimpse()


train2 <- train %>%
  select(-resultado)

balance = table(train$resultado)
prop.table(balance)

base_completa <- rbind(train2, test)


#Definir nombres columnas

colnames(train) <- c('id', 
                     'fecha_nacimiento', 
                     'tipo_cliente', 
                     'factura_online', 
                     'antiguedad_equipo', 
                     'plan_datos', 
                     'facturacion', 
                     'mora', 
                     'fecha_inicio_contrato',
                     'minutos',
                     'resultado')

colnames(train2) <- c('id', 
                     'fecha_nacimiento', 
                     'tipo_cliente', 
                     'factura_online', 
                     'antiguedad_equipo', 
                     'plan_datos', 
                     'facturacion', 
                     'mora', 
                     'fecha_inicio_contrato',
                     'minutos',
                     'resultado')


colnames(test) <- c('id', 
                     'fecha_nacimiento', 
                     'tipo_cliente', 
                     'factura_online', 
                     'antiguedad_equipo', 
                     'plan_datos', 
                     'facturacion', 
                     'mora', 
                     'fecha_inicio_contrato',
                     'minutos',
                     'resultado')

escalares <- train %>%
  select(c(antiguedad_equipo, facturacion, mora, minutos))
describe(escalares)

escalares_2 <- train2 %>%
  select(c(antiguedad_equipo, facturacion, mora, minutos))
describe(escalares_2)

#Definir variables categoricas y dummies

train <- transform(train, tipo_cliente = as.character(tipo_cliente))
train <- transform(train, factura_online = as.character(factura_online))
train <- transform(train, plan_datos = as.character(plan_datos))

test <- transform(test, tipo_cliente = as.character(tipo_cliente))
test <- transform(test, factura_online = as.character(factura_online))
test <- transform(test, plan_datos = as.character(plan_datos))

# histogramas Categoricas

hist1 <- base_completa %>% ggplot(aes(x=tipo_cliente)) +
  geom_bar() +
  ggtitle('Tipo Ciente') +
  labs(y = "", x = "Tipo Cliente") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist2 <- base_completa %>% ggplot(aes(x=factura_online)) +
  geom_bar() +
  ggtitle('Factura Online') +
  labs(y = "", x = "Factura Online") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist4 <- base_completa %>% ggplot(aes(x=plan_datos)) +
  geom_bar() +
  ggtitle('Plan Datos') +
  labs(y = "", x = "Plan Datos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(hist1, hist2, hist4, nrow = 1, ncol = 3)

# Histograma Escalares

hist3 <- base_completa %>% ggplot(aes(x=antiguedad_equipo)) +
  geom_bar() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Antiguedad Equipo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist5 <- base_completa %>% ggplot(aes(x=facturacion)) +
  geom_bar() +
  ggtitle('Facturación') +
  labs(y = "", x = "Facturación") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist6 <- base_completa %>% ggplot(aes(x=mora)) +
  geom_bar() +
  ggtitle('Mora') +
  labs(y = "", x = "Mora") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist7 <- base_completa %>% ggplot(aes(x=minutos)) +
  geom_bar() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Antiguedad Equipo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box1 <- base_completa %>% ggplot(aes(x=antiguedad_equipo)) +
  geom_boxplot() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Antiguedad_Equipo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box1 <- box1 + coord_flip()

box2 <- base_completa %>% ggplot(aes(x=facturacion)) +
  geom_boxplot() +
  ggtitle('Facturación') +
  labs(y = "", x = "Facturación") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box2 <- box2 + coord_flip()

box3 <- base_completa %>% ggplot(aes(x=mora)) +
  geom_boxplot() +
  ggtitle('Mora') +
  labs(y = "", x = "Mora") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box3 <- box3 + coord_flip()

box4 <- base_completa %>% ggplot(aes(x=minutos)) +
  geom_boxplot() +
  ggtitle('Minutos') +
  labs(y = "", x = "Minutos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box4 <- box4 + coord_flip()

ggarrange(hist3, hist5, hist6, hist7,box1, box2, box3, box4, nrow = 2, ncol = 4)

#Box Plots variable respuesta

train <- transform(train, resultado = as.character(resultado))

box5 <- train %>% ggplot(aes(x=resultado, y = antiguedad_equipo)) +
  geom_boxplot() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box6 <- train %>% ggplot(aes(x=resultado, y = facturacion)) +
  geom_boxplot() +
  ggtitle('Facturación') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box7 <- train %>% ggplot(aes(x=resultado, y = mora)) +
  geom_boxplot() +
  ggtitle('Mora') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box8 <- train %>% ggplot(aes(x=resultado, y = minutos)) +
  geom_boxplot() +
  ggtitle('Minutos') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(box5, box6, box7, box8, nrow = 2, ncol = 2)

# Cruces de variables - Tipo Cliente


box9 <- train %>% ggplot(aes(x=resultado, y = antiguedad_equipo, fill = tipo_cliente)) +
  geom_boxplot() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box10 <- train %>% ggplot(aes(x=resultado, y = facturacion, fill = tipo_cliente)) +
  geom_boxplot() +
  ggtitle('Facturación') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box11 <- train %>% ggplot(aes(x=resultado, y = mora, fill = tipo_cliente)) +
  geom_boxplot() +
  ggtitle('Mora') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box12 <- train %>% ggplot(aes(x=resultado, y = minutos, fill = tipo_cliente)) +
  geom_boxplot() +
  ggtitle('Minutos') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Cruces de variables - Factura Online

box13 <- train %>% ggplot(aes(x=resultado, y = antiguedad_equipo, fill = factura_online)) +
  geom_boxplot() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box14 <- train %>% ggplot(aes(x=resultado, y = facturacion, fill = factura_online)) +
  geom_boxplot() +
  ggtitle('Facturación') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box15 <- train %>% ggplot(aes(x=resultado, y = mora, fill = factura_online)) +
  geom_boxplot() +
  ggtitle('Mora') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box16 <- train %>% ggplot(aes(x=resultado, y = minutos, fill = factura_online)) +
  geom_boxplot() +
  ggtitle('Minutos') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(box13, box14, box15, box16, nrow = 2, ncol = 2)

# Cruces de variables - Plan Datos

box17 <- train %>% ggplot(aes(x=resultado, y = antiguedad_equipo, fill = plan_datos)) +
  geom_boxplot() +
  ggtitle('Antiguedad Equipo') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box18 <- train %>% ggplot(aes(x=resultado, y = facturacion, fill = plan_datos)) +
  geom_boxplot() +
  ggtitle('Facturación') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box19 <- train %>% ggplot(aes(x=resultado, y = mora, fill = plan_datos)) +
  geom_boxplot() +
  ggtitle('Mora') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

box20 <- train %>% ggplot(aes(x=resultado, y = minutos, fill = plan_datos)) +
  geom_boxplot() +
  ggtitle('Minutos') +
  labs(y = "", x = "Deserción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(box9, box10, box11, box12, box13, box14, box15, box16, box17, box18, box19, box20, nrow = 3, ncol = 4)



# Preparación de los datos

train$dias_de_contrato <- difftime(as.POSIXct("2019-01-01"), train$fecha_inicio_contrato)
train$dias_de_contrato <- as.numeric(train$dias_de_contrato, units = "days")

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
train$edad <- elapsed_months(as.Date("2019-01-01"), train$fecha_nacimiento)


train <- dummy_cols(train, select_columns =c('tipo_cliente'),
                 remove_selected_columns = TRUE)
train <- train[,-which(names(train) %in% c('id','fecha_nacimiento','fecha_inicio_contrato','tipo_cliente_3'))]

trainvars <- dummyVars("~.",data=train, fullRank = F)
train2 <- as.data.frame(predict(trainvars,newdata=train))
train2 <- train2 %>%
  mutate(resultado=as.factor(resultado))




# --------- Balanceo ---------

## Oversampling
prop.table(table(train$resultado))
table(train$resultado)

datos_bal_over <- ovun.sample(resultado ~ ., 
                              data = train, 
                              method = "over", 
                              N = table(train$resultado)[1]*2)$data

table(datos_bal_over$resultado)

## SMOTE
datos_bal_smote <- performanceEstimation::smote(resultado ~ ., 
                                                data = train, 
                                                perc.over = 2, 
                                                k = 5, 
                                                perc.under = 2)

table(datos_bal_smote$resultado)
prop.table(table(datos_bal_smote$resultado))


# --------- Modelo ---------

# Original
x <- train[,-which(names(train) %in% c('resultado'))]
y <- train$resultado

# Oversampling
x <- datos_bal_over[,-which(names(datos_bal_over) %in% c('resultado'))]
y <- datos_bal_over$resultado

# SMOTE
x <- datos_bal_smote[,-which(names(datos_bal_smote) %in% c('resultado'))]
y <- datos_bal_smote$resultado


fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE)

grid <- expand.grid(depth = c(2,3),
                    learning_rate = c(0.03,0.04,0.05),
                    iterations = c(900,1100,1300,1500),
                    l2_leaf_reg = c(0.5,1),
                    rsm = 0.95,
                    border_count = 64
)


report <- train(x, as.factor(make.names(y)),
                method = catboost.caret,
                logging_level = 'Silent', preProc = NULL,
                tuneGrid = grid, trControl = fit_control)



report$bestTune
best_results <- report$results 
best_results <- best_results[order(-best_results$Accuracy),]
best_results[1:5,]

print(report)
importance <- varImp(report, scale = FALSE)
print(importance)



#train/test

set.seed(4) 
sample <- sample.int(nrow(train2), floor(.75*nrow(train2)))
df.train <- train2[sample, ]
df.test <- train2[-sample, ]



mod.logit <- glm(resultado~., family=binomial, df.train)
steplogit <- step(mod.logit, direction="both", trace=0)
summary(steplogit)

coeficientes <- steplogit$coefficients
odd_change <- exp(coeficientes)
odd_change

# Odd ratios
oddbase<-prop.table(balance)[2]/prop.table(balance)[1]
oddbase

oddfin <- oddbase*odd_change
prob1step <- oddfin/(1+oddfin)
prob1step


probabil <- predict(steplogit, newdata = df.test, type='response')
test_prob <- df.test %>% 
  mutate(probabilidades = probabil)

prontest <- ifelse(probabil > 0.5,1,0)


pronostrain <- ifelse(steplogit$fitted.values > 0.5,1,0)

FBeta_Score(df.test$resultado, prontest, positive = "1", beta = 1)
FBeta_Score(df.test$resultado, prontest, positive = "1", beta = 0.5)
FBeta_Score(df.test$resultado, prontest, positive = "1", beta = 2)


mconftrain <- confusionMatrix(as.factor(pronostrain),
                              df.train$resultado, positive = "1")
mconftrain$table
mconftrain$byClass


probabil <- predict(steplogit, newdata = df.test, type='response')
test_prob <- df.test %>% 
  mutate(probabilidades = probabil)

pre <- prediction(probabil, df.test$resultado)
pre@predictions


curvaROC <- performance(pre, measure="tpr", x.measure="fpr")
plot(curvaROC)
abline(h = 0.8)

auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]
auc



# --------- Preparación de variables TEST ---------

test <- read_excel("testelco.xlsx")
save <- data.frame(test$id)

test$dias_de_contrato <- difftime(as.POSIXct("2019-01-01"), test$`Fecha inicio contrato`)
test$dias_de_contrato <- as.numeric(test$dias_de_contrato, units = "days")


test$edad <- elapsed_months(as.Date("2019-01-01"), test$`Fecha de nacimiento`)

test <- dummy_cols(test, select_columns =c('tipo cliente'),
                      remove_selected_columns = TRUE)
test <- test[,-which(names(test) %in% c('id','Fecha de nacimiento','Fecha inicio contrato'))]



#Select model
names(save)[1] = "id"
predict_1 = predict(report, newdata=test, type = "prob")
save$resultado <- predict_1$X1
write.csv(save,"csv/018.csv", row.names = FALSE)

