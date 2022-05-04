if (!require('readxl')) install.packages('readxl')
if (!require('fastDummies')) install.packages('fastDummies')
if (!require('psych')) install.packages('psych')



## Revisar que el archivo cartera se encuentre en documentos, 
## o la ruta en donde se encuentra direccionado R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# --------- Lectura del archivo en excel ---------
df <- read_excel("traintelco.xlsx")
df_test <- read_excel("testelco.xlsx")

# --------- Preparación de variables TRAIN ---------


df$dias_de_contrato <- difftime(as.POSIXct("2019-01-01"), df$`Fecha inicio contrato`)
df$dias_de_contrato <- as.numeric(df$dias_de_contrato, units = "days")

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  1 * (ed$year - sd$year)
}
df$edad <- elapsed_months(as.Date("2019-01-01"), df$`Fecha de nacimiento`)

df <- dummy_cols(df, select_columns =c('tipo cliente'),
                 remove_selected_columns = TRUE)
df <- df[,-which(names(df) %in% c('id','Fecha de nacimiento','Fecha inicio contrato',
                                  'tipo cliente_1'))]

# --------- Preparación de variables TEST ---------

df_test$dias_de_contrato <- difftime(as.POSIXct("2019-01-01"), df_test$`Fecha inicio contrato`)
df_test$dias_de_contrato <- as.numeric(df_test$dias_de_contrato, units = "days")

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  1 * (ed$year - sd$year)
}
df_test$edad <- elapsed_months(as.Date("2019-01-01"), df_test$`Fecha de nacimiento`)

df_test <- dummy_cols(df_test, select_columns =c('tipo cliente'),
                 remove_selected_columns = TRUE)
df_test <- df_test[,-which(names(df_test) %in% c('id','Fecha de nacimiento','Fecha inicio contrato',
                                  'tipo cliente_1'))]

str(df_test)
summary(df_test)
describe(df_test)

