#Importar librerias
library(dplyr)
library(ggpubr)
library(ggplot2)
library(VIM)
library(tidyr)
library(naniar)
#Leer data set de Hepatitis.
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

#Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')


# Obtener columna que posee m?s valores sin documentar

missing_values_recount <- data %>% summarise_all(~ sum(. == "?"))

# Obtener qu? paciente posee la mayor cantidad de atributos sin documentar
missing_values_per_patient <- rowSums(data == "?")
missing_values_per_patient <- data.frame(missing_values = missing_values_per_patient) %>% 
                                mutate(id = seq(nrow(data))) %>%
                                arrange(desc(missing_values))

#Obtener top 10 pacientes con mas valores NA
ids_patient_top_ten_missing_Values <- missing_values_per_patient$id[1:10]

# max_missing_values_patient <- which(missing_values_per_patient == max(missing_values_per_patient))

#Cambiar '?' valores a NA
data <- data %>% mutate_all(~na_if(., "?"))

#Visualizar datos faltantes en el data set - Muestra porcentajes de missing data por variable.

plot_missing_data <- gg_miss_var(data, show_pct = TRUE) + labs(y = "Porcentaje de valores faltantes")
print(plot_missing_data)

# Limpiar los datos

#Eliminar variable Protime, dado que contiene 43% de valores NA.
data$Protime <- NULL

#Eliminar top 10 filas con mas valores NA's
data$id <- seq(1:155)
data <- data[!(data$id %in% ids_patient_top_ten_missing_Values), ]

#Convertir las columnas a los formatos correctos
data$Class <- as.character(data$Class)
data$Age <- as.integer(data$Age)
data$Sex <- as.character(data$Sex)
data$Steroid <- as.character(data$Steroid)
data$Antivirals <- as.character(data$Antivirals)
data$Fatigue <- as.character(data$Fatigue)
data$Malaise <- as.character(data$Malaise)
data$Anorexia <- as.character(data$Anorexia)
data$Liver_Big <- as.character(data$Liver_Big)
data$Liver_Firm < as.character(data$Liver_Firm)
data$Spleen_Palpable <- as.character(data$Spleen_Palpable)
data$Spiders <- as.character(data$Spiders)
data$Ascites <- as.character(data$Ascites)
data$Varices <- as.character(data$Varices)
data$Bilirubin <- as.numeric(data$Bilirubin)
data$Alk_Phosphate <- as.integer(data$Alk_Phosphate)
data$Sgot < as.integer(data$Sgot)
data$Albumin <- as.numeric(data$Albumin)
data$Histology <- as.character(data$Histology)

#Obtener la moda de una determinada columna
getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Impute Missing Data
for (cols in colnames(data)) {
  if (cols %in% names(data[,sapply(data, is.numeric)])) {
    data<-data%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    data<-data%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), getmode(!!rlang::sym(cols))))
  }
}

# Estadisticas descriptivas ----

# Para las primeras 10 variables, 9 son categóricas.
# Se omite la primera columna que es la Clase (muerto o vivo).
# La columna 2, 'Age', se dejará para después, enfocándonos en las 8 categóricas a continuación.
# Se hará un recuento de la frecuencia para cada una.


# Función que entrega un gráfico de las variables categóricas dicotómicas que solo
# entregan información de si un síntoma está presente o no.
# Entrada: datos: dataframe/table, column_name: string
# Salida: gráfico de barras
summarise_aux <- function(data, column_name){
  data <- data.frame(x = c("Presente", "No presente"), 
                     total = count(data, get(column_name)) %>% pull())
  return(ggbarplot(data, x = 'x', y = 'total', label = TRUE, lab.pos = "out", xlab = column_name))
}

gender <- data.frame(sex = c("Masculino", "Femenino"), total = count(data, Sex) %>% pull())
gender_plt <- ggbarplot(gender, x = 'sex', y = 'total', label = TRUE, lab.pos = "out")

steroid_plt <- summarise_aux(data, "Steroid")
anti_plt <- summarise_aux(data, "Antivirals")
fatigue_plt <- summarise_aux(data, "Fatigue")
cathegorical_plots_pt_1 <- ggarrange(gender_plt, steroid_plt, anti_plt, fatigue_plt,
                                     ncol = 2, nrow = 2)

malaise_plt <- summarise_aux(data, "Malaise")
anorex_plt <- summarise_aux(data, "Anorexia")
liver_b_plt <- summarise_aux(data, "Liver_Big") + scale_x_discrete("Hígado Agrandado")
liver_f_plt <- summarise_aux(data, "Liver_Firm") + scale_x_discrete("Hígado Firme")
cathegorical_plots_pt_2 <- ggarrange(malaise_plt, anorex_plt, liver_b_plt, liver_f_plt,
                                     ncol = 2, nrow = 2)


# Analizamos la edad.
# age_hist <- hist(data$Age, labels = TRUE, ylim = c(0, 32), breaks = 25)
age_hist <- ggplot(data, aes(x = Age, fill = Class, colour = Class)) + 
  geom_histogram(alpha = 0.5, position = "identity", bins = 25)
plot(age_hist)

# Analizar distribución de Class para Bilirubin (bilirubina)

bilirrubin_class <- ggplot(data,aes(x = Class, y = Bilirubin, fill = Class))+
  geom_boxplot()

# Vemos que pacientes de clase 1 (no sobrevivientes) tuvieron cantidades 
# más altas de bilirubina cuando padecieron la enfermedad.



# Análisis inferencial ----





