#Importar librerias
library(dplyr)
library(ggpubr)
library(ggplot2)
library(VIM)

#Leer data set de Hepatitis.
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

#Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')


# Obtener columna que posee m?s valores sin documentar

missing_values_recount <- data %>% summarise_all(~ sum(. == "?"))

# Obtener qu? paciente posee la mayor cantidad de atributos sin documentar
missing_values_per_patient <- rowSums(data == "?")
max_missing_values_patient <- which(missing_values_per_patient == max(missing_values_per_patient))

#Cambiar '?' valores a NA
data <- data %>% mutate_all(~na_if(., "?"))

#Visualizar datos faltantes en el data set - Muestra porcentajes de missing data por variable.

histogram_of_missing_values <- suppressWarnings(aggr(data, col=c('navyblue','red'), 
                               numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, 
                               gap=3, ylab=c("Histograma de valores faltantes en data set","Patron")))

#Estadisticas descriptivas ----









#Estadisticas inferenciales ----