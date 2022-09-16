#Importar librerias
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggplot2)

#Leer data set de Hepatitis.
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

#Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')


# Contar variables faltantes por columna

get_missing_values <- function(i, data) {
  count <- data %>% 
    count(data[i]) %>% 
    filter(get(colnames(data)[i]) == "?") %>% 
    pull(n)
  return(count <- ifelse(length(count) > 0, count, 0))
}

# Obtener columna que posee más valores sin documentar

missing_values_recount <- sapply(seq_len(ncol(data)), 
       get_missing_values, 
       data) %>%
  as.data.frame %>%
  mutate(attr = colnames(data)) %>%
  rename(n = ".") 

# Obtener qué paciente posee la mayor cantidad de atributos sin documentar
missing_values_per_patient <- rowSums(data == "?")
max_missing_values_patient <- which(missing_values_per_patient == max(missing_values_per_patient))


#Estadisticas descriptivas ----

#Estadisticas inferenciales ----