# Cargar librerias ----
library(tidyverse)
library(cluster)
library(Rtsne) 
library(ggplot2)
library(fpc)
library(factoextra)
library(VIM)
library(ggplot2)
library(naniar)


# Leer data set de Hepatitis. -----
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

# Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')

#Mostrar primeros valores del dataset
print(str(data))

#Convertir '?' a NAs
data <- data %>% mutate_all(~na_if(., "?"))


# Convertir las columnas a los formatos correctos ----
data$Class <- as.factor(data$Class)
data$Age <- as.integer(data$Age)
data$Sex <- as.factor(data$Sex)
data$Steroid <- as.factor(data$Steroid)
data$Antivirals <- as.factor(data$Antivirals)
data$Fatigue <- as.factor(data$Fatigue)
data$Malaise <- as.factor(data$Malaise)
data$Anorexia <- as.factor(data$Anorexia)
data$Liver_Big <- as.factor(data$Liver_Big)
data$Liver_Firm <- as.factor(data$Liver_Firm)
data$Spleen_Palpable <- as.factor(data$Spleen_Palpable)
data$Spiders <- as.factor(data$Spiders)
data$Ascites <- as.factor(data$Ascites)
data$Varices <- as.factor(data$Varices)
data$Bilirubin <- as.numeric(data$Bilirubin)
data$Alk_Phosphate <- as.integer(data$Alk_Phosphate)
data$Sgot <- as.numeric(data$Sgot)
data$Albumin <- as.numeric(data$Albumin)
data$Protime <- as.integer(data$Protime)
data$Histology <- as.factor(data$Histology)

#Visualizar variables en formatos adecuados.
print(str(data))


# Tratar valores perdidos. ----

# Visualizar datos faltantes en el data set - Muestra porcentajes de missing data por variable.
plot_missing_data <- gg_miss_var(data, show_pct = TRUE) + labs(y = "Porcentaje de valores faltantes")
print(plot_missing_data)

# Obtener qué paciente posee la mayor cantidad de atributos sin documentar
missing_values_per_patient <- rowSums(is.na(data))
missing_values_per_patient <- data.frame(missing_values = missing_values_per_patient) %>% 
                              mutate(id = seq(nrow(data))) %>%arrange(desc(missing_values))

# Obtener top 10 pacientes con más valores NA
ids_patient_top_ten_missing_Values <- missing_values_per_patient$id[1:10]

#Eliminar variable Protime, dado que contiene 43% de valores NA.
data$Protime <- NULL

# Eliminar top 10 filas con m+as valores NA's
data$id <- seq(1:155)
data <- data[!(data$id %in% ids_patient_top_ten_missing_Values), ]
data$id <-NULL

# Imputacion ----

# Obtener la moda de una determinada columna
getmode <- function(v){
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute Missing Data
for (cols in colnames(data)) {
  if (cols %in% names(data[,sapply(data, is.numeric)])) {
    data<-data%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else {
    data<-data%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), getmode(!!rlang::sym(cols))))
  }
}

# Visualizar nuevamente porcentaje de valores perdidos por variable.
plot_missing_data_after_imputation <- gg_miss_var(data, show_pct = TRUE) + labs(y = "Porcentaje de valores faltantes despues de imputación.")
print(plot_missing_data_after_imputation)

# Outliers ----

# Visualizar outliers en variables numericas
boxplot_age <- boxplot(data$Age, horizontal = TRUE, main = "Boxplot variable Age")
boxplot_bilirubin <- boxplot(data$Bilirubin, horizontal = TRUE, main = "Boxplot variable Bilirubin")
boxplot_alk_phosphate <- boxplot(data$Alk_Phosphate, horizontal = TRUE, main = "Boxplot variable Alk Phosphate")
boxplot_sgot <- boxplot(data$Sgot, horizontal = TRUE, main = "Boxplot variable Sgot")
boxplot_albumin <- boxplot(data$Albumin, horizontal = TRUE, main = "Boxplot variable Albumin")

# Obtencion del cluster ----

#Distancia de Gower
gower_dist <- daisy(data,metric = "gower")
gower_mat <- as.matrix(gower_dist)

#Numero optimo de cluster
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

#Dos clusters parecen ser la mejor opcion.

#PAM Clustering 
k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- data %>%mutate(cluster = pam_fit$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results$the_summary

#Visualizar cluster
fviz_cluster(pam_results,data=data,ellipse.type="norm")



