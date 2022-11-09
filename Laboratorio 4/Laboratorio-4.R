# Cargar librerias ----
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library("C50")
library("caret")
# Leer data set de Hepatitis. -----
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

# Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')

#Convertir '?' a NAs
data <- data %>% mutate_all(~na_if(., "?"))

# Asignar nombres descriptivos a los niveles de las variables categoricas

# Convertir las columnas a los formatos correctos ----
data$Class <- as.factor(data$Class)
data$Class <- droplevels(data$Class)

data$Age <- as.numeric(data$Age)

data$Sex <- as.factor(data$Sex)
data$Sex <- droplevels(data$Sex)

data$Steroid <- as.factor(data$Steroid)
data$Steroid <- droplevels(data$Steroid)

data$Antivirals <- as.factor(data$Antivirals)
data$Antivirals <- droplevels(data$Antivirals)

data$Fatigue <- as.factor(data$Fatigue)
data$Fatigue <- droplevels(data$Fatigue)

data$Malaise <- as.factor(data$Malaise)
data$Malaise <- droplevels(data$Malaise)

data$Anorexia <- as.factor(data$Anorexia)
data$Anorexia <- droplevels(data$Anorexia)

data$Liver_Big <- as.factor(data$Liver_Big)
data$Liver_Big <- droplevels(data$Liver_Big)

data$Liver_Firm <- as.factor(data$Liver_Firm)
data$Liver_Firm <- droplevels(data$Liver_Firm)

data$Spleen_Palpable <- as.factor(data$Spleen_Palpable)
data$Spleen_Palpable <- droplevels(data$Spleen_Palpable)

data$Spiders <- as.factor(data$Spiders)
data$Spiders <- droplevels(data$Spiders)

data$Ascites <- as.factor(data$Ascites)
data$Ascites <- droplevels(data$Ascites)

data$Varices <- as.factor(data$Varices)
data$Varices <- droplevels(data$Varices)

data$Bilirubin <- as.numeric(data$Bilirubin)
data$Alk_Phosphate <- as.integer(data$Alk_Phosphate)
data$Sgot <- as.numeric(data$Sgot)
data$Albumin <- as.numeric(data$Albumin)

data$Histology <- as.factor(data$Histology)
data$Histology <- droplevels(data$Histology)

levels(data$Class) <- c("Muerto", "Vivo")
levels(data$Sex) <- c("Hombre", "Mujer")
levels(data$Steroid) <- c("Yes Steroid", "No Steroid")
levels(data$Antivirals) <- c("Yes Antivirals", "No Antivirals")
levels(data$Fatigue) <- c("YesFatigue", "No Fatigue")
levels(data$Malaise) <- c("Yes Malaise", "No Malaise")
levels(data$Anorexia) <- c("Yes Anorexia", "No Anorexia")
levels(data$Liver_Big) <- c("Yes Liver Big", "No Liver Big")
levels(data$Liver_Firm) <- c("Yes Liver Firm", "No Liver Firm")
levels(data$Spleen_Palpable) <- c("Yes Spleen Palpable", "No Spleen Palpable")
levels(data$Spiders) <- c("Yes Spiders", "No Spiders")
levels(data$Ascites) <- c("Yes Ascites", "No Ascites")
levels(data$Varices) <- c("Yes Varices", "No Varices")
levels(data$Histology) <- c("Yes Histology", "No Histology")

# Pre procesamiento ----
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

#Datos de prueba
training <- createDataPartition(data$Class,p=0.7)$Resample1
training.set <- data[training,]
test.set <- data[-training,]

#Arbol de decision
tree = C5.0(Class~., training.set)
tree.rules <- C5.0(x=training.set[,-8],y=training.set$Class,rules=T)
tree.pred.class <- predict(tree, test.set[,-8],type="class")
tree.pred.prob <- predict(tree,test.set[,-8],type="prob")

plot(tree)


conf.matrix.tree <- confusionMatrix(table(test.set$Class,tree.pred.class))
print(conf.matrix.tree)

