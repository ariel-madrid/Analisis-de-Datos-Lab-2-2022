# Cargar librerias ----
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library("C50")
library("caret")
library(groupdata2)
# Leer data set de Hepatitis. -----
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

# Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')

#Convertir '?' a NAs
data <- data %>% mutate_all(~na_if(., "?"))

# Asignar nombres descriptivos a los niveles de las variables categoricas

# Convertir las columnas a los formatos correctos ----
data$Class <- factor(ifelse(data$Class==1,'Muerto','Vivo'))

data$Age <- as.numeric(data$Age)

data$Sex <- factor(ifelse(data$Sex==1, 'Hombre','Mujer'))

data$Steroid <- factor(ifelse(data$Steroid==1,'Yes_Steroid','No_Steroid'))

data$Antivirals <- factor(ifelse(data$Antivirals==1,'Yes_Antivirals','No_Antivirals'))

data$Fatigue <- factor(ifelse(data$Fatigue==1,'Yes_Fatigue','No_Fatigue'))

data$Malaise <- factor(ifelse(data$Malaise==1,'Yes_Malaise','No_Malaise'))

data$Anorexia <- factor(ifelse(data$Anorexia==1,'Yes_Anorexia','No_Anorexia'))

data$Liver_Big <- factor(ifelse(data$Liver_Big==1,'Yes_Liver_Big','No_Liver_Big'))

data$Liver_Firm <- factor(ifelse(data$Liver_Firm==1,'Yes_Liver_Firm','No_Liver_Firm'))

data$Spleen_Palpable <- factor(ifelse(data$Spleen_Palpable==1,'Yes_Spleen_Palpable','No_Spleen_Palpable'))

data$Spiders <- factor(ifelse(data$Spiders==1,'Yes_Spiders','No_Spiders'))

data$Ascites <- factor(ifelse(data$Ascites==1,'Yes_Ascites','No_Ascites'))

data$Varices <- factor(ifelse(data$Varices==1,'Yes_Varices','No_Varices'))

data$Bilirubin <- as.numeric(data$Bilirubin)
data$Alk_Phosphate <- as.integer(data$Alk_Phosphate)
data$Sgot <- as.numeric(data$Sgot)
data$Albumin <- as.numeric(data$Albumin)

data$Histology <- factor(ifelse(data$Histology==1,'Yes_Histology','No_Histology'))

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

#Data 
data.tree <- data

#Datos de entrenamiento
set.seed(222)
training <- createDataPartition(data.tree$Class,p=0.7)$Resample1

#Datos de prueba
training.set <- data.tree[training,]
test.set <- data.tree[-training,]

#Arbol de decision
tree = C5.0(training.set[-1], training.set$Class)
tree.rules <- C5.0(x=training.set[,-1],y=training.set$Class,rules=T)
tree.pred.class <- predict(tree, test.set[,-1],type="class")
tree.pred.prob <- predict(tree,test.set[,-1],type="prob")

#Matriz de confusion 
conf.matrix.tree <- confusionMatrix(table(test.set$Class,tree.pred.class))
print(conf.matrix.tree)

######################################
tmp<-upsample(data, cat_col = "Class")

training2 <- createDataPartition(tmp$Class,p=0.7)$Resample1
training.set2 <- data.tree[training2,]
test.set2 <- data.tree[-training2,]

tree2 = C5.0(training.set2[-1], training.set2$Class)
tree.rules2 <- C5.0(x=training.set2[,-1],y=training.set2$Class,rules=T)
tree.pred.class2 <- predict(tree2, test.set2[,-1],type="class")
tree.pred.prob2 <- predict(tree2,test.set2[,-1],type="prob")

conf.matrix.tree2 <- confusionMatrix(table(test.set2$Class,tree.pred.class2))
print(conf.matrix.tree2)
