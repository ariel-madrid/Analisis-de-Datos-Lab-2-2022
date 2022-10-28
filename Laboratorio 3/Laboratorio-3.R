# Cargar librerias ----
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(arulesViz)
library(dplyr)
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

#Discretizar variables numericas.
data.rules <- data

#Discretizar edad
data.rules$Age[data.rules$Age>=5 & data.rules$Age<=13] <- "Niño"
data.rules$Age[data.rules$Age>=14 & data.rules$Age<=17] <- "Adolescente"
data.rules$Age[data.rules$Age>=18 & data.rules$Age<=35] <- "Adulto joven"
data.rules$Age[data.rules$Age>=36 & data.rules$Age<=64] <- "Adulto"
data.rules$Age[data.rules$Age == 78] <- "Tercera edad"
data.rules$Age[data.rules$Age == 66] <- "Tercera edad"
data.rules$Age[data.rules$Age == 65] <- "Tercera edad"
data.rules$Age[data.rules$Age == 69] <- "Tercera edad"
data.rules$Age[data.rules$Age == 72] <- "Tercera edad"

data.rules$Age <- as.factor(data.rules$Age)

#Discretizar bilirubin
data.rules$Bilirubin[data$Bilirubin<0.1] <- "Bilirubin baja"
data.rules$Bilirubin[data$Bilirubin>=0.1 & data$Bilirubin<=1.2] <- "Bilirubin normal"
data.rules$Bilirubin[data$Bilirubin>1.2 & data$Bilirubin<=Inf] <- "Bilirubin alta"

data.rules$Bilirubin <- as.factor(data.rules$Bilirubin)

#Discretizar Alk Phosphate
data.rules$Alk_Phosphate[data$Alk_Phosphate<30] <- "Alk Phosphate baja"
data.rules$Alk_Phosphate[data$Alk_Phosphate>=30 & data$Alk_Phosphate<=120] <- "Alk Phosphate normal"
data.rules$Alk_Phosphate[data$Alk_Phosphate>120 & data$Alk_Phosphate<=Inf] <- "Alk Phosphate alta"

data.rules$Alk_Phosphate <- as.factor(data.rules$Alk_Phosphate)

#Discretizar Sgot
data.rules$Sgot[data$Sgot<8] <- "Sgot baja"
data.rules$Sgot[data$Sgot>=8 & data$Sgot<=45] <- "Sgot normal"
data.rules$Sgot[data$Sgot>45 & data$Sgot<=Inf] <- "Sgot alta"

data.rules$Sgot <- as.factor(data.rules$Sgot)

#Discretizar Albumin
data.rules$Albumin[data.rules$Albumin<3.4] <- "Albumin baja"
data.rules$Albumin[data.rules$Albumin>=3.4 & data.rules$Albumin<=5.4] <- "Albumin normal"
data.rules$Albumin[data.rules$Albumin==6.4] <- "Albumin alta"

data.rules$Albumin <- as.factor(data.rules$Albumin)

# Informacion sobre la clase
labs <- c("Muerto","Vivo")
porcentaje <- c(19.3,80.6)

pie <- data.frame(labs,porcentaje)

ggplot(pie,aes(x="",y=porcentaje, fill=labs))+
        geom_bar(stat = "identity", color="white")+
        geom_text(aes(label=porcentaje),position=position_stack(vjust=0.5),color="white",size=6)+
        coord_polar(theta = "y")+
        scale_fill_manual(values=c("salmon","steelblue"))+
        theme_void()+
        labs(title="Proporcion de cada clase")

# Obtener reglas de asociacion ----

#rules <- apriori(data=data.rules,parameter = list(support=0.6,minlen=2,maxlen=6,target="rules"))

#inspect(sort(x=rules,decreasing = TRUE,by="confidence"))


# Sin embargo, importan las reglas que contemplen a la clase en el consecuente

#Reglas que tienen a la clase muerto como consecuente
rules_muerto <- apriori(data=data.rules,
                 parameter = list(support=0.06,confidence=0.8,minlen=2,maxlen=9,target="rules"),
                 appearance = list(rhs=c("Class=Muerto")))

rules_muerto <- rules_muerto[!is.redundant(rules_muerto)]

inspect(sort(x=rules_muerto,decreasing = TRUE,by="confidence"))

#Reglas que tienen a la clase vivo como consecuente
rules_vivo <- apriori(data=data.rules,
                        parameter = list(support=0.6,confidence=0.8,minlen=3,maxlen=9,target="rules"),
                        appearance = list(rhs=c("Class=Vivo")))

rules_vivo <- rules_vivo[!is.redundant(rules_vivo)]

inspect(sort(x=rules_vivo,decreasing = TRUE,by="confidence"))

#Otras reglas interesantes.
rules_sgot <- apriori(data=data.rules,
                      parameter = list(support=0.15,confidence=0.8,minlen=3,maxlen=9,target="rules"),
                      appearance = list(rhs=c("Sgot=Sgot alta")))

rules_sgot <- rules_sgot[!is.redundant(rules_sgot)]

inspect(sort(x=rules_sgot,decreasing = TRUE,by="lift"))

#Otras reglas interesantes.
rules_albumin <- apriori(data=data.rules,
                 parameter = list(support=0.1,confidence=0.8,minlen=3,maxlen=6,target="rules"),
                 appearance = list(rhs=c("Albumin=Albumin baja")))

rules_albumin <- rules_albumin[!is.redundant(rules_albumin)]

inspect(sort(x=rules_albumin,decreasing = TRUE,by="lift"))

#Otras reglas interesantes.
rules_bilirubin <- apriori(data=data.rules,
                 parameter = list(support=0.1,confidence=0.8,minlen=2,maxlen=6,target="rules"),
                 appearance = list(rhs=c("Bilirubin=Bilirubin alta")))

rules_bilirubin <- rules_bilirubin[!is.redundant(rules_bilirubin)]

inspect(sort(x=rules_bilirubin,decreasing = TRUE,by="lift"))





