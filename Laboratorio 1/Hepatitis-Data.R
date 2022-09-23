#Importar librerías
library(dplyr)
library(ggpubr)
library(ggplot2)
library(VIM)
library(tidyr)
library(naniar)
library(corrplot)
library(nortest)
library(car)
#Leer data set de Hepatitis.
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")

#Asignar nombres mas representativos a las variables de acuerdo al archivo hepatitis.name.
names(data) <- c('Class', 'Age', 'Sex', 'Steroid', 'Antivirals', 'Fatigue', 'Malaise', 'Anorexia', 'Liver_Big', 'Liver_Firm', 'Spleen_Palpable',
                 'Spiders', 'Ascites', 'Varices', 'Bilirubin', 'Alk_Phosphate', 'Sgot', 'Albumin', 'Protime', 'Histology')


# Obtener columna que posee más valores sin documentar

missing_values_recount <- data %>% summarise_all(~ sum(. == "?"))

# Obtener qué paciente posee la mayor cantidad de atributos sin documentar
missing_values_per_patient <- rowSums(data == "?")
missing_values_per_patient <- data.frame(missing_values = missing_values_per_patient) %>% 
                                mutate(id = seq(nrow(data))) %>%
                                arrange(desc(missing_values))

#Obtener top 10 pacientes con más valores NA
ids_patient_top_ten_missing_Values <- missing_values_per_patient$id[1:10]

# max_missing_values_patient <- which(missing_values_per_patient == max(missing_values_per_patient))

# Cambiar '?' valores a NA
data <- data %>% mutate_all(~na_if(., "?"))

# Visualizar datos faltantes en el data set - Muestra porcentajes de missing data por variable.

plot_missing_data <- gg_miss_var(data, show_pct = TRUE) + labs(y = "Porcentaje de valores faltantes")
print(plot_missing_data)

# Limpiar los datos

#Eliminar variable Protime, dado que contiene 43% de valores NA.
data$Protime <- NULL

# Eliminar top 10 filas con m+as valores NA's
data$id <- seq(1:155)
data <- data[!(data$id %in% ids_patient_top_ten_missing_Values), ]
data$id <-NULL
 
# Convertir las columnas a los formatos correctos
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
data$Histology <- as.factor(data$Histology)

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

# Para las primeras 10 variables, 9 son categ?ricas.
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
liver_b_plt <- summarise_aux(data, "Liver_Big") + scale_x_discrete("Higado Agrandado")
liver_f_plt <- summarise_aux(data, "Liver_Firm") + scale_x_discrete("Higado Firme")
cathegorical_plots_pt_2 <- ggarrange(malaise_plt, anorex_plt, liver_b_plt, liver_f_plt, ncol = 2, nrow = 2)

#Grafico de frecuencias para reconocer la moda de la variable Spleen Palpable
barplot_spleen_palpable <- ggplot(data,aes(x=factor(Spleen_Palpable)))+ geom_bar(position="dodge")+ labs(title="", x="Spleen Palpable", y = "Frecuencia")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#Grafico de frecuencias para reconocer la moda de la variable Spiders
barplot_spiders <- ggplot(data,aes(x=factor(Spiders)))+ geom_bar(position="dodge")+ labs(title="", x="Spiders", y = "Frecuencia")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#Grafico de frecuencias para reconocer la moda de la variable Ascites
barplot_ascites <- ggplot(data,aes(x=factor(Ascites)))+ geom_bar(position="dodge")+ labs(title="", x="Ascites", y = "Frecuencia")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#Grafico de frecuencias para reconocer la moda de la variable Varices
barplot_varices <- ggplot(data,aes(x=factor(Varices)))+ geom_bar(position="dodge")+ labs(title="", x="Varices", y = "Frecuencia")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#Grafico de frecuencias para reconocer la moda de la variable Histology
barplot_histology <- ggplot(data,aes(x=factor(Histology)))+ geom_bar(position="dodge")+ labs(title="", x="Histology", y = "Frecuencia")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#Combinar figuras
final_barplot_combined <- ggarrange(barplot_spleen_palpable,barplot_spiders,barplot_ascites,barplot_varices,barplot_histology, nrow=2,ncol=4)
print(final_barplot_combined)

#Obtener matriz de correlacion para observar correlacion de las variables numéricas de la base de datos.
correlacion<-round(cor(select_if(data, is.numeric)), 1)

corrplot(correlacion, method="number", type="upper")

#Diagrama de dispersion que muestra leve relacion inversa entre Bilirubin y Albumin
bilirubin_albumin_cor <- scatterplot(Bilirubin ~ Albumin,data = data, smooth = FALSE, grid = F, frame = F)
print(bilirubin_albumin_cor)

#Visualizar distribución de variables numéricas continuas

#Bilirubin distribution
bilirubin_distribution <- ggplot(data, aes(x=Bilirubin)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FFF666") + labs(title="", y = "Densidad") + 
  geom_vline(aes(xintercept=mean(Bilirubin, na.rm=T)), color="red", linetype="dashed", size=1)

#Albumin distribution
albumin_distribution <- ggplot(data, aes(x=Albumin)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FFF666") + labs(title="", y = "Densidad") + 
  geom_vline(aes(xintercept=mean(Albumin, na.rm=T)), color="red", linetype="dashed", size=1)

#Combinar gráficos
final_plot_distribution <- ggarrange(bilirubin_distribution,albumin_distribution, nrow=1,ncol=2)
print(final_plot_distribution)

#Gráfica de barras segmentadas para Class + Spleen_Palpable
levels(data$Class) <- c("Muerto","Vivo")
levels(data$Spleen_Palpable) <- c("Yes", "No")
contingency_table  <- xtabs(~ Class + Spleen_Palpable , data = data)
contingency_table  <- as.data.frame(contingency_table)

#Se crea el gráfico
segmented_bar_plot <- ggplot(contingency_table, aes(fill = Class , y = Freq , x = Spleen_Palpable))
segmented_bar_plot <- segmented_bar_plot + geom_bar(position = "stack", stat = "identity")
segmented_bar_plot <- segmented_bar_plot + labs(y = "Frecuencia") + ggtitle("Barras  apiladas para Class y Spleen Palpable")
segmented_bar_plot <- segmented_bar_plot + theme_pubr()

print(segmented_bar_plot)


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



#Test Lilliefors para determinar si variable Albumin sigue una distribución normal.

#H0: La distribución es normal
#Ha: La distribución no es normal

lilliefors_albumin <- lillie.test(x = data$Albumin)
print(lilliefors_albumin)

