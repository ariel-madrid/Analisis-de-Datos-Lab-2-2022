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
library(ggpubr)
library(NbClust)
library(kmed)
# Leer data set de Hepatitis. -----
data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data", fileEncoding = "UTF-8", sep = ",")
data_respaldo <- data
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
boxplot_age <- ggboxplot(data$Age, horizontal = TRUE, main = "Boxplot variable Age")
boxplot_bilirubin <- ggboxplot(data$Bilirubin, horizontal = TRUE, main = "Boxplot variable Bilirubin")
boxplot_alk_phosphate <- ggboxplot(data$Alk_Phosphate, horizontal = TRUE, main = "Boxplot variable Alk Phosphate")
boxplot_sgot <- ggboxplot(data$Sgot, horizontal = TRUE, main = "Boxplot variable Sgot")
boxplot_albumin <- ggboxplot(data$Albumin, horizontal = TRUE, main = "Boxplot variable Albumin")

boxplots <- ggarrange(boxplot_age,boxplot_bilirubin,boxplot_alk_phosphate,boxplot_sgot,boxplot_albumin,ncol=3,nrow=2)
print(boxplots)
# Obtencion del cluster ----

#Eliminar la clase del dataset
#data$Class <- NULL

#Distancia de Gower
gower_dist <- daisy(data,metric = "gower")
gower_mat <- as.matrix(gower_dist)

#Distancia de Podani
podani_dist <- distmix(data, method = "podani",idnum = c(2,15,16,17,18),idcat = c(1,3,4,5,6,7,8,9,10,11,12,13,14,19))
podani_mat <- as.matrix(podani_dist)

#Distancia de Harikumar
harikumar_dist <- distmix(data, method = "harikumar",idnum = c(2,15,16,17,18),idcat = c(1,3,4,5,6,7,8,9,10,11,12,13,14,19))
harikumar_mat <- as.matrix(harikumar_dist)

#Numero optimo de cluster distancia de gower- Metodo de la silueta
silueta_gower <- fviz_nbclust(gower_mat, pam, method = "silhouette") +
  labs(subtitle = "Método de la silueta - Distancia de Gower")

#Numero optimo de cluster distancia de podani- Metodo de la silueta
silueta_podani <- fviz_nbclust(podani_mat, pam, method = "silhouette") +
  labs(subtitle = "Método de la silueta - Distancia de Podani")

#Numero optimo de cluster distancia de harikumar- Metodo de la silueta
silueta_harikumar <- fviz_nbclust(harikumar_mat, pam, method = "silhouette") +
  labs(subtitle = "Método de la silueta - Distancia de Harikumar")

silueta <- ggarrange(silueta_gower,silueta_podani,silueta_harikumar)

print(silueta)

#Numero optimo de cluster distancia de gower - Metodo del codo
codo_gower <- fviz_nbclust(gower_mat, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Método del codo - Distancia de Gower")

#Numero optimo de cluster distancia de podani- Metodo del codo
codo_podani <- fviz_nbclust(podani_mat, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Método del codo - Distancia de Podani")

#Numero optimo de cluster distancia de harikumar - Metodo del codo
codo_harikumar <- fviz_nbclust(harikumar_mat, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Método del codo - Distancia de Harikumar")

codo <- ggarrange(codo_gower,codo_podani,codo_harikumar)

print(codo)

#Numero optimo de cluster distancia de gower - Metodo del gap
gap_gower <- fviz_nbclust(gower_mat, pam,
                          nstart = 25,
                          method = "gap_stat",
                          nboot = 10
) + labs(subtitle = "Gap statistic method - Distancia de Gower")

#Numero optimo de cluster distancia de podani - Metodo del gap
gap_podani <- fviz_nbclust(podani_mat, pam,
                           nstart = 25,
                           method = "gap_stat",
                           nboot = 10
) + labs(subtitle = "Gap statistic method - Distancia de Podani")

#Numero optimo de cluster distancia de harikumar - Metodo del gap
gap_harikumar <- fviz_nbclust(harikumar_mat, pam,
                              nstart = 25,
                              method = "gap_stat",
                              nboot = 10
) + labs(subtitle = "Gap statistic method - Distancia de Harikumar")

gap <- ggarrange(gap_gower,gap_podani,gap_harikumar)

print(gap)


#Crear clusters ----

#K=4 ----

#PAM Clustering con k=4 y matriz de distancia gower
elbow_method_suggested_k <- 4
pam_fit_four_cluster_gower <- pam(gower_dist, diss = TRUE, elbow_method_suggested_k)
pam_results_four_cluster_gower <- data %>%mutate(cluster = pam_fit_four_cluster_gower$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_four_cluster_gower$the_summary

clusplot(gower_mat, pam_fit_four_cluster_gower$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#PAM Clustering con k=4 y matriz de distancia podani
pam_fit_four_cluster_podani <- pam(podani_dist, diss = TRUE, elbow_method_suggested_k)
pam_results_four_cluster_podani <- data %>%mutate(cluster = pam_fit_four_cluster_podani$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_four_cluster_podani$the_summary

clusplot(podani_mat, pam_fit_four_cluster_podani$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#PAM Clustering con k=4 y matriz de distancia harikumar
pam_fit_four_cluster_harikumar <- pam(harikumar_dist, diss = TRUE, elbow_method_suggested_k)
pam_results_four_cluster_harikumar <- data %>%mutate(cluster = pam_fit_four_cluster_harikumar$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_four_cluster_harikumar$the_summary

clusplot(harikumar_mat, pam_fit_four_cluster_harikumar$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#K=2 ----

#PAM Clustering con k=2 y matriz de distancia gower
silhouette_method_suggested_k <- 2
pam_fit_two_cluster_gower <- pam(gower_dist, diss = TRUE, silhouette_method_suggested_k)
pam_results_two_cluster_gower <- data %>%mutate(cluster = pam_fit_two_cluster_gower$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_two_cluster_gower$the_summary

clusplot(gower_mat, pam_fit_two_cluster_gower$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#PAM Clustering con k=2 y matriz de distancia podani
pam_fit_two_cluster_podani <- pam(podani_dist, diss = TRUE, silhouette_method_suggested_k)
pam_results_two_cluster_podani <- data %>%mutate(cluster = pam_fit_two_cluster_podani$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_two_cluster_podani$the_summary

clusplot(podani_mat, pam_fit_two_cluster_podani$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#PAM Clustering con k=2 y matriz de distancia harikumar
pam_fit_two_cluster_harikumar <- pam(harikumar_dist, diss = TRUE, silhouette_method_suggested_k)
pam_results_two_cluster_harikumar <- data %>%mutate(cluster = pam_fit_two_cluster_harikumar$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_two_cluster_harikumar$the_summary

clusplot(harikumar_mat, pam_fit_two_cluster_harikumar$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#K=9 ----

#PAM Clustering con k=9 y matriz de distancia gower
gap_method_suggested_k <- 9
pam_fit_nine_cluster_gower <- pam(gower_dist, diss = TRUE, gap_method_suggested_k)
pam_results_nine_cluster_gower <- data %>%mutate(cluster = pam_fit_nine_cluster_gower$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_nine_cluster_gower$the_summary

clusplot(gower_mat, pam_fit_nine_cluster_gower$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#PAM Clustering con k=9 y matriz de distancia podani
pam_fit_nine_cluster_podani <- pam(podani_dist, diss = TRUE, gap_method_suggested_k)
pam_results_nine_cluster_podani <- data %>%mutate(cluster = pam_fit_nine_cluster_podani$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_nine_cluster_podani$the_summary

clusplot(podani_mat, pam_fit_nine_cluster_podani$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#PAM Clustering con k=1 y matriz de distancia harikumar
pam_fit_nine_cluster_harikumar <- pam(harikumar_dist, diss = TRUE, 1)
pam_results_nine_cluster_harikumar <- data %>%mutate(cluster = pam_fit_nine_cluster_harikumar$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_nine_cluster_harikumar$the_summary

clusplot(harikumar_mat, pam_fit_nine_cluster_harikumar$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#Analisis ----
#data$clus <- as.factor(pam_fit_two_cluster$clustering)
#data$clus <- factor(data$clus)

#data_long <- gather(data, "Variable", "Valor", 1:18, factor_key=TRUE)
#ggplot(data_long, aes(as.factor(x = Variable), y = Valor,group=clus, colour = clus)) + 
#  stat_summary(fun = mean, geom="pointrange", size = 1, aes(shape = clus))+
#  stat_summary(geom="line")


#Dendograma
#dend <- hcut(gower_mat,k=4,stand = TRUE, method="median")
#fviz_dend(dend,rect=TRUE, cex=0.5,k_colors = "simpsons")


# Evaluamos cada clúster obtenido
# k = 2

#cluster_2_df <- data.frame(data, 
#                   class = attr_class,                    
#                   clust_k_2 = as.factor(pam_fit_two_cluster$clustering))

#cluster_2_group_1 <- cluster_2_df %>% filter(clust_k_2 == 1)
#cluster_2_group_2 <- cluster_2_df %>% filter(clust_k_2 == 2)

#print(pam_fit_two_cluster$clusinfo)


