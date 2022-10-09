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
library(RColorBrewer)
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

#K=2 ----

#PAM Clustering con k=2 y matriz de distancia gower
silhouette_method_suggested_k <- 2
pam_fit_two_cluster_gower <- pam(gower_dist, diss = TRUE, silhouette_method_suggested_k)
pam_results_two_cluster_gower <- data %>%mutate(cluster = pam_fit_two_cluster_gower$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_two_cluster_gower$the_summary

clusplot(gower_mat, pam_fit_two_cluster_gower$cluster, color=TRUE, labels=0, lines=0)

#PAM Clustering con k=2 y matriz de distancia podani
pam_fit_two_cluster_podani <- pam(podani_dist, diss = TRUE, silhouette_method_suggested_k)
pam_results_two_cluster_podani <- data %>%mutate(cluster = pam_fit_two_cluster_podani$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_two_cluster_podani$the_summary

clusplot(podani_mat, pam_fit_two_cluster_podani$cluster, color=TRUE, labels=0, lines=0)

#PAM Clustering con k=2 y matriz de distancia harikumar
pam_fit_two_cluster_harikumar <- pam(harikumar_dist, diss = TRUE, silhouette_method_suggested_k)
pam_results_two_cluster_harikumar <- data %>%mutate(cluster = pam_fit_two_cluster_harikumar$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_two_cluster_harikumar$the_summary

clusplot(harikumar_mat, pam_fit_two_cluster_harikumar$cluster, color=TRUE, labels=0, lines=0)

#K=4 ----

#PAM Clustering con k=4 y matriz de distancia gower
elbow_method_suggested_k <- 4
pam_fit_four_cluster_gower <- pam(gower_dist, diss = TRUE, elbow_method_suggested_k)
pam_results_four_cluster_gower <- data %>%mutate(cluster = pam_fit_four_cluster_gower$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_four_cluster_gower$the_summary

clusplot(gower_mat, pam_fit_four_cluster_gower$cluster, color=TRUE, labels=0, lines=0)

#PAM Clustering con k=4 y matriz de distancia podani
pam_fit_four_cluster_podani <- pam(podani_dist, diss = TRUE, elbow_method_suggested_k)
pam_results_four_cluster_podani <- data %>%mutate(cluster = pam_fit_four_cluster_podani$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_four_cluster_podani$the_summary

clusplot(podani_mat, pam_fit_four_cluster_podani$cluster, color=TRUE, labels=0, lines=0)

#PAM Clustering con k=4 y matriz de distancia harikumar
pam_fit_four_cluster_harikumar <- pam(harikumar_dist, diss = TRUE, elbow_method_suggested_k)
pam_results_four_cluster_harikumar <- data %>%mutate(cluster = pam_fit_four_cluster_harikumar$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_four_cluster_harikumar$the_summary

clusplot(harikumar_mat, pam_fit_four_cluster_harikumar$cluster, color=TRUE, labels=0, lines=0)

#K=9 ----

#PAM Clustering con k=9 y matriz de distancia gower
gap_method_suggested_k <- 9
pam_fit_nine_cluster_gower <- pam(gower_dist, diss = TRUE, gap_method_suggested_k)
pam_results_nine_cluster_gower <- data %>%mutate(cluster = pam_fit_nine_cluster_gower$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_nine_cluster_gower$the_summary

clusplot(gower_mat, pam_fit_nine_cluster_gower$cluster, color=TRUE, labels=0, lines=0)

#PAM Clustering con k=9 y matriz de distancia podani
pam_fit_nine_cluster_podani <- pam(podani_dist, diss = TRUE, gap_method_suggested_k)
pam_results_nine_cluster_podani <- data %>%mutate(cluster = pam_fit_nine_cluster_podani$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_nine_cluster_podani$the_summary

clusplot(podani_mat, pam_fit_nine_cluster_podani$cluster, color=TRUE, labels=0, lines=0)

#PAM Clustering con k=1 y matriz de distancia harikumar
pam_fit_nine_cluster_harikumar <- pam(harikumar_dist, diss = TRUE, 1)
pam_results_nine_cluster_harikumar <- data %>%mutate(cluster = pam_fit_nine_cluster_harikumar$clustering) %>%group_by(cluster) %>%do(the_summary = summary(.))
pam_results_nine_cluster_harikumar$the_summary

clusplot(harikumar_mat, pam_fit_nine_cluster_harikumar$cluster, color=TRUE, labels=0, lines=0)

#Analisis ----

#Dendograma
dend <- hcut(harikumar_mat,k=2,stand = TRUE, method="median")
fviz_dend(dend,rect=TRUE, cex=0.5,k_colors = "RdGy")

data$clus <- as.factor(pam_fit_two_cluster_harikumar$clustering)
data$clus <- factor(data$clus)

#Registros dentro del cluster 1
group_one <- data%>%filter(clus==1)

#Registros dentro del cluster 2
group_two <- data%>%filter(clus==2)

#Barplot variable Sex por grupo del cluster
sex_group_one <-ggplot(group_one, aes(x=Sex, fill=Sex )) + 
                geom_bar( ) +
                scale_fill_hue(c = 40) +
                theme(legend.position="none") + ggtitle("Grupo 1")

sex_group_two <-ggplot(group_two, aes(x=Sex, fill=Sex )) + 
                geom_bar( ) +
                scale_fill_hue(c = 40) +
                theme(legend.position="none") + ggtitle("Grupo 2")

sex_cluster <- ggarrange(sex_group_one,sex_group_two,ncol=2,nrow=1)
sex_cluster

#Barplot variable Steroid por grupo del cluster
steroid_group_one <-ggplot(group_one, aes(x=Steroid, fill=Steroid )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")+ggtitle("Grupo 1")

steroid_group_two <-ggplot(group_two, aes(x=Steroid, fill=Steroid)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")+ggtitle("Grupo 2")

steroid_cluster <- ggarrange(steroid_group_one,steroid_group_two,ncol=2,nrow=1)
steroid_cluster

steroid_sex <- ggarrange(sex_cluster,steroid_cluster, ncol = 1, nrow = 2)
steroid_sex

#Barplot variable Antivirals por grupo del cluster
antivirals_group_one <-ggplot(group_one, aes(x=Antivirals, fill=Antivirals)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

antivirals_group_two <-ggplot(group_two, aes(x=Antivirals, fill=Antivirals)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

antivirals_cluster <- ggarrange(antivirals_group_one,antivirals_group_two,ncol=2,nrow=1)
antivirals_cluster

#Barplot variable Fatigue por grupo del cluster
fatigue_group_one <-ggplot(group_one, aes(x=Fatigue, fill=Fatigue)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

fatigue_group_two <-ggplot(group_two, aes(x=Fatigue, fill=Fatigue)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

fatigue_cluster <- ggarrange(fatigue_group_one,fatigue_group_two,ncol=2,nrow=1)
fatigue_cluster

antivirals_fatigue <- ggarrange(antivirals_cluster,fatigue_cluster, ncol = 1, nrow = 2)
antivirals_fatigue

#Barplot variable Malaise por grupo del cluster
malaise_group_one <-ggplot(group_one, aes(x=Malaise, fill=Malaise)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

malaise_group_two <-ggplot(group_two, aes(x=Malaise, fill=Malaise)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

malaise_cluster <- ggarrange(malaise_group_one,malaise_group_two,ncol=2,nrow=1)
malaise_cluster

#Barplot variable Anorexia por grupo del cluster
anorexia_group_one <-ggplot(group_one, aes(x=Anorexia, fill=Anorexia)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

anorexia_group_two <-ggplot(group_two, aes(x=Anorexia, fill=Anorexia)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

anorexia_cluster <- ggarrange(anorexia_group_one,anorexia_group_two,ncol=2,nrow=1)
anorexia_cluster

malaise_anorexia <- ggarrange(malaise_cluster,anorexia_cluster, ncol = 1, nrow = 2)
malaise_anorexia

#Barplot variable Liver big por grupo del cluster
liverbig_group_one <-ggplot(group_one, aes(x=Liver_Big, fill=Liver_Big)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

liverbig_group_two <-ggplot(group_two, aes(x=Liver_Big, fill=Liver_Big)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

liverbig_cluster <- ggarrange(liverbig_group_one,liverbig_group_two,ncol=2,nrow=1)
liverbig_cluster

#Barplot variable Liver firm por grupo del cluster
liverfirm_group_one <-ggplot(group_one, aes(x=Liver_Firm, fill=Liver_Firm)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

liverfirm_group_two <-ggplot(group_two, aes(x=Liver_Firm, fill=Liver_Firm)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

liverfirm_cluster <- ggarrange(liverfirm_group_one,liverfirm_group_two,ncol=2,nrow=1)
liverfirm_cluster

liverbig_liverfirm <- ggarrange(liverbig_cluster,liverfirm_cluster, ncol = 1, nrow = 2)
liverbig_liverfirm

#Barplot variable Spleen palpable por grupo del cluster
spleen_group_one <-ggplot(group_one, aes(x=Spleen_Palpable, fill=Spleen_Palpable)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

spleen_group_two <-ggplot(group_two, aes(x=Spleen_Palpable, fill=Spleen_Palpable)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

spleen_cluster <- ggarrange(spleen_group_one,spleen_group_two,ncol=2,nrow=1)
spleen_cluster

#Barplot variable Spiders por grupo del cluster
spiders_group_one <-ggplot(group_one, aes(x=Spiders, fill=Spiders)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

spiders_group_two <-ggplot(group_two, aes(x=Spiders, fill=Spiders)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

spiders_cluster <- ggarrange(spiders_group_one,spiders_group_two,ncol=2,nrow=1)
spiders_cluster

spleen_spiders <- ggarrange(spleen_cluster,spiders_cluster, ncol = 1, nrow = 2)
spleen_spiders

#Barplot variable Ascites por grupo del cluster
ascites_group_one <-ggplot(group_one, aes(x=Ascites, fill=Ascites)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

ascites_group_two <-ggplot(group_two, aes(x=Ascites, fill=Ascites)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

ascites_cluster <- ggarrange(ascites_group_one,ascites_group_two,ncol=2,nrow=1)
ascites_cluster

#Barplot variable Varices por grupo del cluster
varices_group_one <-ggplot(group_one, aes(x=Varices, fill=Varices)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

varices_group_two <-ggplot(group_two, aes(x=Varices, fill=Varices)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

varices_cluster <- ggarrange(varices_group_one,varices_group_two,ncol=2,nrow=1)
varices_cluster

ascites_varices <- ggarrange(ascites_cluster,varices_cluster, ncol = 1, nrow = 2)
ascites_varices

#Barplot variable Histology por grupo del cluster
histology_group_one <-ggplot(group_one, aes(x=Histology, fill=Histology)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")+ ggtitle("Grupo 1")

histology_group_two <-ggplot(group_two, aes(x=Histology, fill=Histology)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none") + ggtitle("Grupo 2")

histology_cluster <- ggarrange(histology_group_one,histology_group_two,ncol=2,nrow=1)
histology_cluster


