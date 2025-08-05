## Uji Validitas dan Reabilitas
library(readxl) 
library(psych) 
library(dplyr)

data <- read_excel("D:/kuliah nasywa/skripsi/Stunting Dadapan.xlsx")
pengetahuan_ibu <- (data[1:30,1:4])
tingkat_ekonomi <- (data[1:30,5:9])
pola_asuh <- (data[1:30,10:15])
pola_makan_anak <- (data[1:30,16:24])
lingkungan <- (data[1:30,25:33])
status_gizi_ibu <- (data[1:30,34:39])
layanan_kesehatan <- (data[1:30,40:51])
sosial_dan_dukungan_keluarga <- (data[1:30,52:63])

psych::alpha(pengetahuan_ibu)
psych::alpha(tingkat_ekonomi)
psych::alpha(pola_asuh)
psych::alpha(pola_makan_anak)
psych::alpha(lingkungan)
psych::alpha(status_gizi_ibu)
psych::alpha(layanan_kesehatan)
psych::alpha(sosial_dan_dukungan_keluarga)

## Penskalaan SRS
freq <- apply(data, 2, function(x) table(factor(x, levels = 1:5)))
proporsi <- prop.table(freq, margin = 2)
proporsi_kumulatif <- apply(proporsi, 2, cumsum)
nilai_tengah_proporsi <- apply(proporsi_kumulatif, 2, function(x) (x + c(0, head(x, -1))) / 2)
nilai_kritis_Z <- qnorm(nilai_tengah_proporsi)
min_Z <- min(nilai_kritis_Z, na.rm = TRUE)
hasil_skala <- nilai_kritis_Z - min_Z
hasil_skala

data_skala <- data
for (i in 1:ncol(data)) {
  for (likert_value in 1:5) {
    data_skala[data[, i] == likert_value, i] <- hasil_skala[likert_value, i]
  }
}
data_skala

## Metode rata-rata skor
# pengetahuan ibu
Y1_indikator1 <- rowMeans(data_skala[, 1:2])
Y1_indikator2 <- rowMeans(data_skala[, 3:4])
pengetahuan_ibu <- rowMeans(cbind(Y1_indikator1, Y1_indikator2))
pengetahuan_ibu

# tingkat ekonomi
Y2_indikator1 <- rowMeans(data_skala[, 5:6])
Y2_indikator2 <- rowMeans(data_skala[, 7:9])
tingkat_ekonomi <- rowMeans(cbind(Y2_indikator1, Y2_indikator2))
tingkat_ekonomi

# pola asuh
Y3_indikator1 <- rowMeans(data_skala[, 10:12])
Y3_indikator2 <- rowMeans(data_skala[, 13:15])
pola_asuh <- rowMeans(cbind(Y3_indikator1, Y3_indikator2))
pola_asuh

# pola makan anak
Y4_indikator1 <- rowMeans(data_skala[, 16:18])
Y4_indikator2 <- rowMeans(data_skala[, 19:21])
Y4_indikator3 <- rowMeans(data_skala[, 22:24])
pola_makan_anak <- rowMeans(cbind(Y4_indikator1, Y4_indikator2, Y4_indikator3))
pola_makan_anak

# lingkungan
Y5_indikator1 <- rowMeans(data_skala[, 25:27])
Y5_indikator2 <- rowMeans(data_skala[, 28:30])
Y5_indikator3 <- rowMeans(data_skala[, 31:33])
lingkungan <- rowMeans(cbind(Y5_indikator1, Y5_indikator2, Y5_indikator3))
lingkungan

# status gizi ibu
X1_indikator1 <- rowMeans(data_skala[, 34:36])
X1_indikator2 <- rowMeans(data_skala[, 37:39])
status_gizi_ibu <- rowMeans(cbind(X1_indikator1, X1_indikator2))
status_gizi_ibu

# layanan kesehatan
X2_indikator1 <- rowMeans(data_skala[, 40:42])
X2_indikator2 <- rowMeans(data_skala[, 43:45])
X2_indikator3 <- rowMeans(data_skala[, 46:48])
X2_indikator4 <- rowMeans(data_skala[, 49:51])
layanan_kesehatan <- rowMeans(cbind(X2_indikator1, X2_indikator2, X2_indikator3, X2_indikator4))
layanan_kesehatan

# sosial dan dukungan keluarga
X3_indikator1 <- rowMeans(data_skala[, 52:55])
X3_indikator2 <- rowMeans(data_skala[, 56:59])
X3_indikator3 <- rowMeans(data_skala[, 60:63])
sosial_dan_dukungan_keluarga <- rowMeans(cbind(X3_indikator1, X3_indikator2, X3_indikator3))
sosial_dan_dukungan_keluarga

# data baru
data_baru <- data.frame(pengetahuan_ibu, tingkat_ekonomi, pola_asuh, pola_makan_anak, lingkungan, 
                        status_gizi_ibu, layanan_kesehatan, sosial_dan_dukungan_keluarga)
data_baru

## Integrasi 1
# cluster K-Means
library(factoextra)
data_cluster <- data.frame(pengetahuan_ibu, tingkat_ekonomi, pola_asuh, pola_makan_anak, lingkungan)
kmeans <- kmeans(data_cluster,centers=2) 
kmeans
kmeans$centers #centroid
kmeans$iter #iterasi
fviz_cluster(kmeans, data=data_cluster) 

##Karakteristik setiap cluster  
n = dim(data_cluster)[1] 
idclus = kmeans$cluster 
c1 = c() 
c2 = c() 
for (i in 1:n) {  
  if(idclus[i] == 1){ 
    c1 = c(c1,i) 
  }  
  else if (idclus[i] == 2){ 
    c2 = c(c2,i) 
  } 
}  
clustering = list(Cluster1 = c1, Cluster2 = c2)  
nclust = list(nCluster1 = length(c1), 
              nCluster2 = length(c2))  
clustering 
nclust 
dxc1 = data_cluster[c1,] 
summary (dxc1) 
dxc2 = data_cluster[c2,] 
summary (dxc2) 

# Membuat variabel baru "risiko_stunting" dari hasil clustering
data_cluster_kmeans <- data_cluster
data_cluster_kmeans$risiko_stunting <- ifelse(kmeans$cluster == 1, 1, -1)
head(data_cluster_kmeans$risiko_stunting)
table(data_cluster_kmeans$risiko_stunting)

#SVM polinomial kuadratik
library(caTools) 
library(e1071) 
library(ggplot2)
library (openxlsx) 

data_svm <- data.frame(status_gizi_ibu, layanan_kesehatan, sosial_dan_dukungan_keluarga, data_cluster_kmeans$risiko_stunting) 
X1 <- data_svm$status_gizi_ibu
X2 <- data_svm$layanan_kesehatan
X3 <- data_svm$sosial_dan_dukungan_keluarga
Y <- data_svm$data_cluster_kmeans.risiko_stunting
data_svm <- data.frame(X1, X2, X3, Y)

training<-data_svm[1:80, ] 
testing<-data_svm[81:100, ]

#Polinomial Kuadratik
model_kmeans_pol2 <- svm(Y ~ X1+X2+X3, data=training, type= "C-classification", 
                  kernel = 'polynomial', 
                  degree=2,  
                  coef0=1, 
                  gamma=1, 
                  scale=FALSE) 
matriks_SV_kmeans_pol2 <- cbind(1, 
                                sqrt(2)*model_kmeans_pol2$SV[,1],
                                sqrt(2)*model_kmeans_pol2$SV[,2], 
                                (model_kmeans_pol2$SV[,1])^2,
                                (model_kmeans_pol2$SV[,2])^2, 
                                sqrt(2)*model_kmeans_pol2$SV[,1]*model_kmeans_pol2$SV[,2]) 
b_kmeans_pol2 = -model_kmeans_pol2$rho 
weight_kmeans_pol2 = t(model_kmeans_pol2$coefs) %*% matriks_SV_kmeans_pol2 
as.data.frame(weight_kmeans_pol2) 
b_kmeans_pol2 
head(model_kmeans_pol2$decision.values) 
head(model_kmeans_pol2$fitted) 

# Training
pred_training_kmeans_pol2 <- predict(model_kmeans_pol2, training)
table_training_kmeans_pol2 <- table(training$Y, pred_training_kmeans_pol2) 
cat("Confusion Matrix - Training:\n")
print(table_training_kmeans_pol2)

# Testing
pred_testing_kmeans_pol2 <- predict(model_kmeans_pol2, testing)
table_testing_kmeans_pol2 <- table(testing$Y, pred_testing_kmeans_pol2) 
cat("Confusion Matrix - Testing:\n")
print(table_testing_kmeans_pol2)

# Menghitung Akurasi, Spesifisitas, dan Sensitivitas
# Fungsi untuk menghitung metrik
calculate_metrics <- function(conf_matrix) {
  # True Positives (TP), True Negatives (TN), False Positives (FP), False Negatives (FN)
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[2, 1]
  FN <- conf_matrix[1, 2]
  
  # Akurasi
  accuracy <- (TP + TN) / sum(conf_matrix)
  
  # Sensitivitas (Recall)
  sensitivity <- TP / (TP + FN)
  
  # Spesifisitas
  specificity <- TN / (TN + FP)
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

# Menghitung metrik pada data training
metrics_train1 <- calculate_metrics(table_training_kmeans_pol2)
cat("Training Data Metrics:\n")
cat("Akurasi:", metrics_train1$accuracy, "\n")
cat("Sensitivitas:", metrics_train1$sensitivity, "\n")
cat("Spesifisitas:", metrics_train1$specificity, "\n")

# Menghitung metrik pada data testing
metrics_test1 <- calculate_metrics(table_testing_kmeans_pol2)
cat("Testing Data Metrics:\n")
cat("Akurasi:", metrics_test1$accuracy, "\n")
cat("Sensitivitas:", metrics_test1$sensitivity, "\n")
cat("Spesifisitas:", metrics_test1$specificity, "\n")

# Integrasi 2
# Polinomial Kubik
model_kmeans_pol3 <- svm(Y ~ X1+X2+X3, data=training, 
                    type= "C-classification" , 
                    kernel = 'polynomial', 
                    degree = 3, 
                    coef0=1, 
                    gamma=1, 
                    scale=FALSE) 
b_kmeans_pol3= -model_kmeans_pol3$rho 
matriks_SV_kmeans_pol3 <- cbind(1, 
                           sqrt(3)*model_kmeans_pol3$SV[,1], 
                           sqrt(3)*model_kmeans_pol3$SV[,2], 
                           sqrt(3)*model_kmeans_pol3$SV[,1]^2, 
                           sqrt(3)*model_kmeans_pol3$SV[,2]^2, 
                           sqrt(6)*model_kmeans_pol3$SV[,1]*model_kmeans_pol3$SV[,2], 
                           model_kmeans_pol3$SV[,1]^3, 
                           model_kmeans_pol3$SV[,2]^3, 
                           sqrt(3)*model_kmeans_pol3$SV[,1]^2*model_kmeans_pol3$SV[,2],
                           sqrt(3)*model_kmeans_pol3$SV[,1]*model_kmeans_pol3$SV[,2]^2) 
weight_kmeans_pol3 = t(model_kmeans_pol3$coefs) %*% 
matriks_SV_kmeans_pol3 
as.data.frame(weight_kmeans_pol3) 
b_kmeans_pol3 
head(model_kmeans_pol3$decision.values) 
head(model_kmeans_pol3$fitted)

# Training
pred_training_kmeans_pol3 <- predict(model_kmeans_pol3, training)
table_training_kmeans_pol3 <- table(training$Y, pred_training_kmeans_pol3) 
cat("Confusion Matrix - Training:\n")
print(table_training_kmeans_pol3)

# Testing
pred_testing_kmeans_pol3 <- predict(model_kmeans_pol3, testing)
table_testing_kmeans_pol3 <- table(testing$Y, pred_testing_kmeans_pol3) 
cat("Confusion Matrix - Testing:\n")
print(table_testing_kmeans_pol3)

# Menghitung Akurasi, Spesifisitas, dan Sensitivitas
# Fungsi untuk menghitung metrik
calculate_metrics <- function(conf_matrix) {
  # True Positives (TP), True Negatives (TN), False Positives (FP), False Negatives (FN)
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[2, 1]
  FN <- conf_matrix[1, 2]
  
  # Akurasi
  accuracy <- (TP + TN) / sum(conf_matrix)
  
  # Sensitivitas (Recall)
  sensitivity <- TP / (TP + FN)
  
  # Spesifisitas
  specificity <- TN / (TN + FP)
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

# Menghitung metrik pada data training
metrics_train2 <- calculate_metrics(table_training_kmeans_pol3)
cat("Training Data Metrics:\n")
cat("Akurasi:", metrics_train2$accuracy, "\n")
cat("Sensitivitas:", metrics_train2$sensitivity, "\n")
cat("Spesifisitas:", metrics_train2$specificity, "\n")

# Menghitung metrik pada data testing
metrics_test2 <- calculate_metrics(table_testing_kmeans_pol3)
cat("Testing Data Metrics:\n")
cat("Akurasi:", metrics_test2$accuracy, "\n")
cat("Sensitivitas:", metrics_test2$sensitivity, "\n")
cat("Spesifisitas:", metrics_test2$specificity, "\n")

# K-Medoids
library("tidyverse")
library("fpc")
library("cluster")
library("factoextra")

data_cluster <- data.frame(pengetahuan_ibu, tingkat_ekonomi, pola_asuh, pola_makan_anak, lingkungan)

pamk.hasil <- pamk(data_cluster)
pamk.hasil
pamk.hasil$nc  # Menampilkan jumlah cluster optimal

pam.hasil <- pam(data_cluster, 2)
summary(pam.hasil)  # Ringkasan hasil PAM
pam.hasil$medoids   # Menampilkan medoids
pam.hasil$diss      # Dissimilarity matrix

fviz_cluster(pam.hasil, data = data_cluster)

clustered_data <- data_cluster %>%
  mutate(Cluster = pam.hasil$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
clustered_data

cluster_members <- data_cluster %>%
  mutate(Cluster = pam.hasil$cluster) %>%
  count(Cluster)
print(cluster_members)

# Menampilkan anggota setiap cluster 
for (cluster in unique(pam.hasil$cluster)) {
  cat("Anggota Cluster", cluster, ":\n")
  print(which(pam.hasil$cluster == cluster))
  cat("\n")
}

# Mengubah hasil cluster menjadi variabel risiko stunting
data_cluster_kmedoids <- data_cluster %>%
  mutate(risiko_stunting = ifelse(pam.hasil$cluster == 1, 1, -1))
head(data_cluster_kmedoids)

#SVM polinomial kuadratik
library(caTools) 
library(e1071) 
library(ggplot2)
library (openxlsx) 

data_svm1 <- data.frame(status_gizi_ibu, layanan_kesehatan, sosial_dan_dukungan_keluarga, data_cluster_kmedoids$risiko_stunting) 
X1 <- data_svm1$status_gizi_ibu
X2 <- data_svm1$layanan_kesehatan
X3 <- data_svm1$sosial_dan_dukungan_keluarga
Y <- data_svm1$data_cluster_kmedoids.risiko_stunting
data_svm1 <- data.frame(X1, X2, X3, Y)

training1 <- data_svm1[1:80, ] 
testing1 <- data_svm1[81:100, ]

# Integrasi 3
# Polinomial Kuadratik
model_kmedoids_pol2 <- svm(Y ~ X1+X2+X3, data=training1, type= "C-classification", 
                      kernel = 'polynomial', 
                      degree=2,  
                      coef0=1, 
                      gamma=1, 
                      scale=FALSE) 
matriks_SV_kmedoids_pol2 <- cbind(1, 
                                  sqrt(2)*model_kmedoids_pol2$SV[,1],
                                  sqrt(2)*model_kmedoids_pol2$SV[,2], 
                                  (model_kmedoids_pol2$SV[,1])^2,
                                  (model_kmedoids_pol2$SV[,2])^2, 
                                  sqrt(2)*model_kmedoids_pol2$SV[,1]*model_kmedoids_pol2$SV[,2]) 
b_kmedoids_pol2 = -model_kmedoids_pol2$rho 
weight_kmedoids_pol2 = t(model_kmedoids_pol2$coefs) %*% matriks_SV_kmedoids_pol2 
as.data.frame(weight_kmedoids_pol2) 
b_kmedoids_pol2 
head(model_kmedoids_pol2$decision.values) 
head(model_kmedoids_pol2$fitted) 

# Training
pred_training_kmedoids_pol2 <- predict(model_kmedoids_pol2, training1)
table_training_kmedoids_pol2 <- table(training1$Y, pred_training_kmedoids_pol2) 
cat("Confusion Matrix - Training:\n")
print(table_training_kmedoids_pol2)

# Testing
pred_testing_kmedoids_pol2 <- predict(model_kmedoids_pol2, testing1)
table_testing_kmedoids_pol2 <- table(testing1$Y, pred_testing_kmedoids_pol2) 
cat("Confusion Matrix - Testing:\n")
print(table_testing_kmedoids_pol2)

# Menghitung Akurasi, Spesifisitas, dan Sensitivitas
# Fungsi untuk menghitung metrik
calculate_metrics <- function(conf_matrix) {
  # True Positives (TP), True Negatives (TN), False Positives (FP), False Negatives (FN)
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[2, 1]
  FN <- conf_matrix[1, 2]
  
  # Akurasi
  accuracy <- (TP + TN) / sum(conf_matrix)
  
  # Sensitivitas (Recall)
  sensitivity <- TP / (TP + FN)
  
  # Spesifisitas
  specificity <- TN / (TN + FP)
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

# Menghitung metrik pada data training
metrics_train3 <- calculate_metrics(table_training_kmedoids_pol2)
cat("Training Data Metrics:\n")
cat("Akurasi:", metrics_train3$accuracy, "\n")
cat("Sensitivitas:", metrics_train3$sensitivity, "\n")
cat("Spesifisitas:", metrics_train3$specificity, "\n")

# Menghitung metrik pada data testing
metrics_test3 <- calculate_metrics(table_testing_kmedoids_pol2)
cat("Testing Data Metrics:\n")
cat("Akurasi:", metrics_test3$accuracy, "\n")
cat("Sensitivitas:", metrics_test3$sensitivity, "\n")
cat("Spesifisitas:", metrics_test3$specificity, "\n")

# Integrasi 4
# Polinomial Kubik
model_kmedoids_pol3 <- svm(Y ~ X1+X2+X3, data=training1, 
                  type= "C-classification" , 
                  kernel = 'polynomial', 
                  degree = 3, 
                  coef0=1, 
                  gamma=1, 
                  scale=FALSE) 
b_kmedoids_pol3= -model_kmedoids_pol3$rho 
matriks_SV_kmedoids_pol3 <- cbind(1, 
                         sqrt(3)*model_kmedoids_pol3$SV[,1], 
                         sqrt(3)*model_kmedoids_pol3$SV[,2], 
                         sqrt(3)*model_kmedoids_pol3$SV[,1]^2, 
                         sqrt(3)*model_kmedoids_pol3$SV[,2]^2, 
                         sqrt(6)*model_kmedoids_pol3$SV[,1]*model_kmedoids_pol3$SV[,2], 
                         model_kmedoids_pol3$SV[,1]^3, 
                         model_kmedoids_pol3$SV[,2]^3, 
                         sqrt(3)*model_kmedoids_pol3$SV[,1]^2*model_kmedoids_pol3$SV[,2],
                         sqrt(3)*model_kmedoids_pol3$SV[,1]*model_kmedoids_pol3$SV[,2]^2) 
weight_kmedoids_pol3 = t(model_kmedoids_pol3$coefs) %*% 
  matriks_SV_kmedoids_pol3 
as.data.frame(weight_kmedoids_pol3) 
b_kmedoids_pol3 
head(model_kmedoids_pol3$decision.values) 
head(model_kmedoids_pol3$fitted)

# Training
pred_training_kmedoids_pol3 <- predict(model_kmedoids_pol3, training1)
table_training_kmedoids_pol3 <- table(training1$Y, pred_training_kmedoids_pol3) 
cat("Confusion Matrix - Training:\n")
print(table_training_kmedoids_pol3)

# Testing
pred_testing_kmedoids_pol3 <- predict(model_kmedoids_pol3, testing1)
table_testing_kmedoids_pol3 <- table(testing1$Y, pred_testing_kmedoids_pol3) 
cat("Confusion Matrix - Testing:\n")
print(table_testing_kmedoids_pol3)

# Menghitung Akurasi, Spesifisitas, dan Sensitivitas
# Fungsi untuk menghitung metrik
calculate_metrics <- function(conf_matrix) {
  # True Positives (TP), True Negatives (TN), False Positives (FP), False Negatives (FN)
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[2, 1]
  FN <- conf_matrix[1, 2]
  
  # Akurasi
  accuracy <- (TP + TN) / sum(conf_matrix)
  
  # Sensitivitas (Recall)
  sensitivity <- TP / (TP + FN)
  
  # Spesifisitas
  specificity <- TN / (TN + FP)
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}

# Menghitung metrik pada data training
metrics_train4 <- calculate_metrics(table_training_kmedoids_pol3)
cat("Training Data Metrics:\n")
cat("Akurasi:", metrics_train4$accuracy, "\n")
cat("Sensitivitas:", metrics_train4$sensitivity, "\n")
cat("Spesifisitas:", metrics_train4$specificity, "\n")

# Menghitung metrik pada data testing
metrics_test4 <- calculate_metrics(table_testing_kmedoids_pol3)
cat("Testing Data Metrics:\n")
cat("Akurasi:", metrics_test4$accuracy, "\n")
cat("Sensitivitas:", metrics_test4$sensitivity, "\n")
cat("Spesifisitas:", metrics_test$specificity, "\n")