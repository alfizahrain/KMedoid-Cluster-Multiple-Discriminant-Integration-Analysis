library(readxl)
library(cluster)
library("tidyverse")
library("fpc")
library("cluster")
library("factoextra")
library(MVN)  
library(dplyr)
library(MASS)   
library(biotools) 
library(openxlsx)
library(car)
library(recipes)
library(caret)
library(ggplot2)
datacl <- read_excel("D:/KULIAH/BISMILLAH 3.5/data fix insyaallah - Copy.xlsx", sheet = "CLUSTER")

# variabel cluster
penggunaan_berulang <- datacl[,1]
referensi_oranglain <- datacl[,2]
penolakan_produk <- datacl[,3]
kepuasan_penggunaan <- datacl[,4]
preferensi_fiturbaru <- datacl[,5]
persepsi_keamanan <- datacl[,6]

# K-Medoids
data_cl <- data.frame(penggunaan_berulang, referensi_oranglain, penolakan_produk, kepuasan_penggunaan, preferensi_fiturbaru, persepsi_keamanan)
colnames(data_cl) <- c("penggunaan_berulang", "referensi_oranglain", "penolakan_produk", "kepuasan_penggunaan", "preferensi_fiturbaru", "persepsi_keamanan")
view(data_cl)

# Deteksi Outlier dengan Boxplot
def.par <- par(mfrow=c(2,3))
for(i in 1:ncol(data_cl)) {
  boxplot(data_cl[,i], main=names(data_cl)[i])
}
par(def.par)

# Menghitung cluster menggunakan jarak Manhattan
pam_result <- pam(data_cl, k = 3, metric = "manhattan")
summary(pam_result)  # Ringkasan hasil PAM
means<-kmeans(data_cl,centers = 3)
pam_result$medoids   # Menampilkan medoids

# Hitung matriks dissimilarity secara terpisah
diss_matrix <- daisy(data_cl, metric = "manhattan")
# Lihat struktur dari diss_matrix
str(diss_matrix)

# Visualisasi cluster
fviz_cluster(pam_result, data = data_cl)

# Uji silhouette pada hasil PAM
pam.hasil <- pam(data_cl, 3, metric = "manhattan")
summary(pam.hasil)
pam.hasil$medoids
pam.hasil$diss
silhouette_score <- silhouette(pam.hasil$clustering, diss_matrix)
summary(silhouette_score)
plot(silhouette_score, col = 1:2, border = NA)

# Menghitung rata-rata untuk setiap cluster
clustered <- data_cl %>%
  mutate(cluster = pam_result$cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean")
clustered

# Menghitung anggota setiap cluster
cl_member <- data_cl %>%
  mutate(cluster = pam_result$cluster) %>%
  count(cluster)
print(cl_member)

# Menampilkan anggota setiap cluster 
for (cluster in unique(pam_result$cluster)) {
  cat("Anggota Cluster", cluster, ":\n")
  print(which(pam_result$cluster == cluster))
  cat("\n")
}

# Menampilkan ringkasan karakteristik untuk setiap cluster
cluster_characteristics <- split(data_cl, pam_result$cluster)
summary_list <- lapply(cluster_characteristics, summary)

# Menampilkan ringkasan
for (i in 1:length(summary_list)) {
  cat("Karakteristik Cluster", i, ":\n")
  print(summary_list[[i]])
  cat("\n")
}

# Mengubah hasil cluster menjadi variabel dependen MDA
cluster_kmedoids <- data_cl %>%
  mutate(pengguna_cashless = case_when(
    pam_result$cluster == 1 ~ "1",    # Cluster 1
    pam_result$cluster == 2 ~ "2",    # Cluster 2
    pam_result$cluster == 3 ~ "3"      # Cluster 3
  ))
head(cluster_kmedoids)

#MDA
datamda <- read_excel("D:/KULIAH/BISMILLAH 3.5/data fix insyaallah - Copy.xlsx", sheet = "MDA")
# Variabel mda
X1 <- rowMeans(datamda[,1:3])
X2 <- rowMeans(datamda[,4:6])
X3 <- rowMeans(datamda[,7:9])
X4 <- rowMeans(datamda[,10:12])
X5 <- rowMeans(datamda[,13:15])
data_mda <- data.frame(
  Y = cluster_kmedoids$pengguna_cashless,  # Variabel dependen
  X1 = X1,
  X2 = X2,
  X3 = X3,
  X4 = X4,
  X5 = X5
  )

# Membagi data training dan testing
train <- data_mda[1:160, ] 
test <- data_mda[161:200, ]

# Asumsi Normal Multivariat
# QQ Plot Kuantil Chi Square dan Jarak Mahalanobis
mu <- colMeans(train[, c(2:6)])  # Mean untuk variabel X1 hingga X5
sigma <- cov(train[, c(2:6)])    # Kovarian untuk variabel X1 hingga X5
dist <- mahalanobis(train[, c(2:6)], mu, sigma)  # Mahalanobis distance
df <- ncol(train[, c(2:6)])  # Derajat kebebasan berdasarkan jumlah variabel
n <- length(dist)
u <- ((1:n) - 0.5) / n
p <- qchisq(u, df)
distsorted <- sort(dist)
plot(distsorted, p,
     col = "black",
     xlab = "Squared Mahalanobis Distance",
     ylab = "Chi-Square Quantile",
     main = "QQ Plot of Mahalanobis Distance vs Chi-Square Quantiles")
abline(0, 1, col = "red")
# Uji normalitas multivariat menggunakan Mardia test
normal_test <- mvn(data = train[, c(2:6)], mvnTest = "mardia")
normal_test$multivariateNormality

# Asumsi Homogenitas Matriks Varian Kovarians 
homogen_test <- boxM(data = train[, c(2:6)], grouping = train[,1])
print(homogen_test)

# Model LDA
linearDA <- lda(Y ~ X1 + X2 + X3 + X4 + X5, data = train)
print(linearDA)

# Konstanta dan Koefisien Fungsi Model LDA
groupmean <- (linearDA$prior%*%fit$means) 
constant <- (groupmean%*%linearDA$scaling)
constant 
linearDA$scaling 

# Uji Wilks' Lambda
man <- manova(formula = cbind(X1, X2, X3, X4, X5) ~ Y, data = train)
summary(object = man, test = 'Wilks')
summary.aov(m)

# Buat model multivariat
library(candisc)
mlm_model <- lm(cbind(X1, X2, X3, X4, X5) ~ Y, data = train)
candisc_model <- candisc(mlm_model)
summary(candisc_model)

# Fungsi untuk menghitung Wilks' Lambda secara manual
compute_wilks <- function(response_var, grouping_var, data) {
  fit <- lm(as.formula(paste(response_var, "~", grouping_var)), data = data)
  residuals <- residuals(fit)
  SSR <- sum(residuals^2)
  SST <- sum((data[[response_var]] - mean(data[[response_var]]))^2)
  lambda <- SSR / SST
  return(lambda)
}

# Hitung Wilks' Lambda untuk setiap variabel
variables <- c("X1", "X2", "X3", "X4", "X5")
wilks_lambda <- sapply(variables, function(var) compute_wilks(var, "Y", train))

# Tampilkan hasil
names(wilks_lambda) <- variables
wilks_lambda

# Pembeda Terkuat
# Standardized Coefficients
raw_coef <- linearDA$scaling
sds_per_group <- apply(train[, c("X1", "X2", "X3", "X4", "X5")], 2, function(x) tapply(x, train$Y, sd))
group_sizes <- table(train$Y)  # Ukuran sampel per kelompok
pooled_sd <- sapply(1:ncol(sds_per_group), function(i) {
  sqrt(sum((sds_per_group[, i]^2) * (group_sizes - 1)) / (sum(group_sizes) - length(group_sizes)))
})
standardized_coef <- raw_coef / pooled_sd
standardized_coef
# Indeks Potensi
X <- train[, c("X1", "X2", "X3", "X4", "X5")]
Y <- train$Y
# Matriks kovarians antar kelas (Between-Class Covariance Matrix)
mean_class <- tapply(1:nrow(X), Y, function(i) colMeans(X[i, ]))
mean_total <- colMeans(X)
S_b <- matrix(0, ncol(X), ncol(X))
for (i in 1:length(unique(Y))) {
  Si <- X[Y == i, ]
  n_i <- nrow(Si)
  mean_class_i <- mean_class[[i]]
  mean_diff <- mean_class_i - mean_total
  S_b <- S_b + n_i * (mean_diff %*% t(mean_diff))
}
# Matriks kovarians dalam kelas (Within-Class Covariance Matrix)
S_w <- matrix(0, ncol(X), ncol(X))
for (i in 1:length(unique(Y))) {
  Si <- X[Y == i, ]
  mean_class_i <- mean_class[[i]]
  S_w <- S_w + cov(Si) * (nrow(Si) - 1)
}
# Matriks kovarians total (Total Covariance Matrix)
S_t <- S_b + S_w
# Eigen-decomposition
eigen_decomp <- eigen(solve(S_w) %*% S_b)
eigenvalues <- eigen_decomp$values  # Eigenvalue
eigenvectors <- eigen_decomp$vectors  # Eigenvectors
relative_eigenvalue <- eigenvalues / sum(eigenvalues)
raw_coef <- linearDA$scaling  # Koefisien tidak standar (raw coefficients)
potensi_variabel <- apply(raw_coef^2, 1, function(x) sum(x * relative_eigenvalue))
potensi_variabel

# Mendapatkan skor diskriminan untuk data training
lda_scores <- predict(linearDA)$x
lda_scores <- data.frame(lda_scores, Group = train$Y)

## Menentukan nilai tengah (centroid) untuk kelompok
# Menghitung skor diskriminan untuk data training
discriminant_scores <- predict(linearDA)$x
centroids <- aggregate(cbind(LD1, LD2) ~ Y, data = discriminant_scores, FUN = mean)
colnames(centroids)[1] <- "Centroid"
centroids

#Prediksi Data Testing
lda_pred <- predict(linearDA, test[,-1]) 
lda_pred

#Menghitung Akurasi, Sensitivitas, Spesifisitas
pred <- as.factor(lda_pred$class) 
reff <- as.factor(test$Y) 

## Classification
cfm <- confusionMatrix(pred,reff)
cfm

accuracy <- cfm$overall['Accuracy']
sensitivity <- cfm$byClass['Sensitivity']
specificity <- cfm$byClass['Specificity']
balanced_accuracy <- cfm$byClass['Balanced Accuracy']
cat("Accuracy: ", accuracy, "\n")
cat("Sensitivity: \n")
print(sensitivity)
cat("Specificity: \n")
print(specificity)
cat("Balanced Accuracy: \n")
print(balanced_accuracy)


# Extracting Sensitivity, Specificity, and Balanced Accuracy
sensitivity <- cfm$byClass[, "Sensitivity"]
specificity <- cfm$byClass[, "Specificity"]
balanced_accuracy <- cfm$byClass[, "Balanced Accuracy"]

# Displaying the values
cat("Sensitivity for each class:\n")
print(sensitivity)

cat("Specificity for each class:\n")
print(specificity)

cat("Balanced Accuracy for each class:\n")
print(balanced_accuracy)

# For overall model accuracy and other statistics:
accuracy <- cfm$overall['Accuracy']
cat("Overall Accuracy: ", accuracy, "\n")

# Prediksi data testing menggunakan model LDA
lda_test_scores <- predict(linearDA, newdata = test)
lda_test_scores <- data.frame(lda_test_scores$x, Group = test$Y)

# Tambahkan kolom untuk menandai jenis data (Training atau Testing)
lda_scores$Type <- "Training"
lda_test_scores$Type <- "Testing"

# Gabungkan data Training dan Testing
combined_scores <- rbind(lda_scores,lda_test_scores)

# Plot Territorial Map dengan penandaan data Testing
library(ggplot2)
ggplot(combined_scores, aes(x = LD1, y = LD2, color = as.factor(Group))) +
  # Plot data dengan simbol berbeda untuk Training dan Testing
  geom_point(data = subset(combined_scores, Type == "Training"), 
             aes(shape = "Training"), size = 3, alpha = 0.8) + 
  geom_point(data = subset(combined_scores, Type == "Testing"), 
             aes(shape = "Testing"), size = 3, alpha = 0.8) +
  # Kustomisasi label, tema, dan legenda
  labs(title = NULL,
       x = "Discriminant Scores from Function 1",
       y = "Discriminant Scores from Function 2",
       color = "Group",
       shape = "Data Type") +
  theme_minimal() +
  scale_color_discrete(name = "Group") +
  scale_fill_discrete(name = "Group") +
  scale_shape_manual(values = c("Training" = 1, "Testing" = 17)) + # Menentukan bentuk simbol
  theme(
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1) # Garis tebal di pinggiran
  )

# 2. Scatterplot untuk menunjukkan distribusi data dalam ruang diskriminan
# Menentukan klasifikasi yang benar dan salah
lda_test_scores$Group <- as.factor(lda_test_scores$Group)
lda_pred$class <- as.factor(lda_pred$class)
# Menentukan klasifikasi yang benar dan salah
test_correct <- lda_test_scores$Group == lda_pred$class

#Plot Territorial Map Data Testing
# Menambahkan kolom test_correct ke dalam lda_test_scores
lda_test_scores$Pred <- test_correct
# Plot Territorial Map
ggplot(lda_test_scores, aes(x = LD1, y = LD2, color = Pred, shape = as.factor(Group))) +
  geom_point(size = 3, alpha = 0.8) +  
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "red")) +  # Warna hitam untuk prediksi benar dan merah untuk prediksi salah
  scale_shape_manual(values = c(1, 2, 3)) +  # Bentuk titik untuk setiap kelompok (1 = bulat, 2 = segitiga, 3 = kotak)
  labs(
    title = "Territorial Map (Data Testing)",
    x = "Discriminant Scores Function 1 (LD1)",
    y = "Discriminant Scores Function 2 (LD2)",
    color = "Prediction Accuracy",
    shape = "Group"
  ) +
  guides(
    shape = guide_legend(title = "Group", override.aes = list(size = 3)),  
    color = guide_legend(title = "Prediction Accuracy")  
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Garis tepi plot
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
  ) +
  xlim(-8, 8) +  
  ylim(-8, 8)   



