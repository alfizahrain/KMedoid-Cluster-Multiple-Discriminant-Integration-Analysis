# Cashless Society Segmentation Project
# Author: Alfi Zahrain
# Description: This script performs customer segmentation using K-Medoids clustering and validates it using Multiple Discriminant Analysis (MDA).

# ---------------------
# 1. Load Libraries
# ---------------------

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

# ---------------------
# 2. Load and Prepare Data
# ---------------------
# Load dataset (replace with your own data path)
cluster_data <- read_excel("data/cashless_data.xlsx", sheet = "CLUSTER")

# Rename and select relevant variables
data_cl <- cluster_data[, c("repeat_usage", "referral", "product_rejection", 
                            "usage_satisfaction", "feature_preference", "security_perception")]

# Outlier Detection with Boxplot
def.par <- par(mfrow=c(2,3))
for(i in 1:ncol(data_cl)) {
  boxplot(data_cl[,i], main=names(data_cl)[i])
}
par(def.par)

# ---------------------
# 3. K-Medoids Clustering
# ---------------------
# Perform K-Medoids clustering (k = 3) using Manhattan distance
pam_result <- pam(data_cl, k = 3, metric = "manhattan")

# Visualize clustering result
fviz_cluster(pam_result, data = data_cl)

# Add cluster labels to dataset
data_cl$cluster <- pam_result$clustering

# Matrix Dissimilarity
diss_matrix <- daisy(data_cl, metric = "manhattan")

# Cluster visualization
fviz_cluster(pam_result, data = data_cl)

# Silhouette score
silhouette_score <- silhouette(pam_result$clustering, diss_matrix)
plot(silhouette_score, col = 1:2, border = NA)

# ---------------------
# 4. Prepare Data for MDA
# ---------------------
# Simulate latent variables (e.g., X1 to X5) using means of question blocks (adjust as needed)
# For this example, we assume the data is already prepared in X1 to X5 and labeled with clusters

# Sample simulated structure (replace with actual computation)
set.seed(123)
data_cl$X1 <- rnorm(nrow(data_cl))
data_cl$X2 <- rnorm(nrow(data_cl))
data_cl$X3 <- rnorm(nrow(data_cl))
data_cl$X4 <- rnorm(nrow(data_cl))
data_cl$X5 <- rnorm(nrow(data_cl))
data_cl$cashless_user <- as.factor(data_cl$cluster)

cluster_kmedoids <- data_cl %>%
  mutate(cashless_user = case_when(
    pam_result$cluster == 1 ~ "1",    # Cluster 1
    pam_result$cluster == 2 ~ "2",    # Cluster 2
    pam_result$cluster == 3 ~ "3"      # Cluster 3
  ))

data_mda <- data.frame(
  Y = cluster_kmedoids$cashless_user,  # Variabel dependen
  X1 = X1,
  X2 = X2,
  X3 = X3,
  X4 = X4,
  X5 = X5
  )

# Split into training and testing sets
train_index <- sample(1:nrow(data_cl), 0.8 * nrow(data_cl))
training_data <- data_cl[train_index, ]
test_data <- data_cl[-train_index, ]

# Normal Multivariate Assumptions (QQ Plot Quantile Chi Square and Mahalanobis Distance)
mu <- colMeans(train[, c(2:6)])  
sigma <- cov(train[, c(2:6)])    
dist <- mahalanobis(train[, c(2:6)], mu, sigma)  
df <- ncol(train[, c(2:6)])  
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
# Mardia test
normal_test <- mvn(data = train[, c(2:6)], mvnTest = "mardia")
normal_test$multivariateNormality

# Homogenity Assumption (Variance Covariance Matrix)
homogen_test <- boxM(data = train[, c(2:6)], grouping = train[,1])
print(homogen_test)

# ---------------------
# 5. Multiple Discriminant Analysis (LDA)
# ---------------------
# Fit LDA model
lda_model <- lda(cashless_user ~ X1 + X2 + X3 + X4 + X5, data = training_data)
print(lda_model)

# Wilks' Lambda Test
man <- manova(formula = cbind(X1, X2, X3, X4, X5) ~ Y, data = train)
summary(object = man, test = 'Wilks')
summary.aov(m)

# ---------------------
# 6. Model Evaluation
# ---------------------
# Predict on testing data
lda_pred <- predict(lda_model, newdata = test_data)

# Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(lda_pred$class), as.factor(test_data$cashless_user))
print(conf_matrix)

#Show Accuracy, Sensitivity, Spesificity
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

# For overall model accuracy and other statistics:
accuracy <- cfm$overall['Accuracy']
cat("Overall Accuracy: ", accuracy, "\n")

# Data Testing Prediction with LDA
lda_test_scores <- predict(linearDA, newdata = test)
lda_test_scores <- data.frame(lda_test_scores$x, Group = test$Y)

# Add New Column (Training atau Testing) & Combine Data
lda_scores$Type <- "Training"
lda_test_scores$Type <- "Testing"
combined_scores <- rbind(lda_scores,lda_test_scores)

# Territorial Map Plot of Data Testing
lda_test_scores$Pred <- test_correct
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
