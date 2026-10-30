data <- read.csv("2019.csv")
summary(data)
data <- data[,-1]
row.names(data) <- data[,1]
data <- data[,-1]
nrow(data)


library(moments)

# Çarpıklık hesaplama
calculate_skewness <- function(df) {
  # Sadece numerik kolonları seç
  numeric_cols <- df[, sapply(df, is.numeric)]
  
  # Çarpıklık hesapla
  skewness_values <- sapply(numeric_cols, skewness)
  
  return(skewness_values)
}

# Çarpıklık değerlerini hesapla
skewness_results <- calculate_skewness(data)

# Sonuçları yazdır
print(skewness_results)




library(car)
# PowerTransform ile optimal lambda değerlerini bul
boxcox_transformed <- powerTransform(data[, c("Social.support", 
                                              "Perceptions.of.corruption", 
                                              "Healthy.life.expectancy", 
                                              "Freedom.to.make.life.choices", 
                                              "Generosity")] + 1)

# Lambda değerlerini kontrol et
boxcox_transformed

# Belirlenen lambda değerleriyle dönüşümü uygula
data_transformed <- data
data_transformed[, c("Social.support", 
                     "Perceptions.of.corruption", 
                     "Healthy.life.expectancy", 
                     "Freedom.to.make.life.choices", 
                     "Generosity")] <- bcPower(
                       data[, c("Social.support", 
                                "Perceptions.of.corruption", 
                                "Healthy.life.expectancy", 
                                "Freedom.to.make.life.choices", 
                                "Generosity")] + 1,  # +1 ofset eklenmiş veriyi kullan
                       coef(boxcox_transformed)  # Optimal lambda değerlerini kullan
                     )

calculate_skewness(data)
calculate_skewness(data_transformed)



library(GGally)
library(ggplot2)


calculate_binwidth <- function(x) {
  if (length(x) < 2) {
    return(1)  # Çok az veri varsa varsayılan bin genişliği
  }
  range_x <- range(x, na.rm = TRUE)
  bins <- 1 + log2(length(x))  # Sturges formülü
  binwidth <- (range_x[2] - range_x[1]) / bins
  return(binwidth)
}




# Özel histogram fonksiyonu
custom_hist <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  binwidth <- calculate_binwidth(x)  # Binwidth'i hesapla
  ggplot(data, mapping) +
    geom_histogram(binwidth = binwidth, fill = "lightblue", color = "darkblue", ...) +
    theme_minimal()
}

# Pie korelasyon fonksiyonu
pie_cor <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  cor_value <- round(cor(x, y, use = "complete.obs"), 2)
  
  pie_data <- data.frame(
    category = c("Correlation", "Remaining"),
    value = c(abs(cor_value), 1 - abs(cor_value))
  )
  
  color_palette <- colorRampPalette(c("red", "purple", "blue"))(100)
  cor_color <- color_palette[round((cor_value + 1) * 50)]
  
  cor_fill_color <- ifelse(cor_value >= 0, "skyblue", "red")
  
  ggplot(pie_data, aes(x = "", y = value, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y") +
    geom_text(aes(label = ifelse(category == "Correlation", cor_value, "")), 
              x = 1, y = 0.5, size = 5, color = cor_color, fontface = "bold") +
    scale_fill_manual(values = c("Correlation" = cor_fill_color, "Remaining" = "grey80")) +
    theme_void() +
    theme(legend.position = "none")
}

# ggpairs ile korelasyon matrisi oluşturma
ggpairs(
  data_transformed,  # Dataframe olarak sadece data[,-1]
  upper = list(continuous = wrap(pie_cor)),  # Üst üçgende özel pasta grafikli korelasyon
  lower = list(continuous = wrap("points", color = "blue", alpha = 0.6)),  # Alt üçgende scatter plot mavi
  diag = list(continuous = wrap(custom_hist))  # Diyagonal için özel histogram
)


# Set random seed for reproducibility
set.seed(325)

# Veri satır sayısını al
n <- nrow(data)

# %70'lik train indekslerini rastgele seç
train_indices <- sample(seq_len(n), size = 0.7 * n)

# Train ve test veri setlerini oluştur
train_data <- data[train_indices, ]
test_data <- data[-train_indices,]


# PowerTransform ile optimal lambda değerlerini bul (sadece train verisi üzerinde)
boxcox_transformed <- powerTransform(train_data[, c("Social.support", 
                                                    "Perceptions.of.corruption", 
                                                    "Healthy.life.expectancy", 
                                                    "Freedom.to.make.life.choices", 
                                                    "Generosity")] + 1)

# Lambda değerlerini kontrol et
boxcox_transformed

# Belirlenen lambda değerleriyle dönüşümü uygula
train_data_transformed <- train_data
test_data_transformed <- test_data

# Train verisine dönüşüm uygula
train_data_transformed[, c("Social.support", 
                           "Perceptions.of.corruption", 
                           "Healthy.life.expectancy", 
                           "Freedom.to.make.life.choices", 
                           "Generosity")] <- bcPower(
                             train_data[, c("Social.support", 
                                            "Perceptions.of.corruption", 
                                            "Healthy.life.expectancy", 
                                            "Freedom.to.make.life.choices", 
                                            "Generosity")] + 1,  # +1 ofset eklenmiş veriyi kullan
                             coef(boxcox_transformed)  # Optimal lambda değerlerini kullan
                           )

# Test verisine dönüşüm uygula (Train'den bulunan lambda değerleriyle)
test_data_transformed[, c("Social.support", 
                          "Perceptions.of.corruption", 
                          "Healthy.life.expectancy", 
                          "Freedom.to.make.life.choices", 
                          "Generosity")] <- bcPower(
                            test_data[, c("Social.support", 
                                          "Perceptions.of.corruption", 
                                          "Healthy.life.expectancy", 
                                          "Freedom.to.make.life.choices", 
                                          "Generosity")] + 1,  # +1 ofset eklenmiş veriyi kullan
                            coef(boxcox_transformed)  # Optimal lambda değerlerini kullan
                          )


# Orijinal Train ve Test çarpıklık değerleri
cat("Train verisi (orijinal) çarpıklık:\n")
print(calculate_skewness(train_data))

# Dönüşüm sonrası Train ve Test çarpıklık değerleri
cat("\nTrain verisi (dönüşüm sonrası) çarpıklık:\n")
print(calculate_skewness(train_data_transformed))

cat("\nTest verisi (orijinal) çarpıklık:\n")
print(calculate_skewness(test_data))

cat("\nTest verisi (dönüşüm sonrası) çarpıklık:\n")
print(calculate_skewness(test_data_transformed))


library(MLmetrics)

calc_reg_metrics <- function(
    train_preds,  # Eğitim seti tahminleri
    train_actual, # Eğitim seti gerçek değerleri
    test_preds,   # Test seti tahminleri
    test_actual,  # Test seti gerçek değerleri
    prefix = "model" # Sonuç sütunları için önek
) {
  # Train metrikleri
  train_mae  <- MAE(train_preds, train_actual)
  train_mape <- MAPE(train_preds, train_actual)
  train_mse  <- MSE(train_preds, train_actual)
  train_rmse <- RMSE(train_preds, train_actual)
  train_r2   <- R2_Score(train_preds, train_actual)
  
  # Test metrikleri
  test_mae   <- MAE(test_preds, test_actual)
  test_mape  <- MAPE(test_preds, test_actual)
  test_mse   <- MSE(test_preds, test_actual)
  test_rmse  <- RMSE(test_preds, test_actual)
  test_r2    <- R2_Score(test_preds, test_actual)
  
  # Tek tablo halinde döndür
  results_df <- data.frame(
    Metric = c(
      paste0(prefix, "_MAE"), 
      paste0(prefix, "_MAPE"), 
      paste0(prefix, "_MSE"), 
      paste0(prefix, "_RMSE"), 
      paste0(prefix, "_R2")
    ),
    Train  = c(train_mae, train_mape, train_mse, train_rmse, train_r2),
    Test   = c(test_mae,  test_mape,  test_mse,  test_rmse,  test_r2)
  )
  
  return(results_df)
}


library(olsrr)
str(train_data)
model1 <- lm(Score ~ ., data = train_data_transformed)
a=ols_step_all_possible(model1)
a$result
View(a$result)
plot(a)
a$result$predictors[57]

model2 <- lm(Score ~ ., data = train_data_transformed[c("Score","GDP.per.capita", 
                                                        "Social.support", 
                                                        "Healthy.life.expectancy", 
                                                        "Freedom.to.make.life.choices", 
                                                        "Perceptions.of.corruption")])
summary(model2)
shapiro.test(residuals(model2))

library(Hmisc)
library(car)
library(lmtest)

bptest(model2)
vif(model2)

lm_predictions_train <- predict(model2, newdata = train_data_transformed)
lm_predictions_test  <- predict(model2, newdata = test_data_transformed)

lm_results_df <- calc_reg_metrics(
  train_preds = lm_predictions_train,
  train_actual = train_data_transformed$Score,
  test_preds = lm_predictions_test,
  test_actual = test_data_transformed$Score,
  prefix = "lm"
)

print(lm_results_df)


library(tree)
set.seed(325)
tree_hitters_full <-tree(Score~., data=train_data_transformed)
summary(tree_hitters_full)
plot(tree_hitters_full)
text(tree_hitters_full)

cv_hitters_full <- cv.tree(tree_hitters_full)
cv_hitters_full
plot(cv_hitters_full$size,cv_hitters_full$dev,type='b')
prune_hitters_full=prune.tree(tree_hitters_full,best=6)
summary(prune_hitters_full)
plot(prune_hitters_full)
text(prune_hitters_full,pretty=0)


# --- 1) BUDAMA ÖNCESİ (UNPRUNED) AĞAÇ ---
unpruned_train_preds <- predict(tree_hitters_full, newdata = train_data_transformed)
unpruned_test_preds <- predict(tree_hitters_full, newdata = test_data_transformed)


unpruned_tree_results_df <- calc_reg_metrics(
  train_preds = unpruned_train_preds,
  train_actual = train_data_transformed$Score,
  test_preds = unpruned_test_preds,
  test_actual = test_data_transformed$Score,
  prefix = "unpruned_tree"
)
print(unpruned_tree_results_df)

pruned_train_preds <- predict(prune_hitters_full, newdata = train_data_transformed)
pruned_test_preds  <- predict(prune_hitters_full,  newdata = test_data_transformed)

pruned_tree_results_df <- calc_reg_metrics(
  train_preds = pruned_train_preds,
  train_actual = train_data_transformed$Score,
  test_preds = pruned_test_preds,
  test_actual = test_data_transformed$Score,
  prefix = "pruned_tree"
)
print(pruned_tree_results_df)


set.seed(325)
library(randomForest)
bag<-randomForest(Score~.,data = train_data_transformed,mtry=6,importance=TRUE)

bag_train_preds <- predict(bag, newdata = train_data_transformed)
bag_test_preds <- predict(bag, newdata = test_data_transformed)

bag_results_df <- calc_reg_metrics(
  train_preds = bag_train_preds,
  train_actual = train_data_transformed$Score,
  test_preds = bag_test_preds,
  test_actual = test_data_transformed$Score,
  prefix = "bagging"
)

print(bag_results_df)

set.seed(325)
rf<-randomForest(Score~.,data = train_data_transformed,mtry=2,importance=TRUE)

rf_train_preds <- predict(rf, newdata = train_data_transformed)
rf_test_preds <- predict(rf, newdata = test_data_transformed)

rf_results_df <- calc_reg_metrics(
  train_preds = rf_train_preds,
  train_actual = train_data_transformed$Score,
  test_preds = rf_test_preds,
  test_actual = test_data_transformed$Score,
  prefix = "rf"
)


print(rf_results_df)

list(
  linear_regression            = lm_results_df,
  unpruned_tree = unpruned_tree_results_df,
  pruned_tree   = pruned_tree_results_df,
  random_forest            = rf_results_df
)


threshold <- median(c(train_data_transformed$Score,test_data_transformed$Score))
train_data_transformed$Score <- ifelse(threshold < train_data_transformed$Score,1,0)
test_data_transformed$Score <- ifelse(threshold < test_data_transformed$Score,1,0)
table(c(train_data_transformed$Score,test_data_transformed$Score))
train_data_transformed$Score <- as.factor(train_data_transformed$Score)
test_data_transformed$Score <- as.factor(test_data_transformed$Score)


calc_classification_metrics <- function(
    train_pred,    # Eğitim seti tahminleri (factor veya 0/1)
    train_actual,  # Eğitim seti gerçek değerleri (factor veya 0/1)
    test_pred,     # Test seti tahminleri
    test_actual,   # Test seti gerçek değerleri
    positive = "1",# Pozitif sınıfın etiketi
    prefix   = "model" # Önek (prefix) örn: "unpruned", "pruned", "rf" vb.
) {
  # EĞİTİM METRİKLERİ
  train_f1   <- F1_Score(train_pred, train_actual, positive = positive)
  train_acc  <- Accuracy(train_pred, train_actual)
  train_prec <- Precision(train_pred, train_actual, positive = positive)
  train_rec  <- Recall(train_pred, train_actual, positive = positive)
  train_sens <- Sensitivity(train_pred, train_actual, positive = positive)
  train_spec <- Specificity(train_pred, train_actual, positive = positive)
  
  # TEST METRİKLERİ
  test_f1    <- F1_Score(test_pred, test_actual, positive = positive)
  test_acc   <- Accuracy(test_pred, test_actual)
  test_prec  <- Precision(test_pred, test_actual, positive = positive)
  test_rec   <- Recall(test_pred, test_actual, positive = positive)
  test_sens  <- Sensitivity(test_pred, test_actual, positive = positive)
  test_spec  <- Specificity(test_pred, test_actual, positive = positive)
  
  # TEK BİR TABLO HALİNDE BİRLEŞTİR
  results_df <- data.frame(
    Metric = c(
      paste0(prefix, "_F1"), 
      paste0(prefix, "_Accuracy"), 
      paste0(prefix, "_Precision"), 
      paste0(prefix, "_Recall"), 
      paste0(prefix, "_Sensitivity"), 
      paste0(prefix, "_Specificity")
    ),
    Train = c(
      train_f1, train_acc, train_prec, train_rec, train_sens, train_spec
    ),
    Test  = c(
      test_f1,  test_acc,  test_prec,  test_rec,  test_sens,  test_spec
    )
  )
  
  return(results_df)
}
# Sınıflandırma Ağacı

par(mfrow=c(1,1))
set.seed(325)
tree_hitters_full <-tree(Score~., data=train_data_transformed)
summary(tree_hitters_full)
plot(tree_hitters_full)
text(tree_hitters_full)


cv_hitters_full <- cv.tree(tree_hitters_full)
cv_hitters_full
plot(cv_hitters_full$size,cv_hitters_full$dev,type='b')
prune_hitters_full=prune.tree(tree_hitters_full,best=5)
summary(prune_hitters_full)
plot(prune_hitters_full)
text(prune_hitters_full,pretty=0)

unpruned_train_pred <- predict(tree_hitters_full, 
                               newdata = train_data_transformed, 
                               type = "class")
unpruned_test_pred  <- predict(tree_hitters_full, 
                               newdata = test_data_transformed, 
                               type = "class")

unpruned_results_df <- calc_classification_metrics(
  train_pred   = unpruned_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = unpruned_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "unpruned_tree"  # İstediğiniz ön-ek
)

print(unpruned_results_df)


pruned_train_pred <- predict(prune_hitters_full, newdata = train_data_transformed, type = "class")
pruned_test_pred <- predict(prune_hitters_full, newdata = test_data_transformed, type = "class")

pruned_results_df <- calc_classification_metrics(
  train_pred   = pruned_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = pruned_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "pruned_tree"  # İstediğiniz ön-ek
)

print(pruned_results_df)

set.seed(325)
bag<-randomForest(Score~.,data = train_data_transformed,mtry=6,importance=TRUE)


bag_train_pred <- predict(bag, newdata = train_data_transformed, type = "class")
bag_test_pred <- predict(bag, newdata = test_data_transformed, type = "class")

bag_results_df <- calc_classification_metrics(
  train_pred   = bag_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = bag_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "bag" 
)
print(bag_results_df)

set.seed(325)
rf<-randomForest(Score~.,data = train_data_transformed,mtry=2,importance=TRUE)
rf_train_pred <- predict(rf, newdata = train_data_transformed, type = "class")
rf_test_pred <- predict(rf, newdata = test_data_transformed, type = "class")

rf_results_df <- calc_classification_metrics(
  train_pred   = rf_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = rf_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "rf"
)
print(rf_results_df)


library(MASS)

model1_logistic <- glm(Score ~ ., 
                       data = train_data_transformed, 
                       family = binomial)
summary(model1_logistic)
vif(model1_logistic)
model2_logistic=stepAIC(model1_logistic, direction = "both")
summary(model2_logistic)
vif(model2_logistic)


glm_train_prob <- predict(model2_logistic, newdata = train_data_transformed, type = "response")
glm_train_pred <- ifelse(glm_train_prob > 0.5, 1, 0)
glm_train_pred <- factor(glm_train_pred, levels = c(0, 1))

glm_test_prob <- predict(model2_logistic, newdata = test_data_transformed,  type = "response")
glm_test_pred <- ifelse(glm_test_prob > 0.5, 1, 0)
glm_test_pred <- factor(glm_test_pred, levels = c(0, 1))

glm_results_df <- calc_classification_metrics(
  train_pred   = glm_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = glm_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "glm"
)
print(glm_results_df)

#LDA
set.seed(325)

model_lda<-lda(Score ~ ., 
    data = train_data_transformed[,c("Social.support", "Healthy.life.expectancy",
                                     "Freedom.to.make.life.choices",
                                     "Perceptions.of.corruption","Score")])
model_lda

lda_train_pred <- predict(model_lda, train_data_transformed[,c("Social.support", "Healthy.life.expectancy",
                                                               "Freedom.to.make.life.choices",
                                                               "Perceptions.of.corruption","Score")])$class
plot(model_lda)
library(klaR)
partimat(Score~., data=train_data_transformed[,c("Social.support", "Healthy.life.expectancy",
                                                 "Freedom.to.make.life.choices","Perceptions.of.corruption","Score")],
         method="lda")
lda_test_pred <- predict(model_lda, test_data_transformed[,c("Social.support", "Healthy.life.expectancy",
                                                             "Freedom.to.make.life.choices","Perceptions.of.corruption","Score")])$class

lda_results_df <- calc_classification_metrics(
  train_pred   = lda_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = lda_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "lda"
)
print(lda_results_df)

#QDA
model_qda<-qda(Score ~ ., 
               data = train_data_transformed)

qda_train_pred <- predict(model_qda, train_data_transformed)$class

partimat(Score~., data=train_data_transformed,method="qda")
qda_test_pred <- predict(model_qda, test_data_transformed)$class

qda_results_df <- calc_classification_metrics(
  train_pred   = qda_train_pred,
  train_actual = train_data_transformed$Score,
  test_pred    = qda_test_pred,
  test_actual  = test_data_transformed$Score,
  positive     = "1",
  prefix       = "qda"
)
print(qda_results_df)


list(unpruned_results_df,pruned_results_df,bag_results_df,rf_results_df,glm_results_df,lda_results_df,qda_results_df)
