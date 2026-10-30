getwd()
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(GGally)
library(e1071)
library(tidyverse)

data <- read_excel("data.xlsx")
rownames(data) <- data$Movie
data <- data[, !colnames(data) %in% "Movie"]
data$Year <- as.factor(data$Year)
colnames(data)[colnames(data) == "Aggregate Followers"] <- "Followers"

data$SequelBased <- ifelse(data$Sequel == 1, "Not Sequel Based", "Sequel Based")
data <- data[, !names(data) %in% "Sequel"]
data <- data[, c(setdiff(names(data), "Gross"), "Gross")]

summary(data)

colSums(is.na(data))


# Sadece sayısal sütunları seçme
numerical_data <- data[, sapply(data, is.numeric)]

# Her bir kolonun çarpıklığını hesapla ve yazdır
for (col_name in colnames(numerical_data)) {
  skew_value <- skewness(numerical_data[[col_name]], na.rm = TRUE) # Çarpıklık değeri
  cat("Kolon:", col_name, "- Çarpıklık:", skew_value, "\n")
}
library(MASS)

data$Dislikes <- data$Dislikes + 1
data$Comments <- data$Comments + 1


# Genre için Box-Cox dönüşümü
genre_model <- lm(Genre ~ 1, data = data)
lambda_genre <- boxcox(genre_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(genre_model, lambda = seq(-2, 2, 0.1))$y)]
data$Genre <- (data$Genre^lambda_genre - 1) / lambda_genre

# Budget için Box-Cox dönüşümü
budget_model <- lm(Budget ~ 1, data = data)
lambda_budget <- boxcox(budget_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(budget_model, lambda = seq(-2, 2, 0.1))$y)]
data$Budget <- (data$Budget^lambda_budget - 1) / lambda_budget

# Screens için Box-Cox dönüşümü
screens_model <- lm(Screens ~ 1, data = data)
lambda_screens <- boxcox(screens_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(screens_model, lambda = seq(-2, 2, 0.1))$y)]
data$Screens <- (data$Screens^lambda_screens - 1) / lambda_screens

# Views için Box-Cox dönüşümü
views_model <- lm(Views ~ 1, data = data)
lambda_views <- boxcox(views_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(views_model, lambda = seq(-2, 2, 0.1))$y)]
data$Views <- (data$Views^lambda_views - 1) / lambda_views

# Likes için Box-Cox dönüşümü
likes_model <- lm(Likes ~ 1, data = data)
lambda_likes <- boxcox(likes_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(likes_model, lambda = seq(-2, 2, 0.1))$y)]
data$Likes <- (data$Likes^lambda_likes - 1) / lambda_likes

# Dislikes için Box-Cox dönüşümü
dislikes_model <- lm(Dislikes ~ 1, data = data)
lambda_dislikes <- boxcox(dislikes_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(dislikes_model, lambda = seq(-2, 2, 0.1))$y)]
data$Dislikes <- (data$Dislikes^lambda_dislikes - 1) / lambda_dislikes

# Comments için Box-Cox dönüşümü
comments_model <- lm(Comments ~ 1, data = data)
lambda_comments <- boxcox(comments_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(comments_model, lambda = seq(-2, 2, 0.1))$y)]
data$Comments <- (data$Comments^lambda_comments - 1) / lambda_comments

#  Followers için Box-Cox dönüşümü
followers_model <- lm(Followers ~ 1, data = data)
lambda_followers <- boxcox(followers_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(followers_model, lambda = seq(-2, 2, 0.1))$y)]
data$Followers <- (data$Followers^lambda_followers - 1) / lambda_followers

gross_model <- lm(Gross ~ 1, data = data)
lambda_gross <- boxcox(gross_model, lambda = seq(-2, 2, 0.1))$x[which.max(boxcox(gross_model, lambda = seq(-2, 2, 0.1))$y)]
data$Gross <- (data$Gross^lambda_gross - 1) / lambda_gross

# Sadece sayısal sütunları seçme
numerical_data <- data[, sapply(data, is.numeric)]

# Her bir kolonun çarpıklığını hesapla ve yazdır
for (col_name in colnames(numerical_data)) {
  skew_value <- skewness(numerical_data[[col_name]], na.rm = TRUE) # Çarpıklık değeri
  cat("Kolon:", col_name, "- Çarpıklık:", skew_value, "\n")
}

library(GGally)
library(ggplot2)

# Binwidth hesaplayan fonksiyon
calculate_binwidth <- function(column) {
  column <- column[!is.na(column)]  # NA değerleri çıkar
  if (length(column) <= 1) {
    return(1)  # Varsayılan değer
  }
  bins <- ceiling(log2(length(column)) + 1)
  binwidth <- (max(column) - min(column)) / bins
  if (binwidth == 0 || is.nan(binwidth) || is.infinite(binwidth)) {
    return(1)
  }
  return(binwidth)
}

# Binwidth değerlerini hesapla ve listeye kaydet
binwidth_list <- lapply(numerical_data, calculate_binwidth)

# Özel histogram fonksiyonu
custom_hist <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  ggplot(data, mapping) +
    geom_histogram(binwidth = calculate_binwidth(x), fill = "lightblue", color = "darkblue", ...) +
    theme_minimal()
}

# ggpairs için özel histogram fonksiyonu ve scatter plot
ggpairs(
  numerical_data,
  upper = list(continuous = wrap(pie_cor)),  # Üst üçgende özel pasta grafikli korelasyon
  lower = list(continuous = wrap("points", color = "blue", alpha = 0.6)),  # Alt üçgende scatter plot mavi
  diag = list(continuous = wrap(custom_hist))  # Diyagonal için özel histogram
)

colnames(data)


# Sadece seçilen sütunları al ve uzun forma dönüştür
categorical_data <- pivot_longer(
  data[, c("Year", "SequelBased")],  # "Year", "SequelBased", "Genre" sütunlarını seç
  cols = everything(),                        # Tüm sütunları uzun forma dönüştür
  names_to = "Variable",                      # Sütun isimlerini "Variable" kolonuna taşı
  values_to = "Category"                      # Değerleri "Category" kolonuna taşı
)

# Frekans plotlarını tek bir grafikte çizdirin
ggplot(categorical_data, aes(x = Category)) +
  geom_bar(fill = "skyblue", color = "darkblue") +
  facet_wrap(~Variable, scales = "free_x") + # Her kategorik değişken için ayrı panel
  theme_minimal() +
  labs(title = "Frequency Plots of Categorical Variables",
       x = "Categories",
       y = "Frequency") +
  scale_y_continuous(breaks = seq(0, 200, by = 10))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



data$SequelBased <- as.factor(data$SequelBased)
data$Year <- as.factor(data$Year) 
str(data)
# Gerekli kütüphaneyi yükleyin
library(mice)


# MICE işlemi
imputed_data <- mice(data, 
                     method = c("logreg", 
                                "pmm",
                                "pmm",
                                "pmm",
                                "pmm", 
                                "pmm",
                                "pmm",
                                "pmm",
                                "pmm", 
                                "pmm",
                                "pmm", 
                                "logreg",
                                "pmm"), 
                     maxit = 100, # Maksimum iterasyon sayısı
                     m = 10,      # 5 farklı doldurulmuş veri seti oluştur
                     seed = 123) # Aynı sonuçları almak için sabit tohumlama

# Doldurulmuş veri setini elde etme (1. doldurulmuş set)
completed_data <- complete(imputed_data, 1)

# Doldurulmuş veriyi kontrol etme
summary(completed_data)
colSums(is.na(completed_data))

data <- completed_data


library(FactoMineR)
library(factoextra)

# FAMD Analizi
famd_result <- FAMD(data[, !colnames(data) %in% "Gross"], ncp = 12) # ncp: analiz edilecek bileşen sayısı

# Scree Plot: Eigenvalue'lara dayalı
fviz_eig(famd_result, 
         addlabels = TRUE,        # Çubukların üzerinde değerleri göster
         ylim = c(0, 25),         # Y eksenini ayarla (isteğe bağlı)
         barfill = "skyblue",     # Çubuk rengi
         barcolor = "darkblue",   # Çubuk kenar rengi
         linecolor = "red",       # Çizgi rengi
         ncp = 12)
        
# Eigenvalue'ları ve toplam varyans açıklamasını kontrol etme
print(famd_result$eig)

# Bileşenlere göre değişkenlerin katkısı
print(famd_result$var$contrib)

# Sayısal değişkenlerin koordinatlarını kontrol etme
print(famd_result$var$coord)

# Kategorik değişkenlerin koordinatlarını kontrol etme
print(famd_result$quali.var$coord)

contrib_matrix <- famd_result$var$contrib[, 1:11] # İlk 11 bileşen
library(corrplot)

cos2_matrix <- famd_result$var$cos2[, 1:11] # İlk 11 bileşen için cos2 değerleri

# Corrplot ile görselleştirme
corrplot(cos2_matrix,
         is.corr = FALSE,           # cos2 değerleri korelasyon değil, bu nedenle FALSE
         tl.col = "black",          # Etiket rengi siyah
         tl.srt = 45,               # Etiketleri 45 derece döndür
         cl.pos = "r",              # Renk skalası sağda
         title = "Quality of Representation (cos2) for Variables",
         mar = c(0, 0, 2, 0))       # Başlık için üst boşluk


# PCA için sadece "Views", "Likes", "Dislikes", "Comments" değişkenlerini seçiyoruz
pca_result <- prcomp(data[, c("Views", "Likes", "Dislikes", "Comments")], scale. = TRUE)

# PCA özeti
summary(pca_result)
data2 <- data
# PCA'nın ilk bileşenini ekliyoruz
data2$Interaction_Score <- pca_result$x[, 1]

# Eski sütunları (Views, Likes, Dislikes, Comments) kaldırıyoruz
data2 <- data2[, !(colnames(data) %in% c("Views", "Likes", "Dislikes", "Comments"))]

# Güncellenmiş veri setini kontrol et
str(data2)


data2 <- data2[, c(setdiff(names(data2), "Gross"), "Gross")]
numerical_data2 <- data2[, sapply(data2, is.numeric)]
# ggpairs için özel histogram fonksiyonu ve scatter plot
ggpairs(
  numerical_data2,
  upper = list(continuous = wrap(pie_cor)),  # Üst üçgende özel pasta grafikli korelasyon
  lower = list(continuous = wrap("points", color = "blue", alpha = 0.6)),  # Alt üçgende scatter plot mavi
  diag = list(continuous = wrap(custom_hist))  # Diyagonal için özel histogram
)


model1 <- lm(Gross ~ ., data = data)
summary(model1)

library(olsrr)
library(ISLR)

model2 <- lm(Gross ~ ., data = data2)
summary(model2)


a1=ols_step_all_possible(model1)
View(a1$result)
summary(lm(Gross ~ ., data = data[,c("Year", "Ratings", "Budget", "Screens","Sentiment", "Views", "Likes", "Dislikes", "SequelBased", "Gross")]))

summary(lm(Gross ~ ., data = data[,c("Year", "Ratings", "Budget", "Screens", "Views", "Likes", "Dislikes", "SequelBased", "Gross")]))

?ols_step_all_possible
a2=ols_step_all_possible(model2)
View(a2$result)
final <- lm(Gross ~ ., data = data2[,c("Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")])
summary(final)
summary(lm(Gross ~ ., data = data2[,c("Year","Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")]))
summary(lm(Gross ~ ., data = data2[,c("Year","Ratings" ,"Budget", "Screens","Sentiment" ,"SequelBased", "Interaction_Score", "Gross")]))


length (which(hatvalues(final)>2*mean(hatvalues(final))))
summary(hatvalues(final))

par(mfrow=c(1,1))

# Standart artıkları (Standardized Residuals) hesaplama
st.res <- final$residuals / sd(final$residuals)

# Leverage değerlerini hesaplama
hat_values <- hatvalues(final)

# Ortalama leverage değeri
mean_hat <- mean(hat_values)

# Alanın dışında kalan gözlemleri bulma
outliers <- which(hat_values > 2 * mean_hat & (st.res < -3 | st.res > 3))
potential_outliers  <- which(hat_values > 2 * mean_hat | (st.res < -3 | st.res > 3))
# Grafik çizimi (sadece görsel, ekleme yapılmaz)
plot(hat_values, st.res)
abline(h = c(-3, 3), v = 2 * mean_hat)

# Alan dışındaki indeksleri yazdırma
print(outliers)
print(potential_outliers)
data2[potential_outliers,c("Year","Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")]
summary(data2[,c("Year","Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")])

par(mfrow=c(1,1))
plot(data2$Gross,cooks.distance(final))
abline(h=4/(length(data2$Gross)-2))
which(cooks.distance(final) > 4 / (length(data2$Gross ) - 2))


data2[c(80, 106, 160, 231),c("Year","Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")]
summary(data2[,c("Year","Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")])

final <- lm(Gross ~ ., data = data2[-c(80,106, 231), c("Ratings" ,"Budget", "Screens", "SequelBased", "Interaction_Score", "Gross")])
summary(final)

par(mfrow=c(2,2))
plot(final)

residuals <- resid(final)
par(mfrow=c(1,1))
hist(residuals)
shapiro.test(residuals)
par(mfrow=c(1,1))

ks.test(residuals, "pnorm", mean=mean(residuals), sd=sd(residuals))




library(nortest)   # Anderson-Darling testi için
ad.test(residuals)



ols_press(final)


library(Hmisc)
library(car)
library(lmtest)

bptest(final)
vif(final)
