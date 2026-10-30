getwd()
data <- read.csv("formula1.csv", row.names = NULL)
data <- data[,-c(1)]

rownames(data) <- data$name
data$name <- NULL
nrow(data)
summary(data)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(GGally)


# Sadece sayısal sütunları seçme
numerical_data <- data %>% select(where(is.numeric))

# Korelasyon katsayısını gösteren ve pasta grafiği oluşturan özel fonksiyon
pie_cor <- function(data, mapping, ...) {
  # İlgili iki değişkenin adını al
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # Korelasyon hesapla
  cor_value <- round(cor(x, y, use = "complete.obs"), 2)
  
  # Pasta grafiği için veri hazırla (pozitif ve negatif korelasyon için iki parça)
  pie_data <- data.frame(
    category = c("Correlation", "Remaining"),
    value = c(abs(cor_value), 1 - abs(cor_value))
  )
  
  # Özel renk paleti: -1 kırmızı, 0 mor, +1 mavi
  color_palette <- colorRampPalette(c("red", "purple", "blue"))(100)
  cor_color <- color_palette[round((cor_value + 1) * 50)]
  
  # Korelasyon pozitif mi negatif mi?
  cor_fill_color <- ifelse(cor_value >= 0, "skyblue", "red")
  
  # Pasta grafiği oluştur
  ggplot(pie_data, aes(x = "", y = value, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y") +
    # Korelasyon katsayısını siyah çerçeveli renkli metinle göster
    geom_text(aes(label = ifelse(category == "Correlation", cor_value, "")), 
              x = 1, y = 0.5, size = 5, color = cor_color, fontface = "bold") +
    scale_fill_manual(values = c("Correlation" = cor_fill_color, "Remaining" = "grey80")) +
    theme_void() +
    theme(legend.position = "none")
}

# ggpairs ile korelasyon matrisi oluşturma
ggpairs(numerical_data,
        upper = list(continuous = wrap(pie_cor)),  # Üst üçgende özel pasta grafikli korelasyon
        lower = list(continuous = wrap("points", color = "blue", alpha = 0.6)),  # Alt üçgende scatter plot mavi
        diag = list(continuous = wrap("barDiag", fill = "lightblue", color = "darkblue")))  # Diyagonal histogramlar


ggplot(data %>%
         select(races, gp_wins, podiums, poles) %>%
         pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value"),
       aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(color = "darkblue", outlier.color = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(title = "Boxplots of Selected Numeric Variables",
       x = "Variables",
       y = "Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(data %>%
                                           select(races, gp_wins, podiums, poles) %>%
                                           pivot_longer(cols = everything(), values_to = "Value") %>%
                                           pull(Value), 
                                         na.rm = TRUE), 
                                  by = 20))

ggplot(data %>%
         select(avg_yearly_salary, championships, dnfs, years_in_f1) %>%
         pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value"),
       aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(color = "darkblue", outlier.color = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(title = "Boxplots of Selected Numeric Variables",
       x = "Variables",
       y = "Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(data %>%
                                           select(races, gp_wins, podiums, poles) %>%
                                           pivot_longer(cols = everything(), values_to = "Value") %>%
                                           pull(Value), 
                                         na.rm = TRUE), 
                                  by = 5))

# Sadece kategorik değişkenleri seçip uzun formata dönüştürün
categorical_data <- data %>%
  select(country, team, fav_circuit) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category")

# Frekans plotlarını tek bir grafikte çizdirin
ggplot(categorical_data, aes(x = Category)) +
  geom_bar(fill = "skyblue", color = "darkblue") +
  facet_wrap(~Variable, scales = "free_x") + # Her kategorik değişken için ayrı panel
  theme_minimal() +
  labs(title = "Frequency Plots of Categorical Variables",
       x = "Categories",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # X ekseni etiketlerini eğik yap
  

# Kategorik değişkenlerin isimlerini bir vektör olarak tanımlayın
categorical_vars <- c("country", "fav_circuit", "team")

# ANOVA testlerini uygulayın ve özetleri yazdırın
anova_results <- lapply(categorical_vars, function(var) {
  model <- aov(as.formula(paste("avg_yearly_salary ~", var)), data = data)
  summary(model)
})

# Sonuçları çıktı olarak göster
names(anova_results) <- categorical_vars
anova_results




# Country için ggplot boxplot
ggplot(data, aes(x = country, y = avg_yearly_salary)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Country ile Avg Yearly Salary İlişkisi",
       x = "Country", y = "Avg Yearly Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Fav Circuit için ggplot boxplot
ggplot(data, aes(x = fav_circuit, y = avg_yearly_salary)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Fav Circuit ile Avg Yearly Salary İlişkisi",
       x = "Fav Circuit", y = "Avg Yearly Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Team için ggplot boxplot
ggplot(data, aes(x = team, y = avg_yearly_salary)) +
  geom_boxplot(fill = "salmon", color = "darkred") +
  labs(title = "Team ile Avg Yearly Salary İlişkisi",
       x = "Team", y = "Avg Yearly Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(DescTools)

# Eta kare analizini ve sonuçların çıktısını alan bir fonksiyon tanımlayalım
calculate_eta_square <- function(dependent_variable, independent_variable) {
  eta_square <- EtaSq(aov(as.formula(paste(dependent_variable, "~", independent_variable)), data = data))
  print(paste("Eta Kare -", independent_variable))
  print(eta_square)
}

# Her kategorik değişken için eta kare hesaplayıp yazdırma
calculate_eta_square("avg_yearly_salary", "country")
calculate_eta_square("avg_yearly_salary", "fav_circuit")
calculate_eta_square("avg_yearly_salary", "team")

data %>%
  group_by(team) %>%
  summarise(
    count = n(),
    mean_salary = mean(avg_yearly_salary, na.rm = TRUE),
    sd_salary = sd(avg_yearly_salary, na.rm = TRUE)
  )


# Lineer regresyon modeli oluşturma
data$country <- relevel(factor(data$country), ref = "Great Britain")
data$fav_circuit <- relevel(factor(data$fav_circuit), ref = "Circuit of The Americas")
data$team <- relevel(factor(data$team), ref = "Ferrari")

model1 <- lm(avg_yearly_salary ~ ., data = data)

# Modelin özetini görüntüleme
summary(model1)


data2 <- data%>%
        select(-c("years_in_f1","fav_circuit","country"))
model2 <- lm(avg_yearly_salary ~ ., data = data2)
summary(model2)

data3 <- data%>%
  select(-c("years_in_f1","fav_circuit","country","dnfs"))
model3 <- lm(avg_yearly_salary ~ ., data = data3)
summary(model3)

model3_no_intercept <- lm(avg_yearly_salary ~ . - 1, data = data3)
summary(model3_no_intercept)


library(olsrr)

library("ISLR")

a=ols_step_all_possible(model1)
a$result
View(a$result)
plot(a)
a$result$predictors[638]

?ols_step_both_p
s=ols_step_both_p(model1)

s$metrics
s$model
s$others
s=ols_step_both_p(model1, pent = 0.05, prem = 0.05)
plot(s)

b=ols_step_backward_p(model1)
b$metrics
b$model
plot(b)

f=ols_step_forward_p(model1, p_val = 0.001)
f$metrics
f$model
f$others
plot(f)


hatvalues(model3)
hatvalues(model3)>2*mean(hatvalues(model3))
which(hatvalues(model3)>2*mean(hatvalues(model3)))


par(mfrow=c(2,2))
plot(model3)

par(mfrow=c(1,1))
st.res=model3$residuals/sd(model3$residuals)
plot(hatvalues(model3),st.res)
abline(h=c(-2,2),v=2*mean(hatvalues(model3)))
identify(hatvalues(model3),st.res)

par(mfrow=c(1,1))
plot(data3$avg_yearly_salary,cooks.distance(model3))
abline(h=4/(length(data3$avg_yearly_salary)-2))
identify(data3$avg_yearly_salary,cooks.distance(model3))

data[148,]
data[585,]
data[676,]
summary(data)

par(mfrow=c(1,1))
plot(data3$avg_yearly_salary,cooks.distance(model3))
abline(h=4/(length(data3$avg_yearly_salary)-2))
identify(data3$avg_yearly_salary,cooks.distance(model3))

data[410,]
data[166,]
data[79,]
data[21,]


data3_cleaned=data3[-c(585, 676),]

summary(model3)
model3_cleaned <- lm(avg_yearly_salary ~ ., data = data3_cleaned)
summary(model3_cleaned)
confint(model3_cleaned)
qt(.975,733)
qf(0.95,14,733)

ols_press(model1)
ols_press(model2)
ols_press(model3)
ols_press(model3_cleaned)
summary(model3)



# model3'ün artıklarını çıkarın
residuals_model3_cleaned <- resid(model3_cleaned)

# Shapiro-Wilk Testi
shapiro_result <- shapiro.test(residuals_model3_cleaned)
print("Shapiro-Wilk Testi Sonuçları:")
print(shapiro_result)

# Normal dağılımın varsayıldığı parametreler (ortalama ve standart sapma)
ks_result <- ks.test(residuals_model3_cleaned, "pnorm", mean=mean(residuals_model3_cleaned), sd=sd(residuals_model3_cleaned))
print("Kolmogorov-Smirnov Testi Sonuçları:")
print(ks_result)

# Anderson-Darling Testi
library(nortest)   # Anderson-Darling testi için

ad_result <- ad.test(residuals_model3_cleaned)
print("Anderson-Darling Testi Sonuçları:")
print(ad_result)

# Q-Q Grafiği
ggplot(data.frame(residuals = residuals_model3_cleaned), aes(sample = residuals)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Grafiği: Model3 Artıkları") +
  xlab("Teorik Quantiles") +
  ylab("Gözlenen Quantiles") +
  theme_minimal()

# Histogram (Varsayılan Bin Sayısı ile)
ggplot(data.frame(residuals = residuals_model3_cleaned), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Histogram: Model3 Artıkları") +
  xlab("Artık Değerleri") +
  ylab("Yoğunluk") +
  theme_minimal()


library(lmtest)
bptest(model3_cleaned)

library("Hmisc")
vif(model1)
vif(model3_cleaned)

summary(data3_cleaned)

# Tüm takımların bir listesini alalım
teams <- unique(data3_cleaned$team)

# Her takım için ortalamaları hesaplayan veri çerçevesi oluşturma (tam sayıya yuvarlama)
new_data <- data.frame(
  team = teams,
  races = round(mean(data3_cleaned$races, na.rm = TRUE)),
  championships = round(mean(data3_cleaned$championships, na.rm = TRUE)),
  gp_wins = round(mean(data3_cleaned$gp_wins, na.rm = TRUE)),
  podiums = round(mean(data3_cleaned$podiums, na.rm = TRUE)),
  poles = round(mean(data3_cleaned$poles, na.rm = TRUE))
)

# Confidence interval tahminleri
confidence_predictions <- predict(
  model3_cleaned, 
  newdata = new_data, 
  interval = "confidence", 
  level = 0.95
)

# Prediction interval tahminleri
prediction_predictions <- predict(
  model3_cleaned, 
  newdata = new_data, 
  interval = "prediction", 
  level = 0.95
)

# Kolonları düzenleme: Confidence ve Prediction Interval ayrı sütunlar olarak
new_data <- cbind(
  new_data,
  conf_fit = confidence_predictions[, "fit"],
  conf_lwr = confidence_predictions[, "lwr"],
  conf_upr = confidence_predictions[, "upr"],
  pred_fit = prediction_predictions[, "fit"],
  pred_lwr = prediction_predictions[, "lwr"],
  pred_upr = prediction_predictions[, "upr"]
)

# Confidence ve Prediction Interval farklarını hesaplama
new_data <- new_data %>%
  mutate(
    conf_diff = conf_upr - conf_lwr, # Confidence interval farkı
    pred_diff = pred_upr - pred_lwr  # Prediction interval farkı
  )

# Sonuçları görmek için
print(new_data)


