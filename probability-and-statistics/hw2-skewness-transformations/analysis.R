data <- read.csv("data2.csv")
head(data)
summary(data)
nrow(data)
library(e1071)
numerical_data <- data[, sapply(data, is.numeric)]
for (col_name in colnames(numerical_data)) {
  skew_value <- skewness(numerical_data[[col_name]], na.rm = TRUE) # Çarpıklık değeri
  cat("Kolon:", col_name, "- Çarpıklık:", skew_value, "\n")
}

library(GGally)
library(ggplot2)

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

# Özel histogram fonksiyonu
custom_hist <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  ggplot(data, mapping) +
    geom_histogram(binwidth = calculate_binwidth(x), fill = "lightblue", color = "darkblue", ...) +
    theme_minimal()
}

# ggpairs ile korelasyon matrisi oluşturma
ggpairs(
  numerical_data,
  upper = list(continuous = wrap(pie_cor)),  # Üst üçgende özel pasta grafikli korelasyon
  lower = list(continuous = wrap("points", color = "blue", alpha = 0.6)),  # Alt üçgende scatter plot mavi
  diag = list(continuous = wrap(custom_hist))  # Diyagonal için özel histogram
)

skewness(sqrt(data$total_population))
skewness((data$total_population)^(1/3))
skewness(log(data$total_population))
skewness(1/data$total_population)

skewness(data$net_migration)
skewness(data$net_migration^2)
skewness(data$net_migration^3)

library(caret)

# Öncelikle dönüşüm modelini oluştur
preprocessor <- preProcess(data.frame(net_migration = data$net_migration), method = "YeoJohnson")

# Dönüştürülmüş veriyi hesapla
transformed_data <- predict(preprocessor, data.frame(net_migration = data$net_migration))

# Çarpıklık değerini hesapla
skewness(transformed_data$net_migration)


Year <- data$Year
total_population_log <- log(data$total_population)
preprocessor <- preProcess(data.frame(net_migration = data$net_migration), method = "YeoJohnson")
net_migration_yj <- predict(preprocessor, data.frame(net_migration = data$net_migration))$net_migration

data2 <- data.frame(
  Year = Year,
  total_population = total_population_log,
  net_migration = net_migration_yj
)
summary(data2)
ggpairs(
  data2,
  upper = list(continuous = wrap(pie_cor)),  # Üst üçgende özel pasta grafikli korelasyon
  lower = list(continuous = wrap("points", color = "blue", alpha = 0.6)),  # Alt üçgende scatter plot mavi
  diag = list(continuous = wrap(custom_hist))  # Diyagonal için özel histogram
)


# 1. Plot: Year Karşılaştırması
par(mfrow = c(2, 1))  # Bir pencerede iki grafik, üst üste
boxplot(data$Year, main = "Year Comparison: Original", horizontal = TRUE, col = "lightblue", ylim = range(data$Year))
boxplot(data2$Year, main = "Year Comparison: Transformed", horizontal = TRUE, col = "lightgreen", ylim = range(data2$Year))

# 2. Plot: Total Population Karşılaştırması
boxplot(data$total_population, main = "Total Population: Original", horizontal = TRUE, col = "lightblue", ylim = range(data$total_population))
boxplot(data2$total_population, main = "Total Population: Log Transformed", horizontal = TRUE, col = "lightgreen", ylim = range(data2$total_population))

# 3. Plot: Net Migration Karşılaştırması
boxplot(data$net_migration, main = "Net Migration: Original", horizontal = TRUE, col = "lightblue", ylim = range(data$net_migration))
boxplot(data2$net_migration, main = "Net Migration: Yeo-Johnson Transformed", horizontal = TRUE, col = "lightgreen", ylim = range(data2$net_migration))

set.seed(42)
data_30 <- data2[sample(nrow(data2), 30), ]
data_60 <- data2[sample(nrow(data2), 60), ]

library(nortest)  # Lilliefors, Cramer-von Mises, Anderson-Darling
library(goftest)  # Watson testi
library(stats)    # Kolmogorov-Smirnov ve Shapiro-Wilk
library(DescTools)

# Test fonksiyonunu uygulayan bir yardımcı fonksiyon
run_tests <- function(data_column) {
  ks_pval <- ks.test(data_column, "pnorm", mean(data_column), sd(data_column))$p.value
  lilliefors_pval <- lillie.test(data_column)$p.value
  cvm_pval <- cvm.test(data_column)$p.value
  ad_pval <- ad.test(data_column)$p.value
  shapiro_pval <- shapiro.test(data_column)$p.value
  
  return(c(ks_pval, lilliefors_pval, cvm_pval, ad_pval, shapiro_pval))
}

# Bir değişken için test sonuçlarını bir dataframe'e toplayan fonksiyon
generate_test_df <- function(data2, data_30, data_60, column_name) {
  data2_pvalues <- run_tests(data2[[column_name]])
  data30_pvalues <- run_tests(data_30[[column_name]])
  data60_pvalues <- run_tests(data_60[[column_name]])
  
  # Sonuçları bir dataframe'e dönüştür
  pvalues_df <- data.frame(
    Test = c("Kolmogorov-Smirnov", "Lilliefors", "Cramer-von Mises", "Anderson-Darling", "Shapiro-Wilk"),
    Data30 = data30_pvalues,
    Data60 = data60_pvalues,
    DataFull = data2_pvalues
  )
  
  return(pvalues_df)
}

# Listeyi oluştur
pvalues_list <- list(
  Year = generate_test_df(data2, data_30, data_60, "Year"),
  TotalPopulation = generate_test_df(data2, data_30, data_60, "total_population"),
  NetMigration = generate_test_df(data2, data_30, data_60, "net_migration")
)

# Listeyi görüntüle
print(pvalues_list)
