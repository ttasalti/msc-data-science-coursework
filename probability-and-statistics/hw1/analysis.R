getwd()
veri<-read.csv(file="train.csv")
head(veri)

#Verinin Sıralı Hali
sort(veri$Ability, decreasing = FALSE)
length(veri$Ability)
# Ortalama
mean(veri$Ability)
# Medyan
median(veri$Ability)
# Mod
as.numeric(names(which.max(table(veri$Ability))))
# Aralık
range(veri$Ability)
# Standart Sapma
sd(veri$Ability)

# Çeyrekler
Q1 <- quantile(veri$Ability, 0.25)
Q3 <- quantile(veri$Ability, 0.75)
print(Q1)
print(Q3)
print(Q3-Q1)

# Sonuçları print etme
print(Q1)
print(Q3)
#Sıklık
table(veri$Ability)

# Boxplot
boxplot(veri$Ability, main = "Ability Boxplot", xlab = "Ability", col = "lightblue",horizontal = T, ylim=c(0,100))
verifrekans <- table(cut(veri$Ability, breaks = c(-0.1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100.1)))
print(verifrekans)


plot(as.numeric(cut(veri$Ability, breaks = c(-0.1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100.1))), 
     veri$Ability, 
     pch = 19, col = "blue", xlab = "Sınıflar", ylab = "Actual Value (Ability)", 
     main = "Her Gözlemin Sınıflara Göre Dağılımı", xaxt = "n", 
     xlim = c(1, 10)) 
axis(1, at = 1:length(levels(cut(veri$Ability, breaks = c(-0.1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100.1)))), 
     labels = levels(cut(veri$Ability, breaks = c(-0.1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100.1))))






hist(veri$Ability, main="Ability Histogram",
xlab = "Ability", ylab = "Frequency", col = "green",xlim=c(0,100))


