# Package names
packages <- c("corrplot","stats","cluster","factoextra", "clustertend", "NbClust", "mclust", "dbscan", "fpc", "clValid")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##veri seti-tan?mlay?c? istatistikler
getwd()
data<-read.csv("data.csv",sep = ";", header = TRUE, quote = "", fill = TRUE)
View(data)
dataWithLabel <- data[,c(-1)]
data <- data[,c(-1,-2)]
# Karakter olarak okunan kolonları numeric tipine çevirmek için bir fonksiyon tanımlayın
# Tüm sütunları karakter türünden numeric türüne dönüştürün
convert_to_numeric <- function(column) {
  as.numeric(gsub(",", ".", column))
}

# Tüm sütunlar için dönüşüm uygulayın
data[] <- lapply(data, convert_to_numeric)
raw_data=data
dim(data)
summary(data)
apply(data, 2, sd)
boxplot(data)
library(hopkins)
hopkins(data, nrow(data)-1)



        
corr=cor(data, method = "pearson")
corr
corrplot.mixed(corr,upper="pie")


##Temel Bile?enler Analizi
data.pca <- prcomp(data, center = TRUE, scale. = TRUE)
summary(data.pca)
(data.pca$sdev)^2 #?zde?erler

fviz_eig(data.pca)


data.pca$rotation #özvektörler
data.pca$x[,1:2]
cor(data.pca$x[,1],data.pca$x[,2])



# Results for Variables
res.var <- get_pca_var(data.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation


corrplot(res.var$cos2, is.corr=FALSE) # Quality of representation 
fviz_contrib(data.pca, choice = "var", axes = 1, top = 9)
fviz_contrib(data.pca, choice = "var", axes = 2, top = 9)


fviz_pca_var(data.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)



# Results for individuals
res.ind <- get_pca_ind(data.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation

data
summary(data)

##Graph of individuals. Individuals with a similar profile are grouped together
fviz_pca_ind(data.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


data=scale(data)
summary(data)
apply(data, 2, sd)
boxplot(data)

##Hopkins _ Kümelenme Eğilimi
set.seed(123)
if (!require(hopkins)) install.packages("hopkins")
library(hopkins)

hopkins(data, nrow(data)-1)
##0'a yakın çıkması istenir. 0.5'e yakın çıkması uniform dağıldığını ani kümelemeye uygun olmadığını gösterir
hopkins(data.pca$x[,1:2], nrow(data)-1)

#####distance
dist_euc=get_dist(data.pca$x[,1:2], stand = TRUE, method="euclidean")
#View(as.matrix(dist_euc))
#as.matrix(dist_euc)[21:30,21:30]
fviz_dist(dist_euc)



dist_man=get_dist(data.pca$x[,1:2], stand = TRUE, method="manhattan")
#View(as.matrix(dist_man))
#as.matrix(dist_man)[21:30,21:30]
fviz_dist(dist_man)



#30 indices for choosing the best number of clusters


fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200,  method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(data, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
nb <- NbClust(data, distance = "manhattan", min.nc = 2,
              max.nc = 9, method = "kmeans")

###pca_kmeans
fviz_nbclust(data.pca$x[,1:2], kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
fviz_nbclust(data.pca$x[,1:2], kmeans, nstart = 25, iter.max = 200,  method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(data.pca$x[,1:2], kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

set.seed(123)
km_data <- kmeans(data, 2, nstart=25) 
print(km_data)
km_data <- kmeans(data, 3, nstart=25) 
print(km_data)
km_data <- kmeans(data, 4, nstart=25) 
print(km_data)
##k=2
set.seed(123)
km_data_pca <- kmeans(data.pca$x[,1:2], 2, nstart=25) 
print(km_data_pca)


fviz_cluster(km_data_pca, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


summary(data)
aggregate(data, by=list(cluster=km_data_pca$cluster), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_pca <- cbind(data, cluster=km_data_pca$cluster)
dd_pca


##k=3
set.seed(123)
km_data_pca <- kmeans(data.pca$x[,1:2], 3, nstart=25) 
print(km_data_pca)


fviz_cluster(km_data_pca, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


summary(data)
aggregate(data, by=list(cluster=km_data_pca$cluster), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_pca <- cbind(data, cluster=km_data_pca$cluster)

##k=4
set.seed(123)
km_data_pca <- kmeans(data.pca$x[,1:2], 4, nstart=25) 
print(km_data_pca)


fviz_cluster(km_data_pca, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


summary(raw_data)
aggregate(raw_data, by=list(cluster=km_data_pca$cluster), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_pca <- cbind(raw_data, cluster=km_data_pca$cluster)

table(dd[,5],dd_pca[,5]) ###aynı sonuçlar

###pca_kmeans
fviz_nbclust(data, pam, method = "wss") +
  labs(subtitle = "Elbow method")
fviz_nbclust(data, pam,  method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(data, pam, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
set.seed(123)
pam_data_2 <- pam(data,2)
print(pam_data_2)
pam_data_2$clusinfo
fviz_cluster(pam_data_2,ellipse.type = "t", repel = TRUE, ggtheme = theme_classic())

##kmedoids k=3
set.seed(123)
pam_data_3 <- pam(data,3)
print(pam_data_3)



fviz_cluster(pam_data_3,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

##kmedoids k=4
set.seed(123)
pam_data_4 <- pam(data,4)
print(pam_data_4)

fviz_cluster(pam_data_4,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


fviz_cluster(pam_data_4,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam_data_4$clusinfo


####aşamalı kümeleme
## öklid - ward.D2 bağlantı fonksiyonu ile
hc_e <- hclust(d=dist_euc, method="ward.D2")
fviz_dend(hc_e,cex=.5)
coph_e <- cophenetic(hc_e)
cor(dist_euc,coph_e) #Bu değerin yüksek çıkması kümelemenin geçerli olduğunu gösterir. 

## öklid - average bağlantı fonksiyonu ile
hc_ea <- hclust(d=dist_euc, method="average")
fviz_dend(hc_ea,cex=.5)
coph_ea <- cophenetic(hc_ea)
cor(dist_euc,coph_ea) #Bu değerin yüksek çıkması kümelemenin geçerli olduğunu gösterir. 

##manhattan - ward.D2 bağlantı fonksiyonu ile
hc_m <- hclust(d=dist_man, method="ward.D2")
coph_m <- cophenetic(hc_m)
cor(dist_man,coph_m)

## manhattan - average bağlantı fonksiyonu ile
hc_ma <- hclust(d=dist_man, method="average")
coph_ma <- cophenetic(hc_ma)
cor(dist_man,coph_ma)


nb <- NbClust(data, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "average")

fviz_nbclust(data, FUN = hcut, method = "wss")
fviz_nbclust(data, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(data, FUN = hcut, nstart = 25, K.max = 10, B = 500)
fviz_gap_stat(gap_stat)

#### cut tree in 3 groups

#hc_e, hc_ea, hc_m, hc_ma 
grup <- cutree(hc_e, k=3)
fviz_dend(hc_e, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())
#pca data ile 3
grup <- cutree(hc_e, k=3)
fviz_dend(hc_e, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

grup <- cutree(hc_e, k=4)
fviz_dend(hc_e, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())
grup <- cutree(hc_e, k=5)
fviz_dend(hc_e, k = 5, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_cluster(list(data = data, cluster = grup),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())


summary(raw_data)
aggregate(raw_data, by=list(cluster=grup), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_hc <- cbind(raw_data, cluster=grup)
dd_hc


#Computing cluster validation statistics
# K-means clustering
km_data <- eclust(data.pca$x[,1:2], "kmeans", k = 4, nstart = 25, graph = TRUE)
km_data
# Visualize k-means clusters
fviz_cluster(km_data, geom = "point", ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal())
##Silhouette plot kmeans
fviz_silhouette(km_data, palette = "jco",
                ggtheme = theme_classic())

silinfo_km <- km_data$silinfo
silinfo_km
fviz_cluster(list(data = data, cluster = km_data$cluster), data = data,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


# Hierarchical clustering
hc_data <- eclust(data, "hclust", k = 4, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc_data, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)
##Silhouette plot aşamalı kümeleme
fviz_silhouette(hc_data, palette = "jco",
                ggtheme = theme_classic())

silinfo_hc <- hc_data$silinfo
silinfo_hc
fviz_cluster(list(data = data, cluster = hc_data$cluster), data = data,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


#Compare clustering algorithms 
clmethods <- c("kmeans","pam","hierarchical")
intern <- clValid(data.pca$x[,1:2], nClust = 3:10,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
par(mfrow=c(1,3))
plot(intern)
#Dunn endeksinin değeri büyük olmalıdır. Veri kümesi kompakt ve iyi ayrılmış kümeler içeriyorsa, kümelerin çapının küçük olması ve kümeler arasındaki mesafenin büyük olması beklenir.
#connectivity endeksinin değeri küçük olmalıdır. (Nesnelerin veri alanındaki en yakın komşularıyla aynı kümeye ne ölçüde yerleştirildiğine karşılık gelir. )
#silhouette endeksinin değeri büyük olmalıdır

# Stability measures
clmethods <- c("kmeans","pam","hierarchical")
rownames(data.pca$x[,1:2]) <- 1:nrow(data.pca$x[,1:2])
stab <- clValid(data, nClust = 3:10, clMethods = clmethods,
                validation = "stability")
summary(stab)
# Display only optimal Scores
optimalScores(stab)
par(mfrow=c(2,2))
plot(stab)
##APN,AD,ADM,FOM küçük değerler almalıdır.
###https://cran.r-project.org/web/packages/clValid/vignettes/clValid.pdf

####hierarcical k means
res.hk <-hkmeans(data.pca$x[,1:2], 4)
# Print the results
res.hk
# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

summary(raw_data)
aggregate(raw_data, by=list(cluster= res.hk$cluster), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_hk <- cbind(raw_data, cluster=res.hk$cluster)
dd_hk


###model-based clustering
mc <- Mclust(data.pca$x[,1:2])
summary(mc)
#View(mc)
mc$z
head(mc$classification,10)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)
mc=Mclust(data.pca$x[,1:2], G = 4, modelNames = "VVI")
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")

summary(raw_data)
aggregate(raw_data, by=list(cluster= mc$classification), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_mc <- cbind(raw_data, cluster=mc$classification)
dd_mc

#### Density-Based Clustering
# Compute DBSCAN using fpc package
par(mfrow=c(1,1))
set.seed(123)
?kNNdistplot
dbscan::kNNdistplot(data.pca$x[,1:2], k = 4)
?dbscan
db <- fpc::dbscan(data, eps = 1.3, MinPts = 4)
print(db)
# Plot DBSCAN results
fviz_cluster(db, data = data, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
db$cluster

summary(raw_data)
aggregate(raw_data, by=list(cluster= db$cluster), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?
dd_db <- cbind(raw_data, cluster=db$cluster)
dd_db



