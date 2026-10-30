
head(seeds)

plot(seeds[,1:7])
data <- scale(seeds, center = TRUE, scale = TRUE)
par(mfrow=c(1,2))
boxplot(seeds)
boxplot(data)
par(mfrow=c(1,1))


mypr <- prcomp(seeds[,1:7], center = T, scale = T)
summary(mypr)
library(factoextra)
fviz_eig(mypr, addlabels = TRUE, ylim = c(0, 75))
var <- get_pca_var(mypr)

var$cos2


library(corrplot)
corrplot(var$cos2, is.corr=FALSE)

fviz_contrib(mypr, choice = "var", axes = 1, top = 7)
fviz_contrib(mypr, choice = "var", axes = 2, top = 7)

fviz_pca_var(mypr, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
fviz_pca_ind(mypr,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

mypr
fviz_pca_biplot(mypr, 
                # Fill individuals by groups
                geom.ind = as.factor("point"),
                addEllipses = TRUE, label = "var",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = as.factor(seeds$Type),
                col.ind = as.factor("black"),
                # Color variable by groups
                col.var = factor(c("area", "perimeter", "compactness", "lengthofkernel",
                                   "widthofkernel", "asymmetrycoefficient", "asymmetrycoefficient")),
                
                
                legend.title = list(fill = "Type of seed", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors





#Kmeans with scaled data
library(factoextra)
set.seed(123)

fviz_nbclust(data,kmeans,method = "wss",nstart = 25)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data,kmeans,method = "silhouette")+ #for average silhouette width
  labs(subtitle = "Silhoutte method")
fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method") #or gap statistics


set.seed(123)
km_res <- kmeans(data, 3, nstart=25) 
print(km_res)

summary(seeds)
aggregate(seeds, by=list(cluster=km_res$cluster), mean)
dd=cbind(seeds, cluster=km_res$cluster)
head(dd,10)

fviz_cluster(km_res, data = data,
             ellipse.type = "convex", 
             star.plot = TRUE, 
             repel = TRUE,
             geom = "point",
             ggtheme = theme_minimal()
)

library(caret)

g<-c()
g[km_res$cluster == 2] <- 3
g[km_res$cluster == 3] <- 2
g[km_res$cluster == 1] <- 1

measures_km_sc <- confusionMatrix(data=as.factor(g), reference = as.factor(seeds$Type), positive = "TRUE")

measures_km_sc

##Kmeans with pca data
summary(mypr)
set.seed(123)
data <- predict(mypr)[,1:2]

fviz_nbclust(data,kmeans,method = "wss",nstart = 25)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data,kmeans,method = "silhouette")+ #for average silhouette width
  labs(subtitle = "Silhoutte method")
fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method") #or gap statistics


set.seed(123)
km_res <- kmeans(data, 3, nstart=25) 
print(km_res)
fviz_cluster(km_res, data = data,
             ellipse.type = "convex", 
             star.plot = TRUE, 
             repel = TRUE, 
             geom = "point",
             ggtheme = theme_minimal()
)

summary(seeds)
aggregate(seeds, by=list(cluster=km_res$cluster), mean)

g<-c()
g[km_res$cluster == 2] <- 1
g[km_res$cluster == 3] <- 2
g[km_res$cluster == 1] <- 3
g

measures_km_pc <- confusionMatrix(data=as.factor(g), reference = as.factor(seeds$Type), positive = "TRUE")

measures_km_pc

#### Kmedoids with scaled data
set.seed(123)
data<-scale(seeds)
boxplot(data)


fviz_nbclust(data,pam,method = "wss",nstart = 25)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data,pam,method = "silhouette")+ #for average silhouette width
  labs(subtitle = "Silhoutte method")
fviz_nbclust(data, pam, method= "gap")+
  labs(subtitle = "Gap statistic method") #or gap statistics


set.seed(123)
pam_data <- pam(data,3)
print(pam_data)
pam_data$medoids
pam_data$clustering
pam_data$clusinfo

fviz_cluster(pam_data,
             ellipse.type = "convex",
             repel = TRUE, 
             geom = "point",
             ggtheme = theme_classic()
)

summary(seeds)
aggregate(seeds, by=list(pam_data$clustering), mean)  
aggregate(seeds, by=list(pam_data$clustering), sd)


measures_pam_sc <- confusionMatrix(data=as.factor(pam_data$clustering), reference = as.factor(seeds$Type), positive = "TRUE")

measures_pam_sc

### Kmedoids with pca


summary(mypr)
set.seed(123)

data <- predict(mypr)[,1:2]
summary(data)

fviz_nbclust(data,pam,method = "wss",nstart = 25)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data,pam,method = "silhouette")+ #for average silhouette width
  labs(subtitle = "Silhoutte method")
fviz_nbclust(data, pam, method= "gap")+
  labs(subtitle = "Gap statistic method") #or gap statistics


pam_data <- pam(data,3)
print(pam_data)
fviz_cluster(pam_data,
             ellipse.type = "convex", 
             repel = TRUE, 
             geom = "point",
             ggtheme = theme_classic()
)
summary(seeds)
aggregate(seeds, by=list(pam_data$clustering), mean)  

measures_pam_pc <- confusionMatrix(data=as.factor(pam_data$clustering), reference = as.factor(seeds$Type), positive = "TRUE")

measures_pam_pc







