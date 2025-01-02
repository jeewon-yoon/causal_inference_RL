# Load data
setwd("C:/Users/elley/Desktop/MS/dev-causal-inference-main/behavioral_data_analyses")
data = read.csv("anonymized_mining_data.csv")


# group by subjects 
library(dplyr)
data_1 <- data %>% group_by(subject) %>% summarise(Freq=sum(optimal_choice))
data_2 <- data %>% group_by(subject) %>% summarise(Freq=sum(latent_guess))
data_3 <- data %>% group_by(subject) %>% summarise(Freq = mean(age))
data_3$Freq <-round(data_3$Freq)
colnames(data_1)[2]<-"opt"
colnames(data_2)[2]<-"guess"
colnames(data_3)[2]<-"age"

# Take out subjects that has a probability less than 0.6 (and drop out subject id column)
data_4 <- cbind(data_1,data_2$guess,data_3$age)
colnames(data_4)[3]<-"guess"
colnames(data_4)[4]<-"age"
data_4 <- data_4[data_4$opt>=90,]

# Create a column that indicate age category
a <- vector()
a[1:6] <- "blank"
a[7:12] <- "Kid"
a[13:17] <- "Teen"
a[18:26] <- "Adult"
data_4$group <- a[data_4$age]


# Drop the subject id column
data_5 <- data_4[,-1]
# Change the group column as factor variable
data_5$group<-as.factor(data_5$group)


# k-means clustering
library(factoextra)
set.seed(234)
km.res<-kmeans(data_5[,-4], 4, nstart = 25)
print(km.res)

km.res$cluster


# Do the clustering 
library(cluster) #* \label{line:MultipleParticipants:libclust}  *\#
gskmn <- clusGap(data_5[,-4], FUN = kmeans, nstart = 20, K.max = 6, B=500) 
plot(gskmn, ylim=c(0.15, 0.5))


# Another clustering plot
library(tidyverse)
library(data.table)
tab <- data.table(gskmn$Tab)
tab[, k := .I]

clus_p <- ggplot(tab, aes(k, gap)) + geom_line() + geom_point(size = 3) +
  geom_errorbar(aes(ymax = gap + SE.sim, ymin = gap - SE.sim), width = 0.25) +
  ggtitle("Clustering Results") +
  labs(x = "Number of Clusters", y = "Gap Statistic") +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12))
clus_p



# kmeans clustering visualization #1
library(factoextra)
library(ggpubr)
set.seed(123)
res.km<-kmeans(scale(data_5[,-4]), centers=2, nstart=25)
res.km$cluster
df<-data.frame(res.km$cluster)
fviz_cluster(res.km, data = data_5[,-4], palette = c("#2E9FDF", "#E7B800"), 
             geom = "point", ellipse.type = "convex", ggtheme = theme_bw())



# Clustering visualization #2 
# Browse data
head(data_5, 3)

# Compute k-means with k = 2
set.seed(23)
res.km <- kmeans(scale(data_5[, -4]), 2, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

# Dimension reduction using PCA
res.pca <- prcomp(data_5[, -4],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$Group <- data_5$group
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Group", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)
