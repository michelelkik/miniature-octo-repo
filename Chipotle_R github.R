# Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(cluster)
library(Ecdat)
library(compareGroups)
#library(mice)
#library(missForest)
#library(VIM)

Chipotle <- read.csv("Chipotle.csv")
#View(Chipotle)

#here, the data is output with each variable is on a different row
str(Chipotle)


summary(Chipotle)

#here we are taking away the BILLBOARD, the OTHER_PLACES_TO_EAT column SM and FEMALE column which is almost all 
#zero/binary which messes things up
#colmean <- k[, 20]
#colMeans(colmean)


#remove missing data within the rows
k <- na.omit(Chipotle)               #if we didnt omit anything before with k, i should omit using chipotle as k wasnt used in the first place
#at this moment, data is cleaned from NA values


 k <- k[,-1] #place to eat
 k <- k[,-24]#healthyimportanttome
 k <- k[,-23]#buylocal
 k <- k[,-22]#spending
 k <- k[,-21]#plan
 #k <- k[,-4] 
 #k <- k[,-3] 
 #k <- k[,-2] 
 #k <- k[,-1] 


# Compute Gower distance
gower_dist <- daisy(k, metric = "gower", warnBin = FALSE)
gower_mat <- as.matrix(gower_dist)


# Print most similar clients
k[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
           arr.ind = TRUE)[1, ], ]
# Print most dissimilar clients
k[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
           arr.ind = TRUE)[1, ], ]


#for clusters between 2 and 8 (usual and ideal number of clusters)
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

plot(1:8, sil_width, title("Elbow Method"), xlab = "Number of Clusters", ylab = "Silhouette Width")
lines(1:8, sil_width)


#summary of each cluster

cl <- 2
pam_fit <- pam(gower_dist, diss = TRUE, cl)
pam_results <- k %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary



#visualization
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


k1 <- k[,-20]#remove income to be able to observe other variables' boxplots
k1 <- k1[,-19]#remove age
k1 <- k1[,-18]#female
k1 <- k1[,-5]#patronage
k1 <- k1[,-4]#billboard
k1 <- k1[,-3]#walk
k1 <- k1[,-2]#sm
k1 <- k1[,-1]#wom
k1 <- k1[,-5]#importanttaste

boxplot(k1)