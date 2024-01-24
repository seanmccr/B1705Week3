# ----- B1705 Week 3 | Cluster Analysis | 24.01.2024 -----

# ----- 1. Hierarchical Clustering -----

##### 1.1. Loading Data #####
# Loading dataset
df <- read.csv('https://www.dropbox.com/scl/fi/4ka6a7tbz03tdfnu5fb2f/cluster_data_01.csv?rlkey=i5gi1k9ydgxdit65ck98ak3tp&dl=1')
# Showing the dataset headings
head(df,10) # display the first six rows
# Code to provide dataset summary
summary(df)
# Code to show datset strings
str(df)

##### 1.2. Computing Distance Matrix #####

# calculate a distance matrix using Euclidean distance
dist_matrix <- dist(df, method = "euclidean")

##### 1.3. Visualising Distance Matrix #####

# using the pheatmap package
library(pheatmap)
# Code to provide a heatmap of the different data and clusters
pheatmap(dist_matrix)
# Hierarchical clustering code
hc <- hclust(dist_matrix) # performs a basic hierarchical clustering
# Plotting the dendrogram
plot(hc, main="Dendrogram of Hierarchical Clustering")

##### 1.4. Running Hierarchical Cluster Analysis #####

hc <- hclust(dist_matrix, method = "ward.D")

# hc is now in our environment

##### 1.5. Cutting Tree to Form Clusters #####
# Cutting the dendrogram
cutree(hc, k = 3) # Cuts the tree into 3 clusters

##### 1.6. Visualising Cut Dendrogram #####

plot(hc)
# Use the denextend package for enhanced visualisations
library(dendextend)

dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)
plot(dend)

# load the dynamicTreeCut package
library(dynamicTreeCut)

# Cut the dendrogram tree
clusters <- cutreeDynamic(dendro=hc, distM=as.matrix(dist_matrix), method="hybrid")

# Different way of doing so with colours
# Plot the dendrogram with color-coded clusters
plot(hc, labels=FALSE, hang=-1, main="Dendrogram with Dynamic Tree Cut")
rect.hclust(hc, k=length(unique(clusters)), border="red")  # Add colored borders


# load the cluster package
library(cluster)

# Compute silhouette information
sil <- silhouette(cutree(hc, k=3), dist_matrix)

# Plotting the silhouette
plot(sil, col=1:max(sil[,3]), border=NA, main="Silhouette Plot")


##### 1.7. Analysing Results #####

# this command labels each observation depending on which cluster it belongs to
df$cluster <- cutree(hc, k = 3)

# Example R code for scatterplot
df$cluster <- as.factor(df$cluster)  # make sure R knows that cluster is a factor (grouping variable)

library(ggplot2)
ggplot(df, aes(x = AceRate, y = WinRate, color = cluster)) + 
  geom_point() +
  labs(title = "Scatterplot of AceRate vs WinRate by Cluster", x = "AceRate", y = "WinRate") +
  theme_minimal()


##############################################
#library(plotly)

fig <- plotly(df, x = df$DoubleFaultsRate, y = df$WinRate, z = df$FirstServeAccuracy, color = df$cluster, colors = colorspace::rainbow_hcl(6))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Variable 1'),
                                   yaxis = list(title = 'Variable 2'),
                                   zaxis = list(title = 'Variable 3')))
fig

########## RETURN TO THIS TO TRY FIX IT #######
# Loading libraries
library(mclust)
library(cluster)

# Assuming 'hc' is your hierarchical clustering model
# and 'df' is your original data
aic_values <- numeric()
bic_values <- numeric()

max_k <- 5

for (k in 2:max_k) {  # max_k is the maximum number of clusters you want to consider
  # Cut the dendrogram to get k clusters
  clusters <- cutree(hc, k)
  
  # Fit a Gaussian mixture model (as a proxy) to the data for the given number of clusters
  gmm <- Mclust(df, G = k)
  
  # Store the AIC and BIC values
  aic_values[k] <- AIC(gmm)
  bic_values[k] <- BIC(gmm)
}

# Plot AIC and BIC values to find the optimal number of clusters
plot(2:max_k, aic_values[2:max_k], type = "b", col = "blue", xlab = "Number of clusters", ylab = "AIC")
points(2:max_k, bic_values[2:max_k], type = "b", col = "red")
legend("bottomright", legend = c("AIC", "BIC"), col = c("blue", "red"), lty = 1)












