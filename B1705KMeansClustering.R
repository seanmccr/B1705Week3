# ----- B1705 Week 3 | K-Means Clustering | 24.01.2024 -----

# ----- 1. K-means Clustering: Demonstration -----

##### 1.1. Loading libraries and dataset #####
library(dplyr)
library(ggplot2)

athletes_data <- read.csv('https://www.dropbox.com/scl/fi/f1czg6gk1mvhwmms5j4gz/k_means_01.csv?rlkey=jc4xak7rkgf67omdpri9ymygu&dl=1')

head(athletes_data,10) # display the first six rows

summary(athletes_data)

str(athletes_data)



##### 1.2. Determining optimal cluster numbers #####
# Creating code for clustering
ring_data <- athletes_data[, c("speed", "endurance", "strength", "agility")]

# Determine max number of clusters you want to test
max_clusters <- 10

# Calculate total within-cluster sum of square
wss <- sapply(1:max_clusters, function(k){
  kmeans(clustering_data, centers=k, nstart=10)$tot.withinss
})

# Plot the Elbow Plot
plot(1:max_clusters, wss, type="b", xlab="Number of clusters", ylab="Total within-cluster sum of squares")

##### 1.3. Running K-Means Algorithm #####

# Select only the relevant columns for clustering
clustering_data <- athletes_data[, c("speed", "endurance", "strength", "agility")]

# Perform k-means clustering with 4 clusters
set.seed(123) # Set a random seed for reproducibility
kmeans_result <- kmeans(clustering_data, centers = 4, nstart = 20)

# View the results
print(kmeans_result)


# Adding cluster assignments to the original dataset
athletes_data$cluster <- kmeans_result$cluster
# make sure R knows that 'cluster' is a grouping variable, or factor
athletes_data$cluster <- as.factor(athletes_data$cluster)
# We can view the first few rows to see the cluster assignments
head(athletes_data)


# We use factor analysis when we're trying to find underlying dimensions in our data.
# Looking to boil the data down into smaller number of dimensions to represent outcomes

# Cluster analysis looks to see if it makes sense to group these groups of people/data into
# clusters when moving forward with our work, and to see if it should be clustered

##### 1.4. Examining Results #####
# Code for K-Means results
kmeans_result$centers

##### 1.5. Visualising Clusters #####

# Create a scatter plot using ggplot
ggplot(athletes_data, aes(x = speed, y = endurance, color = factor(cluster))) +
  geom_point() +  # Add points
  labs(color = "Cluster", x = "Speed", y = "Endurance") + # Labeling
  theme_minimal() +  # Minimal theme
  scale_color_brewer(palette = "Set1")  # Use a color palette for better distinction

##### 1.6. Validating our Analysis #####

# Load necessary libraries
library(cluster) # for silhouette
library(stats)   # for kmeans

# Calculating silhouette width
sil_width <- silhouette(kmeans_result$cluster, dist(clustering_data))

# Average silhouette width for the entire dataset
avg_sil_width <- mean(sil_width[, 'sil_width'])

# Print the average silhouette width
print(avg_sil_width)

# Plot the silhouette plot
plot(sil_width, col = 1:max(kmeans_result$cluster), border = NA)


##### 1.7. Further Visualisation Methods #####
# Loading libraries
library(ggplot2)
library(RColorBrewer)
library(gplots)
# Code to create our faceted plots with each cluster in it's own box
ggplot(athletes_data, aes(speed, endurance, color=factor(cluster))) + geom_point() + facet_wrap(~cluster)

# Plot with varying sizes and colour coded clusters
ggplot(athletes_data, aes(agility, strength, size=endurance, color=factor(cluster))) + geom_point(alpha=0.7)

# Violin plot visualisation
ggplot(athletes_data, aes(factor(cluster), speed)) + geom_violin() # Repeat for each variable

