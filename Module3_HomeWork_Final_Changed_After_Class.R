# ===============================
# Module 3 Homework - Clustering
# Mike Hankinson
# 10/12/2021
# ===============================

# 1.  Use k-means clustering to segment the athletes in the attached data set into meaningful sub-groups.
# _______________________________

#Open Library Packages
# -------------------------------
library(openxlsx)

library(cluster.datasets)
library(tidyverse)
library(gridExtra)

library(cluster)
library(factoextra)

# install.packages("klaR")
# install.packages("rlang")
# install.packages("labelled")


#Import Data Set
# -------------------------------
athlete_data <- read.xlsx("Assignment_3_data.xlsx", sheet="athlete_data")
head(athlete_data)
tail(athlete_data)

#  Data Set:
#     - Contains 8 variables
#         1. Total Distance
#         2  Acceleration Load
#         3. Speed Load
#         4. Metabolic Exertion 
#         5. Sprints per Minute
#         6. High Accelerations per Minute
#         7. Maximum Speed
#         8. Maximum Acceleration
#     - Data contains 490 athletes (# rows)


#Get to Know the Data - Distributions
# -------------------------------

plot1 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Total.Distance)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "blue") +
  labs(x = "", y="Total Distance (m)")


plot2 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Acceleration.Load)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "orange") +
  labs(x = "", y="Accelaration Load (NA)")


plot3 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Speed.Load)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "green") +
  labs(x = "", y="Speed Load (NA)")


plot4 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Metabolic.Exertion)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "red") +
  labs(x = "", y="Metabolic Exertion (J)")

plot5 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Sprints.per.Minute)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "violet") +
  labs(x = "", y="Sprints per Minute")


plot6 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = High.Accelerations.per.Minute)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "yellow") +
  labs(x = "", y="High Accelerations per Minute")

plot7 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Maximum.Speed)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "black") +
  labs(x = "", y="Maximum Speed (m/s)")

plot8 <- athlete_data %>% 
  ggplot(aes(x = "All Athletes", y = Maximum.Acceleration)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "blue") +
  labs(x = "", y="Maximum Acceleration (m/s2)")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)




# Clean and Normalize Data Set
# -------------------------------

#Normalize, 0-1, columns 2-9 of athlete_data and save to kmeans_athlete_data
kmeans_athlete_data <- sapply(athlete_data[,2:9], function(z) (z-min(z))/(max(z)-min(z)))

#Column bind "Athlete" column from data set with normalized columns
#kmeans_athlete_data <- cbind(athlete_data$Athlete, kmeans_athlete_data[,1:8 ])

 

# **Temporarily removed below after class**
# kmeans_athlete_data <- cbind(kmeans_athlete_data,athlete_data$Athlete)
##Rename first column back to "Athlete"
# colnames(kmeans_athlete_data)[9] <- 'Athlete'



# a. Run k-means for several different numbers of clusters.
# b. Analyze within sum of squares using an elbow plot.
# c. Observe and discuss the size of the clusters.
# -------------------------------

# ***Algorithm Methodology***
# 1.  Scale variables (0-1) to avoid bias created by variables with different scales.  
# 2.  Pick k number of clusters and randomly divide observations into those k groups.
# 3.	Calculate the centroid for each cluster.
# 4.	Calculate the Euclidean distance between every observation and every centroid. 
#     Assign each observation to the cluster which has the closest centroid.
# 5.	Iterate until the clusters stop changing.
# 6.  Determine number of clusters to use by incorporating the "Elbow" plot.  
# 7.  Return centroid values to original scale for better result interpretation. 
# 8.  Plot cluster assignments for visual representation
# 9.  Interpret results.  


# Model 2-7 clusters
#Create vector to record in-cluster sum-of-squares model run
withinss <- numeric(7) 
# Initialize empty 7X9 matrix to record proportion of population in each cluster for each model iteration
size <- matrix(NA,7,9) 
# Populate column 1 with number of clusters for each iteration (2 clusters - 8 clusters) 
size[,1] <- 2:8
# Set n to equal the number of rows in the kmeans_athlete_data data set
n <- nrow(kmeans_athlete_data)


for(i in 2:8){
  name <- paste("km.object.of7", i, sep = ".") #create name based upon the number of cluster
  temp <- assign(name, kmeans(kmeans_athlete_data, centers = i, nstart=50)) #Use assign() to perform Kmeans based upon index, i. saves a kmeans object for each loop iteration
  withinss[i-1] <- temp$tot.withinss #saves the within-cluster sum-of-squares and the appropriate index
  size[i-1, 2:(i+1)] <- round(temp$size/n, 4) #save the cluster size proportions
  }         



# View Elbow Plot and Size Proportions
# -------------------------------
plot(withinss, type="b", pch=16, xaxt="n",xlab="# of clusters", main="Elbow Plot")
axis(1, at=1:7, labels=2:8)
size




# By Analyzing the chart, we can see that when the number of groups (K) 
# reduces from 4 to 3 there is a big increase in the sum of squares, 
# bigger than any other previous increase. 
# The main purpose is to find a fair number of groups that could explain satisfactorily a considerable part of the data.
# Based upon elbow plot, select and evaluate 3 clusters. 


# d. Observe and discuss the centroids of the clusters.
# Recover Centroid Values to Original Scale
# -------------------------------

# --Evaluation of 3 Clusters--

(centroids <- km.object.of7.3$centers)

restored.centers <- data.frame(matrix(0,nrow=3,ncol=8))
names(restored.centers) <- names(athlete_data[2:9]) ### from 2:9
for(i in 1:8){
  restored.centers[,i] <- round(centroids[,i]*(max(athlete_data[,i+1])-min(athlete_data[,i+1]))+min(athlete_data[,i+1]),2)
}

table(restored.centers) 
restored.centers



# e. Include any plots or visualizations you think might be helpful.
# -------------------------------

# --Evaluation of 3 Clusters--


# Silhouette refers to a method of interpretation and validation of consistency 
# within clusters of data.  It is a measure of similarity to its own
# cluster compared to other clusters.  

# Silhouette width*....
# Si > 0 means that the observation is well clustered. The closest it is to 1, the best it is clustered.
# Si < 0 means that the observation was placed in the wrong cluster.
# Si = 0 means that the observation is between two clusters.
# *Reference: towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967


# Silhouette Plot
library(cluster)
library(factoextra)
sil <- silhouette(km.object.of7.3$cluster, dist(kmeans_athlete_data))
fviz_silhouette(sil)


# Clustering Interpretation Plot -- Interactive Plot
library(GGally)
library(plotly)

athlete_data$cluster <- as.factor(km.object.of7.3$cluster)
p <- ggparcoord(data = athlete_data, columns = c(2:9), groupColumn = "cluster", scale = "std") + labs(x = "Physical Activity", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)

athlete_data$cluster 


#*****Plot from Class****
my.color <- c("forestgreen", "blue", "red")[km.object.of7.3$cluster]
plot(athlete_data$Total.Distance, athlete_data$Sprints.per.Minute, pch=16, col=my.color, main="Distance vs. Sprint Rate")







#Results Table Included at the Bottom along with Hierarchical Clustering Data.  


# f. Discuss your findings and interpret results.
# Verify Proper Categorization within Clusters
# -------------------------------
# The silhouette plot provides evidence for 3 clusters since there are 
# no negative silhouette widths and all values exceed 0.50.

# The Clustering Interpretation Plot is not useful in this case.  
# Primary Component Analysis (PCA) may be useful to cut the number of 
# features.  


# ___________________________________________________________________________________________


# ___________________________________________________________________________________________


# 2. Hierarchical Clustering
#   -Using the same data set, perform hierarchical clustering to see how the results compare 
#    to those of k-means clustering. 
#   -Use multiple linkage methods if you think that it is necessary. 
#   -Discuss whether your hierarchical clustering results help inform any previous findings
#    from k-means exploration and whether they confirm or contrast this analysis.
# _______________________________

# Install Packages to Load Library "klaR"
# -------------------------------
# install.packages("klaR")
# install.packages("rlang")
# install.packages("labelled")


#Load and display data
# -------------------------------
athlete_data_2 <- read.xlsx("Assignment_3_data.xlsx", sheet="athlete_data")



#Select only the numeric metrics and scale data for clustering
# -------------------------------
athlete_data_2 <- athlete_data_2[,1:9]
athlete_data_2 <- scale(athlete_data_2)



#Perform Hierarchical clustering
# -------------------------------

# ***Algorithm Methodology***
# 1.  Determine linking method (single, average, complete or centroid)
# 2.  Determine distances between ALL observations(Use a nx-n Dissimilarity Matrix)
# 3.	Visualize the dendogram
# 4.	Identify two most similar objects
# 5.	Identify potential cutpoint(s)
# 6.  Perform the cut and save the clusters
# 7.  Interpret results


# Step 1 - Linking Method
# ...............................
# - a. Use complete method in the hclust() function
# - b. Use average method in the hclust() function


# Step 2 - Determine distances between all observations
# ...............................
# - Use dist() function -- dissimilarity matrix
# - Use hclust() to complete hierarchical cluster analysis 
#   on the dissimilarity matrix.
# - Save results into hc (hierarchical cluster) variable

hc_complete <- hclust(dist(athlete_data_2),method = "complete")
hc_average <- hclust(dist(athlete_data_2),method = "average")

# - Confirm the linkage method and distance measure
hc_complete$method       # [1] "complete"
hc_complete$dist.method  # [1] "euclidean"

hc_average$method       # [1] "average"
hc_average$dist.method  # [1] "euclidean"


# Step 3 - Visualize the dendogram
# ...............................
# - Visualize the dendrogram 
plot(as.dendrogram(hc_complete))
plot(as.dendrogram(hc_average))

# - see the heights where clusters were joined
hc_complete$height      # [1]   60.72892
hc_average$height       # [1]   60.72892 


# Step 4 - Identify two most similar athletes
# ...............................
# - Use cutree() function to cut the tree, e.g., as resulting from hclust, 
#   into several groups based upon the cut heights.
# - h=60.8 is the height after the first join was made.  This provide the result 
#   after first join...and, it demonstrates the 2 most similar athletes in the dataset.  

cutree(hc_complete,h=60.8)     
# Note for COMPLETE linking method: Each athlete is in its own individual cluster except 
# Athletes 217 and 233 are both in cluster 217.  These are the 2 most similar athletes  


cutree(hc_average,h=60.8)
# Note for COMPLETE linking method:
# Again, Athletes 217 and 233 are both in cluster 217.  These are the 2 most similar athletes  


# Zoom in the Y-axis between 0 and 1, able to visualize the most similar countries.
plot(as.dendrogram(hc_complete),ylim=c(0, 300))
plot(as.dendrogram(hc_average),ylim=c(0,300))
# Due to the large number of joins, it is difficult to visually concentration of joins  



# Step 5 - Identify potential cutpoint(s)
# ...............................
plot(as.dendrogram(hc_complete))
abline(h=300,lty=2,col="dodgerblue")

plot(as.dendrogram(hc_average))
abline(h=300,lty=2,col="dodgerblue")


# Step 6 - Perform cut and save groups
# ...............................
(my.cut.complete <- cutree(hc_complete,h=300))
(my.cut.average <- cutree(hc_average,h=300))


# Go through each value of cluster to print the different country names that 
# belong to that value.  
# This provides the final result. 

athlete.groups.complete <- vector("list",max(my.cut.complete))
for(i in 1:length(athlete.groups.complete)){
  athlete.groups.complete[[i]] <- names(my.cut.complete)[which(my.cut.complete==i)]
}
athlete.groups.complete



athlete.groups.average <- vector("list",max(my.cut.average))
for(i in 1:length(athlete.groups.average)){
  athlete.groups.average[[i]] <- names(my.cut.average)[which(my.cut.average==i)]
}
athlete.groups.average





# For this large number of athletes and features, I do not like the methodology for performing
# cuts, above.  

# Therefore, I would like to place athletes in 5 distinct buckets (clusters).
# This is similar to the star rating utilized by college football recruiting sites
# for perspective high school football players.  


(my.cut.complete.5clusters <- cutree(hc_complete,k=5, h=NULL))
(my.cut.average.5clusters <- cutree(hc_average,k=5, h=NULL))



athlete.groups.complete.5clusters <- vector("list",max(my.cut.complete.5clusters))
for(i in 1:length(athlete.groups.complete.5clusters)){
  athlete.groups.complete.5clusters[[i]] <- names(my.cut.complete.5clusters)[which(my.cut.complete.5clusters==i)]
}
athlete.groups.complete.5clusters



athlete.groups.average.5clusters <- vector("list",max(my.cut.average.5clusters))
for(i in 1:length(athlete.groups.average.5clusters)){
  athlete.groups.average.5clusters[[i]] <- names(my.cut.average.5clusters)[which(my.cut.average.5clusters==i)]
}
athlete.groups.average.5clusters



# Step 7 - Results
# ...............................

# Agglomerative Hierarchical Clustering Results for Both 
# Complete and Average Linking Methods Together with K-Means Clustering:

#============================================================================
#                      Complete Method       Average Method       K-Means   #                    
#                      _______________       ______________       _______   #
# Cluster Size 1             201                   459              125     #
# Cluster Size 2             117                    23              169     # 
# Cluster Size 3             136                     6              196     #   
# Cluster Size 4              30                     1                      #                  
# Cluster Size 5               6                     1                      # 
#                      _______________       ______________       _______   #                   
# Total Athletes             490                   490              490     #     
#============================================================================

# In this analysis, I chose to move from 3 clusters in K-Means to 5 clusters
# using hierarchical cluster.  Therefore, there isn't a direct comparison of data 
# across methodologies.  
# However, given the high number of features, it might be prudent to 
# consolidate using PCA.  
# I am surprised at the difference between the agglomerative methods.
# In addition, I would like to spend additional time re-evaluating 
# the K-Means method.  I question the likelihood that all 3 clusters 
# would contain the same population.  Perhaps a programming mishap.  










# Function Descriptions
# -------------------------------
# ?nrow()
# ?paste() #Concatenate Strings. Concatenate vectors after converting to character.
# # sep = ,  A character string to separate the terms
# ?assign() #Assign a Value to a Name
# ?kmeans()  # Perform k-means clustering on a data matrix.
# #names() #gets or sets the names of objects
# ?silhouette()
# ?cutree()
# 



#    rm(list = ls())      Removes global environment





























