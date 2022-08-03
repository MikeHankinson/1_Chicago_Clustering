# 1_Chicago_Clustering
## Project: K-Means Clustering
### Skills and Tools
1. Investigate the concept of exploratory analysis and recognize its role in the machine learning process.
2. Divide observations into meaningful and representative groups using K-means clustering. Assess the outputs from several models to select the best iteration.
3. Combine similar observations using hierarchical clustering. Consider different measurements of similarity, and the role

• Probability Space
• Random Variables
• Distributions
• Method of Moments
• Hypothesis Testing
• K-means
• K-modes
• Agglomerative Hierarchical Clustering

### Context
Unsupervised learning is a powerful tool that allows us to better understand the relationships between the rows and columns in our dataset, and improves the capacity for interpretation thanks to dimension reduction. Unsupervised learning can be used in exploratory analysis as a purely informative tool, a method of engineering features for supervised learning, or as a self-contained method of analysis that produces inferences and predictions.

This module focussed on one of the core concepts in the realm of unsupervised learning: clustering techniques.

### Problem Statement
Use k-means clustering to segment the athletes in the attached data set into meaningful sub-groups based upon performance criteria.

**Algorithm Methodology**
1.  Scale variables (0-1) to avoid bias created by variables with different scales.  
2.  Pick k number of clusters and randomly divide observations into those k groups.
3.  Calculate the centroid for each cluster.
4.  Calculate the Euclidean distance between every observation and every centroid. 
    Assign each observation to the cluster which has the closest centroid.
5.  Iterate until the clusters stop changing.
6.  Determine number of clusters to use by incorporating the "Elbow" plot.  
7.  Return centroid values to original scale for better result interpretation. 
8.  Plot cluster assignments for visual representation
9.  Interpret results.  

### Results
All exploratory and inferential statistical analysis and clustering can be found in the attached [notebook](Module3_HomeWork_Final_Changed_After_Class.R.html).  


