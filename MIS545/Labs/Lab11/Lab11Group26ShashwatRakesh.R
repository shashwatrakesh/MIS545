# Practicing how to import csv files, assigning data types, generating clusters 
# and interpreting clusters. Using the CountryData.csv and generating clusers 
# using the k-means clustering method and optimizing the model and interpreting 
# the clusters.

# Install the tidyverse and factoextra packages
# (comment out the install line after writing it)
# install.packages("tidyverse")
# install.packages("factoextra")

# Load the tidyverse, stats, factoextra, cluster, and gridExtra libraries
library("tidyverse")
library("stats")
library("factoextra")
library("cluster")
library("gridExtra")

# Set the working directory to your Lab11 folder
setwd("C://Users//ual-laptop//Desktop//Lab11")

# Read CountryData.csv into an object called countries using the 
# read_csv() function
countries  <-  read_csv(file= "CountryData.csv", 
                       col_names= TRUE, 
                       col_types= 'cnnnnini')

# Display the countries tibble on the console
print(countries)

# Display the structure of the countries tibble
print(str(countries))

# Display a summary of the countries tibble
print(summary(countries))

# Convert the column containing the country name to the row title of the tibble 
# (this is a requirement for later visualizing the clusters)
countries <- countries %>% column_to_rownames(var = 'Country')

# Remove countries from the tibble with missing data in any feature using the 
# following code:
countries <- countries %>% drop_na()

# View the summary of the countries tibble again to ensure there are no
# NA values
print(summary(countries))
 
# We are going to cluster the countries based on their corruption index and the 
# number of days it takes to open a business. Create a new tibble called 
# countriesScaled containing only these two features and scale them so they 
# have equal impact on the clustering calculations.
countriesScaled <- countries %>%
  select(CorruptionIndex, DaysToOpenBusiness) %>% scale() 

# Set the random seed to 679
set.seed(679)

# Generate the k-means clusters in an object called countries4Clusters using 
# 4 clusters and a value of 25 for nstart
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)

# Display cluster sizes on the console
countries4Clusters$size

# Display cluster centers (z-scores) on the console
countries4Clusters$centers

# Visualize the clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)

# Optimize the value for k by evaluating the elbow method, 
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "wss")

# Average silhouette method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "silhouet")

# Gap statistic method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "gap_stat")

# Regenerate the cluster analysis using the optimal number of clusters
countriesClusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)

# Display cluster sizes on the console
countriesClusters$size

# Display cluster centers (z-scores) on the console
countriesClusters$centers

# Visualize the clusters
fviz_cluster(object = countriesClusters,
             data = countriesScaled,
             repel = FALSE)

# Determine similarities and differences among the clusters using the remaining 
# features in the dataset (GiniCoefficient, GDPPerCapita, EduPercGovSpend, 
# EduPercGDP, and CompulsoryEducationYears
countries %>% 
  mutate(cluster = countriesClusters$cluster) %>%
  select(cluster,
         GiniCoefficient,
         GDPPerCapita,
         EduPercGovSpend,
         EduPercGDP,
         CompulsoryEducationYears) %>%
  group_by(cluster) %>%
  summarise_all("mean")