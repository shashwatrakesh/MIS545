# Using the tireTread.csv file to learn how to determine outliers, extract
# outliers, perform normalization operations on a column, discretizing, creating
# dummy variables, plotting scatterplot, and adding linear regression line to it

# Set working directory to location where tireTread.csv file is located
setwd("C:\\Users\\ual-laptop\\Desktop\\Lab04")

# Install and Load the tidyverse and dummies package
# Comment the install commands once you have installed the packages
# install.packages("dummies", repos = NULL, type="source")
# install.packages("tidyverse")
library(dummies)
library(tidyverse)

# Reading the csv file, setting its datatypes, and displaying it
tireTread1 <- read_csv("C:\\Users\\ual-laptop\\Desktop\\Lab04\\TireTread.csv",
                       col_types = 'cfnni',
                       col_names = TRUE)
print(tireTread1)

# Display the structure of tireTread1
print(str(tireTread1))

# Display the summary of tireTread1
print(summary(tireTread1))

# Imputing missing data in the UsageMonths feature with the mean of the values
# present in the same column
tireTread2 <- tireTread1 %>%
  mutate(UsageMonths = ifelse(is.na(UsageMonths), mean(UsageMonths, na.rm=TRUE),
                              UsageMonths))

# Display summary of tireTread2 tibble
print(summary(tireTread2))

# Determining outliers in the TreadDepth feature
# Calculating outlier min and max and storing into variables called outlierMin
# and outlierMax
outlierMin <- quantile(tireTread2$TreadDepth, 0.25) - 
  (IQR(tireTread2$TreadDepth) * 1.5)
outlierMax <- quantile(tireTread2$TreadDepth, 0.75) + 
  (IQR(tireTread2$TreadDepth) * 1.5)

# Keeping the outliers in the dataset, but adding the outliers to their own
# tibble called treadDepthOutliers
treadDepthOutliers <- tireTread2 %>%
  filter(TreadDepth < outlierMin | TreadDepth > outlierMax)

# Normalize the UsageMonths feature by taking the log of UsageMonths into a new
# feature called LogUsageMonths and store the additional column in a tibble 
# called tireTread3
tireTread3 <- tireTread2 %>%
    mutate(LogUsageMonths = log(UsageMonths))

# Discretizing UsageMonths into NeedsReplacing having TreadDepth less than equal
# to 1.6mm and store into new tireTread4 tibble
tireTread4 <- tireTread3 %>%
  mutate(NeedsReplacing = TreadDepth <= 1.6)

# Convert tibble tireTread4 into dataframe tireTread4DataFrame
tireTread4DataFrame <- data.frame(tireTread4)

# Dummy coding the Position feature of tireTread4DataFrame and then converting
# the dataframe to tibble tireTread5
tireTread5 <- as_tibble(dummy.data.frame(data = tireTread4DataFrame,
                                         names = "Position"))

# Showing scatterplot of Miles (x-axis) with TreadDepth (y-axis)
scatterPlotMilesTreadDepth <- ggplot(data = tireTread5,
                                     aes(x = Miles,
                                         y = TreadDepth))

# Adding geometry layer and regression best fit line and add title to the plot
scatterPlotMilesTreadDepth + geom_point(color = "dark gray") +
  geom_smooth(method = lm, level = 0, color = "red") + 
  ggtitle("Tire Miles and Tread Depth Scatter Plot.")