# Install tidyverse and then comment the installation command
# install.packages("tidyverse")

# Load the tidyverse and class packages
library(tidyverse)
library(class)

# Set working directory to the location where SedanSize.csv is present
setwd("C:\\Users\\ual-laptop\\Desktop\\Lab07")

# Read the SedanSize.csv and set datatypes for all features and display it
sedanSize <- read_csv(file = "SedanSize.csv",
                      col_types = 'cfnii',
                      col_names = TRUE)
print(sedanSize)

# Print the structure of sedanSize tibble
print(str(sedanSize))

# Print summary of sedanSize tibble
print(summary(sedanSize))

# Remove make model feature from sedanSize tibble
sedanSize <- sedanSize %>%
                select(-MakeModel)

# Split tibble into two, one with sedanSize and sidanSizeLabels
sedanSizeLabels <- sedanSize %>% 
                select(SedanSize)
sedanSize <- sedanSize %>% 
                select(-SedanSize)

# Creating displayAllHistograms() function
displayAllHistograms <- function(data) {
  data %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") + 
    facet_wrap(~ key, scales = "free") + 
    theme_minimal ()
}

# Call the displayAllHistograms function passing the sedanSize tibble
displayAllHistograms(sedanSize)

# Splitting into training and testing Datasets using seed rate as 517
set.seed(517)

# Create sample_set to generate 75% of random index values for splitting the 
# dataset
sample_set <- sample(nrow(sedanSize),
                    round(nrow(sedanSize) *  0.75),
                    replace = FALSE)

# Creating training datasets using the sedanSize and sedanSizeLabels tibbles
sedanSizeTraining <- sedanSize[sample_set, ]
sedanSizeTrainingLabels <- sedanSizeLabels[sample_set, ]

# Creating testing datasets using the sedanSize and sedanSizeLabels tibbles
sedanSizeTesting <- sedanSize[-sample_set, ]
sedanSizeTestingLabels <- sedanSizeLabels[-sample_set, ]

# Generate the knn model
sedanSizePrediction <- knn(train = sedanSizeTraining,
                          test = sedanSizeTesting,
                          cl = sedanSizeTrainingLabels$SedanSize,
                          k = 7)

# Display the predictions on the testing data onto the console
print(sedanSizePrediction)

# Print the summary of sedanSizePrediction 
print(summary(sedanSizePrediction))

# Create the confusionMatrix of sedanSizePredction Model  and display it
sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                    sedanSizePrediction)
print(sedanSizeConfusionMatrix)

# Calculate the model predictive accuracy and store it into a variable 
# called predictiveAccuracy and display it
predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix))/
  nrow(sedanSizeTesting)
print(predictiveAccuracy)

# Create a matrix of k-values with their predictive accuracy (the matrix will 
# be empty and have 2 columns and 0 rows). Store the matrix into an object 
# called kValueMatrix.
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names of "k value" and "Predictive accuracy" to the kValueMatrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Loop through odd values of k from 1 up to the number of records in the 
# training dataset. With each pass through the loop, store the k-value along 
# with its predictive accuracy. Hint: Watch the video for clarification on how 
# this is accomplished.
for (kValue in 1:nrow(sedanSizeTraining)){
  if (kValue %% 2 != 0) {
    # Generate the model
    sedanSizePrediction <- knn(train = sedanSizeTraining,
                               test = sedanSizeTesting,
                               cl = sedanSizeTrainingLabels$SedanSize,
                               k = kValue)
    
    # Generate confusion matrix
    sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                      sedanSizePrediction)
    
    # Calculate predictive accuracy
    predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix))/
      nrow(sedanSizeTesting)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
  }
}

# Display the kValueMatrix on the console to determine the best k-value
print(kValueMatrix)