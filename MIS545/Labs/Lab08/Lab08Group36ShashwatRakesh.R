# Building a Naive Bayes model on the dwellingType dataset and then examining 
# the performance of the model.

# Install the tidyverse and e1071 packages
# (comment out the install line after writing it)
# install.packages("tidyverse")
# install.packages("e1071")

# Load the tidyverse and e1071 libraries
library("tidyverse")
library("e1071")

# Set the working directory to your Lab08 folder
setwd("C://Users//ual-laptop//Desktop//Lab08")

# Read DwellingType.csv into a tibble called dwellingType
# (use the data types suggested at the bottom of these assignment instructions). 
# Ensure you use the correct read_csv() function and not readcsv() or read.csv().
dwellingType <-  read_csv(file= "DwellingType.csv", 
                          col_names= TRUE, 
                          col_types= 'filll' )

# Display dwellingType in the console
print(dwellingType)

# Display the structure of dwellingType in the console
print(str(dwellingType))

# Display the summary of dwellingType in the console
print(summary(dwellingType))

# Randomly split the dataset into dwellingTypeTraining (75% of records)
# and dwellingTypeTesting (25% of records) using 154 as the random seed
set.seed(154)
sampleSet = sample(nrow(dwellingType), 
                   round(nrow(dwellingType) * 0.75), 
                   replace= FALSE)
dwellingTypeTraining <- dwellingType[sampleSet, ]
dwellingTypeTesting <- dwellingType[-sampleSet, ]

# Generate the Naive Bayes model to predict DwellingType based on the 
# other variables in the dataset.
dwellingModel <- naiveBayes(formula= DwellingType ~., 
                            data= dwellingTypeTraining, 
                            laplace= 1)

# Build probabilities for each record in the testing dataset 
# and store them in dwellingTypeProbability
dwellingTypeProbability <- predict(dwellingModel, 
                                   dwellingTypeTesting, 
                                   type= "raw")

# Display dwellingTypeProbability on the console
print(dwellingTypeProbability)

# Predict classes for each record in the testing dataset 
# and store them in dwellingTypePrediction
dwellingTypePrediction <- predict(dwellingModel, 
                                   dwellingTypeTesting, 
                                   type= "class")

# Display dwellingTypePrediction on the console=
print(dwellingTypePrediction)

# Evaluate the model by forming a confusion matrix
dwellingConfMatrix <- table(dwellingTypeTesting$DwellingType,
                            dwellingTypePrediction)

# Display the confusion matrix on the console
print(dwellingConfMatrix)

# Calculate the model predictive accuracy 
# and store it into a variable called predictiveAccuracy
predictiveAccuracy <- sum(diag(dwellingConfMatrix)) / nrow(dwellingTypeTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)