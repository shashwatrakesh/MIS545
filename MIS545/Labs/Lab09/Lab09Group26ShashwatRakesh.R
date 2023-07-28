# Learning how to prepare dataset, and create a Decision tree Model and then 
# evaluating the model on the basis of Confusion Matrix and predictive Accuracy
# We also try to check the impact of complexity parameter on model performance

# Install the tidyverse and rpart.plot packages
# (comment out the install line after writing it)
# install.packages("tidyverse")
# install.packages("rpart.plot")

# Load the tidyverse, rpart and rpart.plot libraries
library("tidyverse")
library("rpart")
library("rpart.plot")

# Set the working directory to your Lab09 folder
setwd("C://Users//ual-laptop//Desktop//Lab09")

# Read IndonesianRiceFarms.csv into a tibble called riceFarms
# (use the data types suggested at the bottom of these assignment instructions) 
# Ensure you use the correct read_csv() function and not readcsv() or read.csv()
riceFarms <-  read_csv(file= "IndonesianRiceFarms.csv", 
                          col_names= TRUE, 
                          col_types= 'fniiinf')

# Display riceFarms in the console
print(riceFarms)

# Display the structure of riceFarms in the console
print(str(riceFarms))

# Display the summary of riceFarms in the console
print(summary(riceFarms))

# Randomly split the dataset into riceFarmsTraining (75% of records)
# and riceFarmsTesting (25% of records) using 370 as the random seed
set.seed(370)
sampleSet = sample(nrow(riceFarms), 
                   round(nrow(riceFarms) * 0.75), 
                   replace= FALSE)
riceFarmsTraining <- riceFarms[sampleSet, ]
riceFarmsTesting <- riceFarms[-sampleSet, ]

# Generate the decision tree model to predict FarmOwnership based on the other 
# variables in the dataset. Use 0.01 as the complexity parameter.
riceFarmsModel <- rpart(formula = FarmOwnership ~ .,
                        method = "class",
                        cp = 0.01,
                        data = riceFarmsTraining)

# Display the decision tree plot
rpart.plot(riceFarmsModel)

# Predict classes for each record in the testing dataset and store them in 
# riceFarmsPrediction and display it
riceFarmsPrediction <- predict(riceFarmsModel,
                               riceFarmsTesting,
                               type = "class")
print(riceFarmsPrediction)

# Evaluate the model by forming a confusion matrix and display it
ricefarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                   riceFarmsPrediction)
print(ricefarmsConfusionMatrix)

# Calculate predictive accuracy of the model and display it
predictiveAccuracy <- sum(diag(ricefarmsConfusionMatrix)) / 
  nrow(riceFarmsTesting)
print(predictiveAccuracy)

# Generate a new model with complexity parameter as 0
riceFarmsModel <- rpart(formula = FarmOwnership ~ .,
                        method = "class",
                        cp = 0.007,
                        data = riceFarmsTraining)

# Display the decision tree plot
rpart.plot(riceFarmsModel)

# Predict classes for each record in the testing dataset and store them in 
# riceFarmsPrediction
riceFarmsPrediction <- predict(riceFarmsModel,
                               riceFarmsTesting,
                               type = "class")


# Evaluate the model by forming a confusion matrix
ricefarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Calculate predictive accuracy of the model and display it
predictiveAccuracy <- sum(diag(ricefarmsConfusionMatrix)) / 
  nrow(riceFarmsTesting)
print(predictiveAccuracy)