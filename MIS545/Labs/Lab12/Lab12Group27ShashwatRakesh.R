# Using the FishingCharter.csv and learning how to scale some features, make a 
# neural network model and evaluating it's performance using Confusion Matrix 
# and predictive Accuracy.

# Install the tidyverse and neuralnet packages (comment out the install line 
# after writing it)
# install.packages("tidyverse")
# install.packages("neuralnet")

# Load the tidyverse and neuralnet libraries
library("tidyverse")
library("neuralnet")

# Set the working directory to your Lab12 folder
setwd("C://Users//ual-laptop//Desktop//Lab12")

# Read FishingCharter.csv into a tibble called fishingCharter (use the data 
# types suggested at the bottom of these assignment instructions). 
fishingCharter  <-  read_csv(file= "FishingCharter.csv", 
                        col_names= TRUE, 
                        col_types= 'lnn')

# Display fishingCharter in the console
print(fishingCharter)

# Display the structure of fishingCharter in the console
print(str(fishingCharter))

# Display the summary of fishingCharter in the console
print(summary(fishingCharter))

# Scale the AnnualIncome feature from 0 to 1
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome)) /
           (max(AnnualIncome) - min(AnnualIncome)))

# Scale the CatchRate feature from 0 to 1
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate)) /
           (max(CatchRate) - min(CatchRate)))

# Randomly split the dataset into fishingCharterTraining (75% of records) and 
# fishingCharterTesting (25% of records) using 591 as the random seed
set.seed(591)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into fishingCharterTraining
fishingCharterTraining <- fishingCharter[sampleSet, ]

# Put all the other records (25%) into fishingCharterTesting
fishingCharterTesting <- fishingCharter[-sampleSet, ]

# Generate the neural network model to predict CharteredBoat (dependent 
# variable) using AnnualIncomeScaled and CatchRateScaled (independent 
# variables). Use 3 hidden layers. Use "logistic" as the smoothing method and 
# set linear.output to FALSE.
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ AnnualIncomeScaled + CatchRateScaled,
  data = fishingCharterTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display the neural network numeric results
print(fishingCharterNeuralNet$result.matrix)

# Visualize the neural network
plot(fishingCharterNeuralNet)

# Use fishingCharterNeuralNet to generate probabilities on the 
# fishingCharterTesting data set and store this in fishingCharterProbability
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)

# Display the probabilities from the testing dataset on the console
print(fishingCharterProbability)

# Convert probability predictions into 0/1 predictions and store this into 
# fishingCharterPrediction
fishingCharterPrediction <- 
  ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(fishingCharterPrediction)

# Evaluate the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)

# Display the confusion matrix on the console
print(fishingCharterConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix))/
  nrow(fishingCharterTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)