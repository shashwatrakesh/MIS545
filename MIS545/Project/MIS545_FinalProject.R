# MIS 545-02
# ProjectGroup22ParekhPandeyShuklaYaji.R
# 
# 
# 

# Install the tidyverse and neuralnet packages (comment out the install line 
# after writing it)
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")

# Load the tidyverse, corrplot, olsrr and smotefamily libraries
library("tidyverse")
library("corrplot")
library("olsrr")
library("dummies")
library("smotefamily")

# Set the working directory to your Lab12 folder
setwd("C://Users//ual-laptop//Desktop//MIS 545")

# Read VehicleInsurance.csv into a tibble called vehicleInsurance (use the data 
# types suggested at the bottom of these assignment instructions). 
vehicleInsurance <-  read_csv(file = "VehicleInsurance.csv", 
                             col_names = TRUE, 
                             col_types = 'fnnflflnfnl')

# Display vehicleInsurance in the console
print(vehicleInsurance)

# Display the structure of vehicleInsurance in the console
print(str(vehicleInsurance))

# Display the summary of vehicleInsurance in the console
print(summary(vehicleInsurance))

# Recreate the displayAllHistograms() function 
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key), 
                              color = "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal()
}

# Call the displayAllHistograms() function, passing in vehicleInsurance 
displayAllHistograms(vehicleInsurance)

# Display a correlation matrix of vehicleInsurance rounded to two decimal places
# round(cor(vehicleInsurance1),2)

# Display a correlation plot using the "number" method and limit output to the 
# bottom left
# corrplot(cor(vehicleInsurance),
#          method = "number",
#          type = "lower")

# Convert tibble vehicleInsurance into dataframe vehicleInsuranceDataFrame
vehicleInsuranceDataFrame <- data.frame(vehicleInsurance)

# Dummy coding the Position feature of vehicleInsuranceDataFrame and then converting
# the dataframe to tibble vehicleInsurance1
vehicleInsurance1 <- 
  as_tibble(dummy.data.frame(data = vehicleInsuranceDataFrame,
                             names = "Gender"))

vehicleInsurance2 <- 
  as_tibble(dummy.data.frame(data = vehicleInsuranceDataFrame,
                             names = c("Gender", "Previously_Insured", 
                                       "Vehicle_Age", "Vehicle_Damage",
                                       "Response")))

# Randomly split the dataset into vehicleInsurance1Training (75% of records) and 
# vehicleInsurance1Testing (25% of records) using 203 as the random seed
set.seed(203)
sampleSet <- sample(nrow(vehicleInsurance1),
                    round(nrow(vehicleInsurance1) *  0.75),
                    replace = FALSE)
vehicleInsurance1Training <- vehicleInsurance1[sampleSet, ]
vehicleInsurance1Testing <- vehicleInsurance1[-sampleSet, ]

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