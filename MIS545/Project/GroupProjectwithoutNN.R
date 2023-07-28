# Install package
# install.packages("tidyverse")
# install.packages("fastDummies")
# install.packages("rpart.plot")
# install.packages("corrplot")
# install.packages("ggplot2")
# # install.packages("caret")
# install.packages("lattice")
# install.packages("Rtools")
# install.packages("smotefamily")
# install.packages("e1071")
# install.packages("class")
# install.packages("neuralnet")

# Selecting the library
library(ggplot2)
library(tidyverse)
library(fastDummies)
library(e1071)
library(corrplot)
library(smotefamily)
#library(caret)
library(rpart.plot)
library(class)
# library(neuralnet)


# Set working directory
setwd("C:\\Users\\ual-laptop\\Desktop\\MIS 545")

# Read the CSV File
vehicleInsurance <- read_csv(file="VehicleInsurance.csv",
                             col_types = "fninicinnni",
                             col_names = TRUE)

# Display vehicleInsurance
print(vehicleInsurance)

# Summary of vehicleInsurance
summary(vehicleInsurance)

#Queries
print(filter(.data = vehicleInsurance,
             (Previously_Insured == 0 & Vehicle_Age == "> 2 Years")))



print(filter(.data = vehicleInsurance,
             (Age <= 25 & Annual_Premium >= mean(Annual_Premium))))


query3 <- vehicleInsurance %>%
  filter(Age >= 21) %>%
  filter(Age <=75) %>%
  group_by(Age) %>%
  summarize(AverageAnnualPremium = mean(Annual_Premium))


ggplot(query3, aes(x= Age, y= AverageAnnualPremium)) +
  geom_line(color="blue") +
  xlab("Age (Years)") +
  ylab("Average Annual Premium in USD") +
  geom_smooth(method = lm, color="red", level=0) +
  ggtitle("Average Annual Premium for Customers (21 to 75 years)")

# Remove rows with null
drop_na(vehicleInsurance)

# Data Exploration
#group_by(vehicleInsurance,)


# Dummy code Gender
vehicleInsurance <- vehicleInsurance %>%
  mutate(Gender = ifelse(Gender == "Male", 0,1))


# # Corelation plot initial
# corrplot(cor(vehicleInsurance),
#          method = "number",
#          type="lower",
#           
# 
# round(cor(vehicleInsurance%>%
#             keep(is.numeric)),2)

# Set ranges to Age
vehicleInsurance <- vehicleInsurance %>%
  mutate(Age = ifelse(Age <=30, 1,
                      ifelse(Age <= 40, 2,
                             ifelse(Age <= 50, 3, 
                                    ifelse(Age <= 60, 4, 
                                           ifelse(Age <= 70, 5, 
                                                  ifelse(Age <= 80, 6,
                                                         ifelse(
                                                           Age <= 90, 7, 8)
                                                  )))))))

# Set range to Vehicle age
vehicleInsurance <- vehicleInsurance %>%
  mutate(Vehicle_Age = ifelse(Vehicle_Age == "< 1 Year", 0,
                              ifelse(Vehicle_Age == "1-2 Year", 1, 2)))

# Create labels for KNN
vehicleInsuranceLabels <- vehicleInsurance %>%
  select(Response)

## Feature Engineering

# Outliers check
outlierMin<- quantile(vehicleInsurance$Annual_Premium, 0.25) -
  (IQR(vehicleInsurance$Annual_Premium) * 1.5)
outlierMax <- quantile(vehicleInsurance$Annual_Premium, 0.75) +
  (IQR(vehicleInsurance$Annual_Premium) * 1.5)

# Removing outliers
outliersAnnual_Premium <- vehicleInsurance %>%
  filter(Annual_Premium < outlierMin | Annual_Premium > outlierMax)
vehicleInsuranceWithoutOutliers <- vehicleInsurance %>%
  filter(Annual_Premium > outlierMin & Annual_Premium < outlierMax)

# Create displayAllHistograms using function
displayAllHistograms <- function(tibbleDataset){
  tibbleDataset %>% keep(is.numeric) %>% gather() %>% ggplot()+
    geom_histogram(mapping = aes(x=value,fill=key),
                   color = "black") + facet_wrap(~key,scales = "free") +
    theme_minimal()
}

# Display  plots
displayAllHistograms(vehicleInsurance)

# Dummy code for vehicle age
vehicleInsurance <- dummy_cols(vehicleInsurance,select_columns = "Gender")

# Dummy code for vehicle age
vehicleInsurance <- dummy_cols(vehicleInsurance,select_columns = "Vehicle_Age")

# Dummy code for age
vehicleInsurance <- dummy_cols(vehicleInsurance,select_columns = "Age")

# Dummy code fro Driving license
vehicleInsurance <- dummy_cols(
  vehicleInsurance,select_columns = "Driving_License")

# Dummy code fro Previously insured
vehicleInsurance <- dummy_cols(
  vehicleInsurance,select_columns = "Previously_Insured")

# Dummy code fro Vehicle Damage
vehicleInsurance <- dummy_cols(
  vehicleInsurance,select_columns = "Vehicle_Damage")

# Vintage to Years
vehicleInsurance <-  vehicleInsurance %>%
  mutate(Vintage = Vintage/365)

vehicleInsurance_a <- vehicleInsurance

# Normalise using min-max
vehicleInsurance <- vehicleInsurance %>%
  mutate(Annual_Premium = (Annual_Premium - min(Annual_Premium))/
           (max(Annual_Premium) - min(Annual_Premium)))

# Remove columns to reduce multicollinearity
vehicleInsurance <- vehicleInsurance %>%
  select(-Region_Code, -Vehicle_Age, -Policy_Sales_Channel, -Age)
vehicleInsurance <- vehicleInsurance %>%
  select(-Age_7, -Vehicle_Age_2)
vehicleInsurance <- vehicleInsurance %>%
  select(-Driving_License, -Driving_License_1)
vehicleInsurance <- vehicleInsurance %>%
  select(-Previously_Insured, -Previously_Insured_1)
vehicleInsurance <- vehicleInsurance %>%
  select(-Vehicle_Damage, -Vehicle_Damage_1)
vehicleInsurance <- vehicleInsurance %>%
  select(-Gender, -Gender_1, -Vehicle_Age_1, -Age_1)

## Display Corelation plot and create training & testing set

# Corelation plot
corrplot(cor(vehicleInsurance),
         method = "number",
         type="lower",
         number.cex = 0.5)

round(cor(vehicleInsurance%>%
            keep(is.numeric)),2)

# Create a seed, create sample set
set.seed(125)
sampleset <- sample(nrow(vehicleInsurance),
                    round(nrow(vehicleInsurance)*0.75),
                    replace = FALSE)

# Create a training data set for 75% of sample set
vehicleInsuranceTraining <- vehicleInsurance[sampleset,]
vehicleInsuranceLabelTraining <- vehicleInsuranceLabels[sampleset,]

# Create a test data set for 25% of sample set
vehicleInsuranceTesting <- vehicleInsurance[-sampleset,]
vehicleInsuranceLabelTesting <- vehicleInsuranceLabels[-sampleset,]

## Check for class imbalance and resolve it

# Check class imbalance in Cancelled Service
summary(vehicleInsuranceTraining$Response)

# Deal with class imbalance in training dataset
vehicleInsuranceTrainingSmoted <-
  tibble(SMOTE(X = data.frame(vehicleInsuranceTraining),
               target = vehicleInsuranceTraining$Response,
               dup_size = 0)$data)


# remove Class column from smoted set
vehicleInsuranceTrainingSmoted <- vehicleInsuranceTrainingSmoted %>%
  select(-class)

summary(vehicleInsuranceTrainingSmoted)

## Generate Regression models

# Neural network
# vehicleInsuranceNeuralNet <- neuralnet(
#   formula = Response ~ Annual_Premium+Vintage+Previously_Insured_0,
#   data = vehicleInsuranceTrainingSmoted,
#   hidden = 1,
#   act.fct = "logistic",
#   linear.output = FALSE)

# Logistic Regression
vehicleInsuranceGlm <- glm(data=vehicleInsuranceTrainingSmoted,
                           family = "binomial",
                           formula = Response~.)

# Naive-Bayes: smoothing and model generation
vehicleInsurance_bin <- vehicleInsuranceTrainingSmoted

vehicleInsurance_bin<- vehicleInsurance_bin %>% 
  mutate(Annual_Premium = round(Annual_Premium,digits = 2))

vehicleInsurance_bin<-drop_na(vehicleInsurance_bin)

# vehicleInsurance_bin  <- vehicleInsurance_bin %>%
#   mutate(Annual_Premium = ifelse(is.na(Annual_Premium),
#                                Annual_Premium[0,0.25],Annual_Premium))


vehicleInsurance_bin <- vehicleInsurance_bin %>% mutate(
  Annual_Premium = cut(Annual_Premium, breaks=c(-0.1, 0.25, 0.50, 0.75,1.00)))

# vehicleInsurance_bin <- vehicleInsurance_bin %>% mutate(
#   Annual_Premium = cut(Annual_Premium, breaks=c(2629, 24600, 32215, 40263,540165)))

vehicleInsurance_bin <- vehicleInsurance_bin %>% mutate(
  Vintage = cut(Vintage, breaks=c(0, 0.25, 0.50, 0.75,1.00)))

vehicleInsuranceNaiveBayes <- naiveBayes(
  formula = Response~.,
  data = vehicleInsurance_bin,
  laplace = 1)

# KNN
vehicleInsuranceKNN <- knn(train= vehicleInsuranceTraining,
                           test=vehicleInsuranceTesting,
                           cl= vehicleInsuranceLabelTraining$Response,
                           k=7)

# Decision tree
# Run the model with different complexity
vehicleInsuranceDecisionTreeModel <- rpart(formula = Response~.,
                                           method = "class",
                                           cp = 0.03,
                                           data = vehicleInsuranceTrainingSmoted)
# vehicleInsuranceDecisionTreeModel1 <- rpart(formula = Response~.,
#                                            method = "class",
#                                            cp = 0.02,
#                                            data = vehicleInsuranceTrainingSmoted)
# vehicleInsuranceDecisionTreeModel2 <- rpart(formula = Response~.,
#                                            method = "class",
#                                            cp = 0.03,
#                                            data = vehicleInsuranceTrainingSmoted)

# Model Summaries
summary(vehicleInsuranceGlm)
summary(vehicleInsuranceNaiveBayes)
summary(vehicleInsuranceDecisionTreeModel)
summary(vehicleInsuranceKNN)
# summary(vehicleInsuranceNeuralNet)

## Generate prediction for models

# GLM model prediction
vehicleInsuranceGlmPrediction <- predict(vehicleInsuranceGlm,
                                         vehicleInsuranceTesting,
                                         type = "response")
vehicleInsuranceGlmPrediction <- ifelse(
  vehicleInsuranceGlmPrediction >= 0.5,1,0)

# Naive-Bayes Prediction
vehicleInsuranceNaiveBayesPrediction <- predict(vehicleInsuranceNaiveBayes,
                                                vehicleInsuranceTesting,
                                                type = "class")

# Decision Tree prediction
vehicleInsuranceDecisionTreePrediction <- predict(
  vehicleInsuranceDecisionTreeModel,vehicleInsuranceTesting,type = "class")

# Probabilities for Neural Network
# vehicleInsuranceNeuralNetProbabilities <- compute(vehicleInsuranceNeuralNet,
#                                                   vehicleInsuranceTesting)
# vehicleInsuranceNeuralNetPrediction <-
#   ifelse(vehicleInsuranceNeuralNetProbabilities$net.result> 0.5,1,0)

# Prediction summary
summary(vehicleInsuranceGlmPrediction)
summary(vehicleInsuranceNaiveBayesPrediction)
summary(vehicleInsuranceDecisionTreePrediction)
summary(vehicleInsuranceKNN)
# print(vehicleInsuranceNeuralNetProbabilities$net.result)

## Generate Plots
# Display  plots
displayAllHistograms(vehicleInsurance)

rpart.plot(vehicleInsuranceDecisionTreeModel)

# plot(vehicleInsuranceNeuralNet)

ggplot(vehicleInsurance,aes(x= Gender_0 ,y= Response)) +
  geom_point(color="dark grey") +
  geom_smooth(method = lm,color="red",level=0)+
  ggtitle("Reponse and Gender-Male Line Graph")

## Generate confusion matrix & calculate predictive accuracy

# Create a confusion matrix
vehicleInsuranceGlmConfusionMatrix <- table(vehicleInsuranceTesting$Response,
                                            vehicleInsuranceGlmPrediction)

vehicleInsuranceNaiveBayesConfusionMatrix <- table(
  vehicleInsuranceTesting$Response,vehicleInsuranceNaiveBayesPrediction)

vehicleInsuranceDecisionTreeConfusionMatrix <- table(
  vehicleInsuranceTesting$Response,vehicleInsuranceDecisionTreePrediction)

vehicleInsuranceKNNConfusionMatrix <- table(
  vehicleInsuranceTesting$Response,vehicleInsuranceKNN)

# vehicleInsuranceNeuralNetConfusionMatrix <- table(
#   vehicleInsuranceTesting$Response,vehicleInsuranceNeuralNetPrediction)

# Calculate false positive
# Logistic
vehicleInsuranceGlmConfusionMatrix[1,2] /
  (vehicleInsuranceGlmConfusionMatrix[1,1] +
     vehicleInsuranceGlmConfusionMatrix[1,2])

# Naive Bayes
vehicleInsuranceNaiveBayesConfusionMatrix[1,2] /
  (vehicleInsuranceNaiveBayesConfusionMatrix[1,1] +
     vehicleInsuranceNaiveBayesConfusionMatrix[1,2])

# Decision Tree
vehicleInsuranceDecisionTreeConfusionMatrix[1,2] /
  (vehicleInsuranceDecisionTreeConfusionMatrix[1,1] +
     vehicleInsuranceDecisionTreeConfusionMatrix[1,2])

# KNN
vehicleInsuranceKNNConfusionMatrix[1,2] /
  (vehicleInsuranceKNNConfusionMatrix[1,1] +
     vehicleInsuranceKNNConfusionMatrix[1,2])

# Neural Network
# vehicleInsuranceNeuralNetConfusionMatrix[1,2] /
#   (vehicleInsuranceNeuralNetConfusionMatrix[1,1] +
#      vehicleInsuranceNeuralNetConfusionMatrix[1,2])


## Calculate false negative
# Logistic Regression
vehicleInsuranceGlmConfusionMatrix[2,1] /
  (vehicleInsuranceGlmConfusionMatrix[2,1] +
     vehicleInsuranceGlmConfusionMatrix[2,2])

# Naive Bayes
vehicleInsuranceNaiveBayesConfusionMatrix[2,1] /
  (vehicleInsuranceNaiveBayesConfusionMatrix[2,1] +
     vehicleInsuranceNaiveBayesConfusionMatrix[2,2])

# Decision Tree
vehicleInsuranceDecisionTreeConfusionMatrix[2,1] /
  (vehicleInsuranceDecisionTreeConfusionMatrix[2,1] +
     vehicleInsuranceDecisionTreeConfusionMatrix[2,2])

# KNN
vehicleInsuranceKNNConfusionMatrix[2,1] /
  (vehicleInsuranceKNNConfusionMatrix[2,1] +
     vehicleInsuranceKNNConfusionMatrix[2,2])

# Neural Network
# vehicleInsuranceNeuralNetConfusionMatrix[2,1] /
#   (vehicleInsuranceNeuralNetConfusionMatrix[2,1] +
#      vehicleInsuranceNeuralNetConfusionMatrix[2,2])


# Display the confusion matrix
print(vehicleInsuranceGlmConfusionMatrix)
print(vehicleInsuranceNaiveBayesConfusionMatrix)
print(vehicleInsuranceDecisionTreeConfusionMatrix)
print(vehicleInsuranceKNNConfusionMatrix)
# print(vehicleInsuranceNeuralNetConfusionMatrix)

# Predictive accuracy
vehicleInsuranceGlmPredictiveAccuracy <- sum(
  diag(vehicleInsuranceGlmConfusionMatrix))/nrow(vehicleInsuranceTesting)

vehicleInsuranceNaiveBayesPredictiveAccuracy <- sum(diag(
  vehicleInsuranceNaiveBayesConfusionMatrix))/
  nrow(vehicleInsuranceTesting)

vehicleInsuranceDecisionTreePredictiveAccuracy <- sum(diag(
  vehicleInsuranceDecisionTreeConfusionMatrix))/
  nrow(vehicleInsuranceTesting)

vehicleInsuranceKNNPredictiveAccuracy <- sum(diag(
  vehicleInsuranceKNNConfusionMatrix))/
  nrow(vehicleInsuranceTesting)

# vehicleInsuranceNeuralNetPredictiveAccuracy <- sum(diag(
#   vehicleInsuranceNeuralNetConfusionMatrix))/
#   nrow(vehicleInsuranceTesting)

# Display predictive accuracy
print(vehicleInsuranceGlmPredictiveAccuracy)
print(vehicleInsuranceNaiveBayesPredictiveAccuracy)
print(vehicleInsuranceDecisionTreePredictiveAccuracy)
print(vehicleInsuranceKNNPredictiveAccuracy)
# print(vehicleInsuranceNeuralNetPredictiveAccuracy)

## Find optimal K Value
# Create a matrix of k-values with their predictive accuracy
kValueMatrix<- matrix(data= NA,
                      nrow=0,
                      ncol=2)

# Assign column names to the matrix
colnames(kValueMatrix)<- c("kvalue", "Predictive Accuracy")

# Loop through odd values of k from 1 up to the number of records
# in the training dataset
for(kValue in 1:100){
  # Only calculate predictive accuracy if the k value is odd
  if(kValue %% 2 != 0){
    # Generate the model
    vehicleInsuranceKNN <- knn(train= vehicleInsuranceTraining,
                               test=vehicleInsuranceTesting,
                               cl= vehicleInsuranceLabelTraining$Response,
                               k=kValue)
    # Generate the confusion matrix
    vehicleInsuranceKNNConfusionMatrix<- table(
      vehicleInsuranceLabelTesting$Response,vehicleInsuranceKNN)
    # Calculate predictive accuracy
    vehicleInsuranceKNNPredictiveAccuracy <- sum(
      diag(vehicleInsuranceKNNConfusionMatrix))/ nrow(vehicleInsuranceTesting)

    # Add a new row to the kValueMatrix
    kValueMatrix<- rbind(kValueMatrix, c(kValue,
                                         vehicleInsuranceKNNPredictiveAccuracy))
  }
}
# Display k value matrix
print(kValueMatrix)