# Learning to use corrplot and olsrr library packages.
# Practicing Histogram Plots, correlation matrix, , correlation plot,
# making Linear Regression model and testing for Multicollinearity

# Install and load tidyverse, corrplot, and olsrr packages
# Comment the install commands once you have installed the packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")

# Load tidyverse, corrplot and olsrr packages
library("tidyverse")
library("corrplot")
library("olsrr")

# Set working directory to location where ZooVisitSpending.csv file is stored
setwd("C:\\Users\\ual-laptop\\Desktop\\Lab05")

# Read "ZooVisitSpending.csv" file, set datatypes, store it, and display it
zooSpending <- read_csv("ZooVisitSpending.csv", 
                                 col_types = "niil",
                                 col_names = TRUE)
print(zooSpending)

# Displaying the structure of zooSpending
print(str(zooSpending))

# Displaying summary of zooSpending
print(summary(zooSpending))

# Creating displayAllHistograms() function
displayAllHistograms <- function(data) {
  zooSpending %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") + 
      facet_wrap(~ key, scales = "free") + 
      theme_minimal ()
}

# Call displayAllHistograms() function, passing the zooSpending as an argument
displayAllHistograms(zooSpending)

# Display a correlation matrix of zooSpending rounded to two decimal places
round(cor(zooSpending), 2)

# Display a correlation plot using the "number" method and limit output to the bottom left
corrplot(cor(zooSpending),
         method = "number",
         type = "lower")

# Generate the linear regression model and save it in an object called zooSpendingModel
zooSpendingModel <- lm(data = zooSpending,
                       formula = VisitSpending ~ .)

# Display the beta coefficients for the model on the console
print(zooSpendingModel)

ggplot()

# Display the linear regression model results using the summary() function
print(summary(zooSpendingModel))

# Test for multicollinearity using the ols_vif_tol() function
print(ols_vif_tol(zooSpendingModel))