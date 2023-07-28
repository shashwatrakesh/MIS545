# Shashwat Rakesh
# MIS 545-02
# Lab03Group44ShashwatRakesh.R
# Learning and practicing the basics of Extract Transform Load (ETL) pipeline 
# using a groceryTransactions dataset

# Install and load tidyverse package
# Comment the install command written below once you have installed the package
# install.packages("tidyverse")
library(tidyverse)

# Set working directory to location where groceryTransactions.csv file is stored
setwd("C:\\Users\\ual-laptop\\Downloads\\Lab03")

# Read "GroceryTransactions.csv" file, set datatypes, store it, and display it
groceryTransactions1 <- read_csv("GroceryTransactions.csv", 
                                 col_types = 'iDffffifffffffin',
                                 col_names = TRUE)
print(groceryTransactions1)

# Display the first 20 rows of groceryTransactions1
head(groceryTransactions1, 20)

# Display the structure of groceryTransactions1
str(groceryTransactions1)

# Display the summary of groceryTransactions1
summary(groceryTransactions1)

# Display the mean of Revenue, mean of UnitsSold, standard deviation of Revenue,
# Inter-Quartile Range of UnitsSold, minimum of Revenue and maximum of Children
summarize(.data = groceryTransactions1, meanRevenue = mean(Revenue), 
                medianUnitsSold = median(UnitsSold), 
                standardDeviationRevenue = sd(Revenue),
                iqrUnitsSold = IQR(UnitsSold),
                minRevenue = min(Revenue),
                maxChildren = max(Children))

# Subsetting of groceryTransactions1 tibble
groceryTransactions2 <- groceryTransactions1[c("PurchaseDate", "Homeowner", 
                                               "Children", "AnnualIncome", 
                                               "UnitsSold", "Revenue")]

# Filtering groceryTransactions2 according to no Homeowner and more than equal
# to 4 Children
subset(groceryTransactions2, Homeowner == 'N' & Children >= 4)

# Filtering groceryTransactions2 by Annual Income of "$150K +" or more than 6
# UnitsSold
subset(groceryTransactions2, AnnualIncome == '$150K +' | UnitsSold > 6 )

# Group by the AnnualIncome, find mean of Revenue, store it into a new column
# and display all the results in descending order
print(groceryTransactions2 %>%
        group_by(AnnualIncome) %>%
        summarize(AverageTransactionRevenue = mean(Revenue)) %>%
        arrange(desc(AverageTransactionRevenue)), 
        n = Inf)

# Adding a new column named AveragePricePerUnit calculated as Revenue/UnitsSold
# and then display it
groceryTransactions3 <- groceryTransactions2 %>% 
  add_column(AveragePricePerUnit = 
               groceryTransactions2$Revenue / groceryTransactions2$UnitsSold)
groceryTransactions3

# Plot Histogram for AveragePricePerUnit
histogramAveragePricePerUnit <- ggplot(data = groceryTransactions3, 
                                       aes(x=AveragePricePerUnit)) 
histogramAveragePricePerUnit + geom_histogram(binwidth = 1, color = 'black', 
                                              fill = 'orange', alpha = 0.5) + 
  ggtitle("Average Price Per Unit Histogram")

# Plot a boxplot for Revenue
boxplotRevenue <- ggplot(data = groceryTransactions3, aes(x = Revenue))
boxplotRevenue + geom_boxplot(color = '#0C234B', fill = '#AB0520') + 
  ggtitle("Revenue Boxplot")