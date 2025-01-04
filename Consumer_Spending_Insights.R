
install.packages("caret")
install.packages("data.table")
install.packages("factoextra")


# Load necessary libraries
library(data.table)  # Efficient for handling large datasets
library(dplyr)       # Data manipulation
library(ggplot2)     # Data visualization
#1. Data Aquisition

library(caret)       # Modeling and pre-processing
library(factoextra)  # PCA and clustering visualizations
library(corrplot)    # Correlation matrix visualization

setwd("C:/Users/asmit/OneDrive/Desktop/Asmi/academics/big-data")  
data <- fread("C:/Users/asmit/OneDrive/Desktop/Asmi/academics/big-data/Operating _expenditures.csv")  # Use your dataset file here

# structure and summary of the dataset
str(data)
summary(data)

#2. Data Preparation:
#Data cleaning: handle missing values
data_clean <- data[!is.na(TRANSACTION_AMT)]

# Fill missing TRANSACTION_AMT with median using data.table (faster than dplyr)
median_amt <- median(data_clean$TRANSACTION_AMT, na.rm = TRUE)
data_clean[, TRANSACTION_AMT := ifelse(is.na(TRANSACTION_AMT), median_amt, TRANSACTION_AMT)]

# Convert Data Type
data_clean[, TRANSACTION_DT := as.IDate(TRANSACTION_DT, format = "%m/%d/%Y")]

# Remove duplicates
data_clean <- distinct(data_clean)

#3.Exploratory Data Analysis:
# Summary statistics
summary(data_clean)
data_sample <- data_clean[1:10000]

# histograms to check the distribution of a numerical variable
ggplot(data_sample, aes(x = TRANSACTION_AMT)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of TRANSACTION AMT", x = "TRANSACTION AMT", y = "Frequency")

#expenditure by Category
ggplot(data_clean, aes(x = reorder(CATEGORY_DESC, -TRANSACTION_AMT), y = TRANSACTION_AMT)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Expenditures by Category", x = "Category", y = "Total Amount Spent")

# Scatter plot to visualize the relationship between State and amount spent
ggplot(data_clean, aes(x = STATE, y = TRANSACTION_AMT)) +
  geom_boxplot() +
  labs(title = "Expenditures by State", x = "State", y = "Transaction Amount")

#4 Statical Analysis
#4.1 Sensitivity Analysis
# Sensitivity analysis: Vary the transaction amount by ±10%, ±20%
sensitivity_factors <- c(0.9, 1.0, 1.1)
sensitivity_results <- data.table(factor = numeric(), total_expenditure = numeric())

for (factor in sensitivity_factors) {
  data_clean[, adj_transaction_amt := TRANSACTION_AMT * factor]
  total_expenditure <- sum(data_clean$adj_transaction_amt)
  sensitivity_results <- rbind(sensitivity_results, data.table(factor, total_expenditure))
}

# Plot sensitivity analysis results
ggplot(sensitivity_results, aes(x = factor, y = total_expenditure)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Sensitivity Analysis: Impact of Transaction Amount Changes",
       x = "Adjustment Factor",
       y = "Total Expenditure")

# 4.2 Linear regression to predict transaction amount
# Fit a linear regression model on a subset of the data for faster computation
model_data <- data_clean[1:min(500, .N)]  
model <- lm(TRANSACTION_AMT ~ TRANSACTION_DT + STATE + NAME, data = model_data)

# View the summary of the model
summary(model)

# Model diagnostics (reduced to important plots for quicker assessment)
par(mfrow = c(2, 2))  # 2x2 layout for model diagnostic plots
plot(model)
