# Import libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(mice)

# Load the data
uci_data <- read.csv("/Users/luolan/Desktop/Datasets/heart_disease_uci.csv")

# View the structure of the data
View(uci_data)
dim(uci_data)  # (920,16)
str(uci_data)  # Fields have various types
sum(duplicated(uci_data))  # Shows 0, meaning no duplicates

# Check for missing values
sum(is.na(uci_data))  # 962 missing values
miss_value <- sapply(uci_data, function(x) sum(is.na(x)) / 
                       length(x)) * 100  # Calculate the percentage of missing values per field
miss_percentage <- data.frame(Variable = names(miss_value),
                              Missing_Percentage = miss_value)
miss_percentage1 <- miss_percentage %>% arrange(desc(Missing_Percentage))  # Sort variables by missing percentage in descending order
print(miss_percentage1)  # The 'ca' field has a missing value percentage of 66.4%, indicating severe missing data

# Remove the 'id' (not meaningful) and 'dataset' columns
uci_data1 <- uci_data %>% select(-dataset)
uci_data1 <- uci_data1 %>% select(-id)
str(uci_data1)

# Handling missing values
## For categorical variables, use mode imputation
mode_impute <- ifelse(mean(uci_data1$exang, na.rm = TRUE) 
                      > 0.5, TRUE, FALSE)
# Impute missing values with the mode
uci_data1$exang[is.na(uci_data1$exang)] <- mode_impute

## The 'ca' field is special with a high missing rate, so multiple imputation is used
imputed_data <- mice(uci_data1, m = 5, method = 'pmm', seed = 123)
uci_data1 <- complete(imputed_data, 1)

## For continuous variables, use median imputation
numeric_columns <- c("fbs", "oldpeak", "trestbps", 
                     "thalch", "chol")
for (col in numeric_columns) {
  uci_data1[[col]][is.na(uci_data1[[col]])] <- median(uci_data1[[col]], na.rm = TRUE)
}
colSums(is.na(uci_data1))

# Summary statistics
summary(uci_data1)

# Univariate analysis
num_vars <- c('age','trestbps','chol','thalch','oldpeak')
categorical_vars <- c('sex','cp','fbs','exang','slope','thal','num','ca')
num_data <- uci_data1[,num_vars]
categorical_data <- uci_data1[, categorical_vars]

# Use lapply to generate histograms for each numeric variable
# Reshape the dataset to long format
data_long <- uci_data1 %>%
  select(all_of(num_vars)) %>%
  pivot_longer(cols = everything(), names_to = 
                 "Variable", values_to = "Value")

# Plot histograms using facet_wrap
ggplot(data_long, aes(x = Value)) +
  geom_histogram(fill="steelblue", color='black', 
                 bins=30) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 2) +
  theme_minimal() +
  labs(title = "Distributions of Continuous Variables", 
       x = "Value", 
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

## Categorical variables
# Loop through each field, calculate frequency, and plot bar charts
par(mfrow=c(2,4))
for (var in categorical_vars) {
  # Calculate frequency
  freq_table <- table(uci_data1[[var]])
  barplot(freq_table, 
          main = paste("Frequency of", var), 
          xlab = var, 
          ylab = "Frequency", 
          col = "coral",
          ylim = c(0, max(freq_table) + 5))  # Set Y-axis range
}
if (length(categorical_vars) < 8) {
  for (i in 1:(8 - length(categorical_vars))) {
    plot.new()  # Add blank plot
  }
}

## Encoding categorical variables
code_vars <- c('sex','cp','fbs','restecg','exang','slope','thal')
one_hot_encoded <- model.matrix(~.-1, data = uci_data1[code_vars])
one_hot_df <- as.data.frame(one_hot_encoded)
other_vars <- c('age','trestbps','chol','thalch','oldpeak','ca','num')
## Combine the encoded variables with the remaining original variables
other_uci_data <- uci_data1[other_vars]
uci_new_data <- cbind(other_uci_data, one_hot_df)
dim(uci_new_data)
