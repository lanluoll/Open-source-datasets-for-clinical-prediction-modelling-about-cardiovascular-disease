# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(MASS)
library(stats)
library(caret)
library(pROC)


# Load the dataset
heart_data <- read.csv("/Users/luolan/Desktop/Datasets/heart.csv")

# Count the number of events (people with heart disease)
num_events <- sum(heart_data$target == 1)
# Count the number of predictor variables (total variables minus the event column)
num_predictors <- ncol(heart_data) - 1  # Assuming 1 column is the event column
# Calculate EPV
epv <- num_events / num_predictors
print(paste("EPV:", epv))

# Check the dimensions and structure of the dataset
dim(heart_data)
str(heart_data)
sum(duplicated(heart_data))
duplicated_rows <- heart_data[duplicated(heart_data), ]
print(duplicated_rows)
heart_data <- heart_data[!duplicated(heart_data), ]
sum(duplicated(heart_data))

# Check for missing values
sum(is.na(heart_data))

# Summary of the dataset
summary(heart_data)

# Univariate analysis
num_vars <- c('age', 'trestbps', 'chol', 'thalach', 'oldpeak')
categorical_vars <- c('sex', 'cp', 'fbs', 'restecg', 'exang', 'slope', 'ca', 'thal', 'target')
num_data <- heart_data[, num_vars]
categorical_data <- heart_data[, categorical_vars]

# Distribution of continuous features
data_long <- heart_data %>%
  dplyr::select(dplyr::all_of(num_vars)) %>%
  pivot_longer(cols=everything(), names_to="Variable", values_to="Value")

ggplot(data_long, aes(x=Value)) +
  geom_histogram(fill="steelblue", color="black", bins=30) +
  facet_wrap(~Variable, scales="free_x", ncol=2) +
  theme_minimal() +
  labs(title="Distribution of continuous variables", x="Value", y="Frequency") +
  theme(plot.title=element_text(hjust=0.5))

# Distribution of categorical features
data_long1 <- heart_data %>%
  dplyr::select(dplyr::all_of(categorical_vars)) %>%
  pivot_longer(cols=everything(), names_to="Variable", values_to="Value")

ggplot(data_long1, aes(x=factor(Value))) +
  geom_bar(fill="coral", color="black") +
  facet_wrap(~Variable, scales="free_x", ncol=2) +
  theme_minimal() +
  labs(title="Distribution of categorical variables", x="Category", y="Count") +
  theme(plot.title=element_text(hjust=0.5))

# Calculate standard deviation for numerical columns
std_calculate <- sapply(heart_data, function(x) {
  if (is.numeric(x)) {
    sd(x, na.rm=TRUE)
  } else {
    NA
  }
})
std_calculate

# Correlation analysis
correlation_matrix <- cor(heart_data[sapply(heart_data, is.numeric)], use="complete.obs")
corrplot(correlation_matrix, method="color", tl.col="black", tl.srt=45)

# Extract and display high correlations
correlation_long <- as.data.frame(as.table(correlation_matrix))
high_correlations <- correlation_long %>% filter(abs(Freq) > 0.7 & abs(Freq) < 1)
high_correlations




