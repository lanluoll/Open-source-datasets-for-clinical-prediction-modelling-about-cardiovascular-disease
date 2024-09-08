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
cardio_train_data <- read.csv("/Users/luolan/Desktop/Datasets/cardio_train.csv", sep=";", header=TRUE)

# Check the dimensions and structure of the dataset
dim(cardio_train_data)
str(cardio_train_data)
sum(duplicated(cardio_train_data))

# Check for missing values
sum(is.na(cardio_train_data))

# Remove 'id' column (no practical meaning)
cardio_train_data <- cardio_train_data %>% dplyr::select(-id)

# Count the number of events (people with heart disease)
num_events <- sum(cardio_train_data$cardio == 1)
# Count the number of predictor variables (total variables minus the event column)
num_predictors <- ncol(cardio_train_data) - 2 
# Calculate EPV
epv <- num_events / num_predictors
print(paste("EPV:", epv))

# Summary of the dataset
summary(cardio_train_data)

# Convert age from days to years
cardio_train_data$age <- cardio_train_data$age / 365.25

# Count the number of abnormal ap_hi values
ap_hi_abnormal <- sum(cardio_train_data$ap_hi < 90 | cardio_train_data$ap_hi > 250)

# Count the number of abnormal ap_lo values
ap_lo_abnormal <- sum(cardio_train_data$ap_lo < 60 | cardio_train_data$ap_lo > 140)

# Print the number of abnormal values
cat("Number of abnormal ap_hi values (outside 90-250):", ap_hi_abnormal, "\n")
cat("Number of abnormal ap_lo values (outside 60-140):", ap_lo_abnormal, "\n")

# Remove rows where ap_hi and ap_lo values are outside the medically reasonable range
filtered_cardio_data <- cardio_train_data %>%
  filter(ap_hi >= 90 & ap_hi <= 250, ap_lo >= 60 & ap_lo <= 140)

# Check the size of the cleaned dataset
dim(filtered_cardio_data)

# View the summary of the cleaned dataset
summary(filtered_cardio_data)

# Continuous variables for analysis
continuous_vars <- c('age', 'ap_hi', 'ap_lo', 'height', 'weight')

# Create histograms for continuous variables
plots <- lapply(continuous_vars, function(var) {
  ggplot(filtered_cardio_data, aes(x = !!sym(var))) +
    geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = var, x = "Value", y = "Frequency")
})

# Arrange all histograms in a grid
do.call(grid.arrange, c(plots, ncol = 2))

# Univariate analysis
num_vars <- c('age', 'height', 'weight', 'ap_hi', 'ap_lo')
categorical_vars <- c('gender', 'cholesterol', 'gluc', 'smoke', 'alco', 'active', 'cardio')

num_data <- cardio_train_data[, num_vars]
categorical_data <- cardio_train_data[, categorical_vars]

# Distribution of continuous variables
data_long <- cardio_train_data %>%
  dplyr::select(dplyr::all_of(num_vars)) %>%
  pivot_longer(cols=everything(), names_to="Variable", values_to="Value")

ggplot(data_long, aes(x=Value)) +
  geom_histogram(fill="steelblue", color="black", bins=30) +
  facet_wrap(~Variable, scales="free_x", ncol=2) +
  theme_minimal() +
  labs(title="Distribution of continuous variables", x="Value", y="Frequency") +
  theme(plot.title=element_text(hjust=0.5))

# Distribution of categorical variables
data_long1 <- cardio_train_data %>%
  dplyr::select(dplyr::all_of(categorical_vars)) %>%
  pivot_longer(cols=everything(), names_to="Variable", values_to="Value")

ggplot(data_long1, aes(x=factor(Value))) +
  geom_bar(fill="coral", color="black") +
  facet_wrap(~Variable, scales="free_x", ncol=2) +
  theme_minimal() +
  labs(title="Distribution of categorical variables", x="Category", y="Count") +
  theme(plot.title=element_text(hjust=0.5))

# Correlation analysis
correlation_matrix <- cor(cardio_train_data[sapply(cardio_train_data, is.numeric)], use="complete.obs")
corrplot(correlation_matrix, method="color", tl.col="black", tl.srt=45)
