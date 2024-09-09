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

fmh_data <- read.csv("/Users/luolan/Desktop/Datasets/framingham.csv")

dim(fmh_data)
str(fmh_data)
sum(duplicated(fmh_data))

# Count the number of events (people with heart disease)
num_events <- sum(fmh_data$TenYearCHD == 1)
# Count the number of predictor variables (total variables minus the event column)
num_predictors <- ncol(fmh_data) - 1  # Assuming 1 column is the event column
# Calculate EPV
epv <- num_events / num_predictors
print(paste("EPV:", epv))

# missing values
sum(is.na(fmh_data))
miss_value <- sapply(fmh_data, function(x) sum(is.na(x))/length(x)) * 100
miss_percentage <- data.frame(Variable=names(miss_value), Missing_Percentage=miss_value)
missing_percentage1 <- miss_percentage %>% arrange(desc(Missing_Percentage))
print(missing_percentage1)
# impute mode for categorical variables
mode_impute <- function(x){
  x[is.na(x)] <- as.numeric(names(sort(table(x), decreasing=TRUE)))[1]
  return(x)
}
categorical_columns <- c('education', 'BPMeds')
fmh_data[categorical_columns] <- lapply(fmh_data[categorical_columns], mode_impute)
#impute median for continuous variables
numeric_columns <- c('glucose', 'totChol', 'cigsPerDay', 'BMI', 'heartRate')
for (col in numeric_columns) {
  fmh_data[[col]][is.na(fmh_data[[col]])] <- median(fmh_data[[col]], na.rm=TRUE)
}
colSums(is.na(fmh_data))

summary(fmh_data)

# univariate analysis
num_vars <- c('age', 'cigsPerDay', 'totChol', 'sysBP', 'diaBP', 'BMI', 'heartRate', 'glucose')
categorical_vars <- c('male', 'education', 'currentSmoker', 'BPMeds', 'prevalentStroke', 'prevalentHyp', 'diabetes', 'TenYearCHD')
num_data <- fmh_data[, num_vars]
categorical_data <- fmh_data[, categorical_vars]

# continuous variables
data_long <- fmh_data %>%
  dplyr::select(dplyr::all_of(num_vars)) %>%
  pivot_longer(cols=everything(), names_to="Variable", values_to="Value")

ggplot(data_long, aes(x=Value)) +
  geom_histogram(fill="steelblue", color="black", bins=30) +
  facet_wrap(~Variable, scales="free_x", ncol=2) +
  theme_minimal() +
  labs(title="Distribution of continuous variables ", x="Value", y="Frequency") +
  theme(plot.title=element_text(hjust=0.5))

# categorical variables
data_long1 <- fmh_data %>%
  dplyr::select(dplyr::all_of(categorical_vars)) %>%
  pivot_longer(cols=everything(), names_to="Variable", values_to="Value")

ggplot(data_long1, aes(x=factor(Value))) +
  geom_bar(fill="coral", color="black") +
  facet_wrap(~Variable, scales="free_x", ncol=2) +
  theme_minimal() +
  labs(title="Distribution of categorical variables", x="Category", y="Count") +
  theme(plot.title=element_text(hjust=0.5))

std_calculate <- sapply(fmh_data, function(x) {
  if (is.numeric(x)) {
    sd(x, na.rm=TRUE)
  } else {
    NA
  }
})
std_calculate
table(fmh_data$prevalentStroke)

# Correlation analysis
par(mfrow=c(1,1))
correlation_matrix <- cor(fmh_data, use="complete.obs")
corrplot(correlation_matrix, method="color", tl.col="black", tl.srt=45)
correlation_long <- as.data.frame(as.table(correlation_matrix))
high_correlations <- correlation_long %>% filter(abs(Freq) > 0.7 & abs(Freq) < 1)
high_correlations

#Consider removing ‘prevalentStroke’, ‘diabetes’, 'cigsPerDay', 'sysBP'

# Fit the initial GLM model
fit.full <- glm(TenYearCHD ~ age + totChol + diaBP + BMI + heartRate + glucose + 
                   male + education + currentSmoker + BPMeds + prevalentHyp, 
                 data = fmh_data, family = binomial)

# 使用step函数进行后向选择
backward_model <- step(fit.full, direction = "backward")
summary(backward_model)

# Fit the null model (only intercept)
null_model <- glm(TenYearCHD ~ 1, data = fmh_data, family = binomial)

# Perform the Likelihood Ratio Test
lrt_result <- anova(null_model, backward_model, test = "LRT")

# Print the LRT results
print(lrt_result)

# Predict probabilities and classes using the stepwise-selected model
pred_prob <- predict(backward_model, fmh_data, type = "response")
pred_class <- ifelse(pred_prob > 0.2, 1, 0)

# Create a confusion matrix
conf_matrix <- confusionMatrix(as.factor(pred_class), as.factor(fmh_data$TenYearCHD))

# Print the confusion matrix
print(conf_matrix)

# Calculate precision, recall, and F1-score
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")

# Calculate and plot the ROC curve
roc_curve <- roc(fmh_data$TenYearCHD, pred_prob)

# Calculate the AUC
auc_value <- auc(roc_curve)
cat("AUC-ROC: ", auc_value, "\n")

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")

