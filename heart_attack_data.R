#导入包
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(caret)
#导入数据
heart_attack_data <- read.csv("/Users/luolan/Desktop/Datasets/heart_attack_prediction_dataset.csv")

View(heart_attack_data)


##查看数据结构
dim(heart_attack_data)  #(8763,26)
str(heart_attack_data)
sum(duplicated(heart_attack_data))  #0

# Count the number of events (people with heart disease)
num_events <- sum(heart_attack_data$Heart.Attack.Risk == 1)
# Count the number of predictor variables (total variables minus the event column)
num_predictors <- ncol(heart_attack_data) - 1  # Assuming 1 column is the event column
# Calculate EPV
epv <- num_events / num_predictors
print(paste("EPV:", epv))

# 查看描述性统计
summary(heart_attack_data)

#检查是否存在缺失值
sum(is.na(heart_attack_data)) #该份数据集无缺失值

#单变量分析
numeric_vars <- heart_attack_data %>% select_if(is.numeric)
numeric_vars %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(Value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~Variable, scales = "free", nrow = 5, ncol = 4) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12), 
    strip.text = element_text(size = 14)  
  )

categorical_vars <- c('Sex','Diet','Country','Continent','Hemisphere','Heart.Attack.Risk')
categorical_data <- heart_attack_data[,categorical_vars]
categorical_data %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(Value)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~Variable, scales = "free", nrow = 3, ncol = 2) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 60),  
    axis.text.y = element_text(size = 12), 
    strip.text = element_text(size = 14)   
  )


#类别型变量与目标变量（Heart Attack Risk）之间的关系
ggplot(heart_attack_data, aes(x = Sex, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Heart Attack Risk") +
  theme_minimal()

ggplot(heart_attack_data, aes(x = Diet, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Heart Attack Risk") +
  theme_minimal()

ggplot(heart_attack_data, aes(x = Country, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Heart Attack Risk") +
  theme_minimal()

ggplot(heart_attack_data, aes(x = Continent, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Heart Attack Risk") +
  theme_minimal()

ggplot(heart_attack_data, aes(x = Hemisphere, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Heart Attack Risk") +
  theme_minimal()

# 绘制Heart Attack Risk和AGE的关系
age_summary <- heart_attack_data %>%
  group_by(Heart.Attack.Risk) %>%
  summarise(mean_age = mean(Age, na.rm = TRUE))
# 绘制 Age 的条形图
ggplot(age_summary, aes(x = as.factor(Heart.Attack.Risk), y = mean_age, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = round(mean_age, 1)), vjust = -0.5, color = "black", size = 3.5) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Heart Attack Risk", y = "Mean Age", fill = "Heart Attack Risk") +
  theme_minimal()

# Heart Attack Risk和Cholesterol的关系
cholesterol_summary <- heart_attack_data %>%
  group_by(Heart.Attack.Risk) %>%
  summarise(mean_cholesterol = mean(Cholesterol, na.rm = TRUE))
# 绘制 Cholesterol 的条形图
ggplot(cholesterol_summary, aes(x = as.factor(Heart.Attack.Risk), y = mean_cholesterol, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = round(mean_cholesterol, 1)), vjust = -0.5, color = "black", size = 3.5) + scale_fill_manual(values = c("#2ca02c", "#d62728")) + 
  labs(x = "Heart Attack Risk", y = "Mean Cholesterol", fill = "Heart Attack Risk") +
  theme_minimal()

# Heart Attack Risk和 Heart Rate 
heart_rate_summary <- heart_attack_data %>%
  group_by(Heart.Attack.Risk) %>%
  summarise(mean_heart_rate = mean(Heart.Rate, na.rm = TRUE))
# 绘制 Heart Rate 的条形图
ggplot(heart_rate_summary, aes(x = as.factor(Heart.Attack.Risk), y = mean_heart_rate, fill = as.factor(Heart.Attack.Risk))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = c("#9467bd", "#8c564b")) + 
  geom_text(aes(label = round(mean_heart_rate, 1)), vjust = -0.5, color = "black", size = 3.5) + labs(x = "Heart Attack Risk", y = "Mean Heart Rate", fill = "Heart Attack Risk") +
  theme_minimal()

# 将字符型变量转换为因子型变量
heart_attack_data <- heart_attack_data %>% dplyr::select(-Patient.ID)
heart_attack_data <- heart_attack_data %>%
  mutate(across(where(is.character), as.factor))

# 将因子型变量转换为数值型变量(编码)
heart_attack_data <- heart_attack_data %>%
  mutate(across(where(is.factor), as.numeric))
str(heart_attack_data)

# Load necessary library
library(car)

# Fit a linear model using all variables
# Assuming 'Heart.Attack.Risk' is your target variable
model <- lm(Heart.Attack.Risk ~ ., data = heart_attack_data)

# Calculate VIF for each predictor variable
vif_values <- vif(model)

# Print the VIF values
print(vif_values)

# Optionally, you can identify variables with high VIF
high_vif <- vif_values[vif_values > 5]
print(high_vif)


