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
heart_disease <- read.csv("/Users/luolan/Desktop/Datasets/heart_disease.csv.xls")

# Check the dimensions and structure of the dataset
dim(heart_disease)
str(heart_disease)
sum(duplicated(heart_disease))

# Check for missing values
sum(is.na(heart_disease))
miss_value <- sapply(heart_disease, function(x) sum(is.na(x)) /
                       length(x)) * 100  #计算各个字段缺失值比例
miss_percentage <- data.frame(Variable = names(miss_value),
                              Missing_Percentage = miss_value)
miss_percentage1 <- miss_percentage %>% arrange(desc(Missing_Percentage)) #按缺失比例从高到低的顺序排列字段
print(miss_percentage1)
