#导入包
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(mice)
library(scales) # 100，000

#导入数据
heart_2020_cleaned <- read.csv("/Users/luolan/Desktop/Datasets/heart_2020_cleaned.csv")

#字段含义解释
##HeartDisease（心脏病）：是否患有心脏病（Yes表示有，No表示没有）
##BMI（体质指数）：表示个体的体质指数。
##Smoking（吸烟）：表示个体吸烟状况（Yes表示是，No表示否）。
##AlcoholDrinking（饮酒）：表示个体饮酒状况（Yes表示是，No表示否）。
##Stroke（中风）：表示个体中风状况（Yes表示是，No表示否）。
##PhysicalHealth（身体健康）：表示个体身体状况。
##MentalHealth（心理健康）：表示个体心理健康状况。
##DiffWalking（行走困难）：表示个体是否行走有困难（Yes表示是，No表示否）。
##Sex（性别）：表示个体的性别（Male表示男性，Female表示女性）。
##AgeCategory（年龄类别）：表示个体所属的年龄段（例如55-59、80 or older）。
##Race（种族）：表示个体的种族（例如White表示白人，Black表示黑人，Asian表示亚洲人）。
##Diabetic（糖尿病）：表示个体患有糖尿病状况（Yes表示是，No表示否）。
##PhysicalActivity（体育活动）：表示个体参加过体育活动或锻炼的情况（Yes表示有，No表示没有）。
##GenHealth（总体健康状况）：表示个体总体健康状况。可能的值包括Excellent（非常好）、Very good（很好）、Good（好）、Fair（一般）、Poor（差）。
##SleepTime（睡眠时间）：表示个睡眠时间。
##Asthma（哮喘）：表示个体是否有哮喘（Yes表示有，No表示没有）。
##KidneyDisease（肾病）：表示个体是否患有肾病（Yes表示有，No表示没有）。
##SkinCancer（皮肤癌）：表示个体是否有被诊断为皮肤癌（Yes表示有，No表示没有）。


#查看数据结构
View(heart_2020_cleaned)
dim(heart_2020_cleaned)  #(319795,18)
str(heart_2020_cleaned) #字段有多种类型
sum(duplicated(heart_2020_cleaned)) #显示为18078，表示该数据集中存在重复值
##删除重复值
heart_2020_cleaned <- heart_2020_cleaned %>%
  distinct()
##再次查看数据集大小
dim(heart_2020_cleaned)  #(301717,18)

# Count the number of events (people with heart disease)
num_events <- sum(heart_2020_cleaned$HeartDisease == "Yes")
# Count the number of predictor variables (total variables minus the event column)
num_predictors <- ncol(heart_2020_cleaned) - 1  # Assuming 1 column is the event column
# Calculate EPV
epv <- num_events / num_predictors
print(paste("EPV:", epv))

#检查是否存在缺失值
sum(is.na(heart_2020_cleaned)) #0，表示数据集中并不存在缺失值



#汇总统计
summary(heart_2020_cleaned)

#可视化分析

# Distribution of Heart Disease
ggplot(heart_2020_cleaned, aes(x = HeartDisease)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Heart Disease", x = "Heart Disease", y = "Count") +
  scale_y_continuous(labels = comma)

# Distribution of BMI
ggplot(heart_2020_cleaned, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of BMI", x = "BMI", y = "Frequency")

# Heart Disease Distribution by Sex
ggplot(heart_2020_cleaned, aes(x = Sex, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Heart Disease Distribution by Sex", x = "Sex", y = "Proportion")

# Relationship between Smoking and Heart Disease
ggplot(heart_2020_cleaned, aes(x = Smoking, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between Smoking and Heart Disease", x = "Smoking", y = "Proportion")

# Relationship between Alcohol Drinking and Heart Disease
ggplot(heart_2020_cleaned, aes(x = AlcoholDrinking, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between Alcohol Drinking and Heart Disease", x = "Alcohol Drinking", y = "Proportion")

# Relationship between Age and Heart Disease
ggplot(heart_2020_cleaned, aes(x = AgeCategory, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between Age and Heart Disease", x = "Age Category", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heart Disease Distribution by Race
ggplot(heart_2020_cleaned, aes(x = Race, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Heart Disease Distribution by Race", x = "Race", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relationship between BMI and Heart Disease
ggplot(heart_2020_cleaned, aes(x = BMI, y = after_stat(density), fill = HeartDisease)) +
  geom_density(alpha = 0.5) +
  labs(title = "Relationship between BMI and Heart Disease", x = "BMI", y = "Density")

# Relationship between General Health and Heart Disease
ggplot(heart_2020_cleaned, aes(x = GenHealth, fill = HeartDisease)) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between General Health and Heart Disease", x = "General Health", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#进行编码
##对有顺序的变量按1，2，3...进行编码
heart_2020_cleaned <- heart_2020_cleaned %>%
  mutate(AgeCategory = case_when(
    AgeCategory == "18-24" ~ 0,
    AgeCategory == "25-29" ~ 1,
    AgeCategory == "30-34" ~ 2,
    AgeCategory == "35-39" ~ 3,
    AgeCategory == "40-44" ~ 4,
    AgeCategory == "45-49" ~ 5,
    AgeCategory == "50-54" ~ 6,
    AgeCategory == "55-59" ~ 7,
    AgeCategory == "60-64" ~ 8,
    AgeCategory == "65-69" ~ 9,
    AgeCategory == "70-74" ~ 10,
    AgeCategory == "75-79" ~ 11,
    AgeCategory == "80 or older" ~ 12
  ))


heart_2020_cleaned <- heart_2020_cleaned %>%
  mutate(GenHealth = case_when(
    GenHealth == "Poor" ~ 0,
    GenHealth == "Fair" ~ 1,
    GenHealth == "Good" ~ 2,
    GenHealth == "Very good" ~ 3,
    GenHealth == "Excellent" ~ 4
  ))


heart_2020_cleaned$Diabetic <- gsub("(?i)yes.*", "Yes", heart_2020_cleaned$Diabetic, perl = TRUE)
heart_2020_cleaned$Diabetic <- gsub("(?i)no.*", "No", heart_2020_cleaned$Diabetic, perl = TRUE)

# 将数据集中的找出数据集中所有二分类变量
binary_columns <- c("HeartDisease","Smoking", "AlcoholDrinking","Stroke","Diabetic",
                    "DiffWalking","PhysicalActivity", "Asthma", 
                    "KidneyDisease", "SkinCancer")

# 将二分类变量中的Yes替换为1，No替换为0
heart_2020_cleaned[binary_columns] <- lapply(heart_2020_cleaned[binary_columns], function(x) {
  x <- ifelse(x == "Yes", 1, ifelse(x == "No", 0, x))
  return(as.numeric(x))  # 将转换后的值转换为数值型
})

#将性别变量也替换为0，1
heart_2020_cleaned <- heart_2020_cleaned %>%
  mutate(Sex = case_when(
    Sex == "Female" ~ 0,
    Sex == "Male" ~ 1
  ))

#race进行one-hot编码
code_vars<-c('Race')
one_hot_encoded <- model.matrix(~.-1,data=heart_2020_cleaned[code_vars])
one_hot_df <- as.data.frame(one_hot_encoded)
##将编码后的字段与原来没有进行编码的字段重新组合成数据集
other_heart_2020_cleaned <- heart_2020_cleaned %>% dplyr::select(-Race)
heart_2020_cleaned1 <- cbind(other_heart_2020_cleaned,one_hot_df)
str(heart_2020_cleaned1)


