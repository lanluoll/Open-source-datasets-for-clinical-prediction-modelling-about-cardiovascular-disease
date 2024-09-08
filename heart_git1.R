#导入包
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(mice)
library(MASS)

#导入数据
heart_git1 <- read.csv("/Users/luolan/Desktop/Datasets/heart_git1.csv")

#查看数据结构
View(heart_git1)
dim(heart_git1)  #(918,12)
str(heart_git1) #字段有多种类型
sum(duplicated(heart_git1)) #显示为0，表示无重复值

# Count the number of events (people with heart disease)
num_events <- sum(heart_git1$HeartDisease == 1)
# Count the number of predictor variables (total variables minus the event column)
num_predictors <- ncol(heart_git1) - 1  # Assuming 1 column is the event column
# Calculate EPV
epv <- num_events / num_predictors
print(paste("EPV:", epv))

#检查是否存在缺失值
sum(is.na(heart_git1)) #0，表示数据集中不存在缺失值

#汇总统计
summary(heart_git1)

#可视化分析

ggplot(heart_git1, aes(x = factor(HeartDisease))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Heart Disease", x = "Presence of Heart Disease", y = "Count")

ggplot(heart_git1, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

ggplot(heart_git1, aes(x = Sex, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Heart Disease by Sex", x = "Sex", y = "Proportion") +
  scale_fill_discrete(name = "Heart Disease")

ggplot(heart_git1, aes(x = ChestPainType, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  labs(title = "Relationship Between Chest Pain Type and Heart Disease", x = "Chest Pain Type", y = "Proportion") +
  scale_fill_discrete(name = "Heart Disease")

ggplot(heart_git1, aes(x = Age, fill = factor(HeartDisease))) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  labs(title = "Relationship Between Age and Heart Disease", x = "Age", y = "Count") +
  scale_fill_discrete(name = "Heart Disease")

ggplot(heart_git1, aes(x = RestingBP, fill = factor(HeartDisease))) +
  geom_histogram(binwidth = 10, position = "dodge", color = "black") +
  labs(title = "Relationship Between Resting Blood Pressure and Heart Disease", x = "Resting Blood Pressure", y = "Count") +
  scale_fill_discrete(name = "Heart Disease")

ggplot(heart_git1, aes(x = Cholesterol, fill = factor(HeartDisease))) +
  geom_histogram(binwidth = 20, position = "dodge", color = "black") +
  labs(title = "Relationship Between Cholesterol and Heart Disease", x = "Cholesterol", y = "Count") +
  scale_fill_discrete(name = "Heart Disease")

ggplot(heart_git1, aes(x = factor(FastingBS), fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  labs(title = "Relationship Between Fasting Blood Sugar and Heart Disease", x = "Fasting Blood Sugar", y = "Proportion") +
  scale_fill_discrete(name = "Heart Disease")

#进行编码
#将性别变量替换为0，1
heart_git1 <- heart_git1 %>%
  mutate(Sex = case_when(
    Sex == "F" ~ 0,
    Sex == "M" ~ 1
  ))

heart_git1 <- heart_git1 %>%
  mutate(ExerciseAngina = case_when(
    ExerciseAngina == "N" ~ 0,
    ExerciseAngina == "Y" ~ 1
  ))

#剩余字符型变量进行one_hot编码
code_vars<-c('ChestPainType','RestingECG','ST_Slope')
one_hot_encoded <- model.matrix(~.-1,data=heart_git1[code_vars])
one_hot_df <- as.data.frame(one_hot_encoded)
##将编码后的字段与原来没有进行编码的字段重新组合成数据集
# 使用 dplyr::select() 明确调用 dplyr 包中的 select 函数
other_heart_git1 <- heart_git1 %>%
  dplyr::select(-ChestPainType, -RestingECG, -ST_Slope)

heart_git1_1 <- cbind(other_heart_git1,one_hot_df)
str(heart_git1_1)

#变量间的相关性分析
correlation_matrix <- cor(heart_git1_1)
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black")


#划分训练集和测试集检验过拟合状况
train_index <- createDataPartition(heart_git1_1$HeartDisease, p = 0.7, list = FALSE)
train_data <- heart_git1_1[train_index, ]
test_data <- heart_git1_1[-train_index, ]
#广义线性回归模型
logistic_model <- glm(HeartDisease ~ ., data = train_data, family = binomial)
summary(logistic_model)
# 检查模型的充分性
par(mfrow = c(2, 2))
plot(logistic_model)
# 逐步回归
step_model <- stepAIC(logistic_model, direction = "both")
summary(step_model)
# 模型验证
library(boot)
final_model <- glm(formula(step_model), data = train_data, family = binomial)
# 交叉验证
cv_error <- cv.glm(train_data, final_model, K = 10)
print(cv_error$delta)

# 在训练集上预测
predict_train <- predict(step_model, type = "response")
# 计算并绘制训练集的ROC曲线
library(pROC)
roc_curve_train <- roc(train_data$HeartDisease, predict_train)
plot(roc_curve_train)
auc(roc_curve_train)
# 在测试集上进行预测
predict_test <- predict(step_model, newdata = test_data, type = "response")
# 计算并绘制测试集的ROC曲线
roc_curve_test <- roc(test_data$HeartDisease, predict_test)
plot(roc_curve_test)
auc(roc_curve_test)

# 安装并加载car包，如果尚未安装
if (!require(car)) {
  install.packages("car")
  library(car)
}

# 计算逐步回归得到的最终模型的VIF
vif_values <- vif(step_model)
print(vif_values)

