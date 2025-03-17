library(ggplot2)
library(glmnet)
library(tidyverse)
library(gt)
library(patchwork)
library(gridExtra)
library(moderndive)
library(skimr)

data <- read.csv('dataset26.csv', na.strings = '?,')
data <- na.omit(data)
data <- data %>%
  mutate(across(2:ncol(data), ~ substr(.x, 1, nchar(.x) - 1)))
write.csv(data, 'cleaned_data.csv', row.names = FALSE)

data$Income <- ifelse(data$Income == "<=50", 0, 1)
data$Education <- as.factor(data$Education)
data$Marital_Status <- as.factor(data$Marital_Status)
data$Occupation <- as.factor(data$Occupation)
data$Sex <- as.factor(data$Sex)
data$Nationality <- as.factor(data$Nationality)
str(data)

model <- glm(Income ~ Age + Education + Marital_Status + Occupation + Sex + Hours_PW + Nationality, 
             data = data, 
             family = binomial)
summary(model)

coef_table <- summary(model)$coefficients
coef_df <- as.data.frame(coef_table)
significant_vars <- coef_df[coef_df$`Pr(>|z|)` < 0.05, ]
significant_vars

stepwise_model <- step(model, direction = "both", trace = 0)
summary(stepwise_model)
stepwise_aic <- AIC(stepwise_model)
print(paste("Stepwise AIC: ", stepwise_aic))

num_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(num_data)
print(cor_matrix)
data_encoded <- model.matrix(~ Education + Marital_Status + Occupation + Sex + Nationality - 1, data = data)
cor_matrix_encoded <- cor(data_encoded)
print(cor_matrix_encoded)

x <- cbind(data_encoded, data$Age)
y <- data$Income
lasso_model <- glmnet(x, y, alpha = 1)
cv_lasso <- cv.glmnet(x, y, alpha = 1)
print(paste("Best lambda: ", cv_lasso$lambda.min))
plot(cv_lasso)
final_lasso_model <- glmnet(x, y, alpha = 1, lambda = cv_lasso$lambda.min)
print(coef(final_lasso_model))