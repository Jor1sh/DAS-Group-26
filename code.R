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
data$Hours_PW <- as.numeric(data$Hours_PW)
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

# Order education level
edu_levels <- c(
  "Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", 
  "11th", "12th", "HS-grad", "Some-college", "Assoc-acdm", 
  "Assoc-voc", "Bachelors", "Masters", "Prof-school", "Doctorate"
)
data$Education <- factor(data$Education, levels = edu_levels, ordered = TRUE)
data$Education <- as.numeric(data$Education)

# Order nationality level
data$Nationality <- as.factor(data$Nationality)
data$Nationality <- as.numeric(data$Nationality)

# Merge Occupation
levels(data$Occupation) <- ifelse(levels(data$Occupation) %in% c("Exec-managerial"), 
                                  levels(data$Occupation), "Other")
data$Occupation <- factor(data$Occupation)
data$Occupation <- relevel(data$Occupation, ref = "Other") # Set “Other” as the base group

# Merge Marital Status
levels(data$Marital_Status) <- ifelse(levels(data$Marital_Status) %in% c("Married-civ-spouse"), 
                                      levels(data$Marital_Status), "Other")
data$Marital_Status <- factor(data$Marital_Status)
data$Marital_Status <- relevel(data$Marital_Status, ref = "Other") # Set “Other” as the base group

model_new <- glm(Income ~ Age + Education + Marital_Status + Occupation + Sex + Hours_PW + Nationality, 
                 data = data, 
                 family = binomial)
summary(model_new)

stepwise_model <- step(model_new, direction = "both", trace = 0)
summary(stepwise_model)

stepwise_aic <- AIC(stepwise_model)
print(paste("Stepwise AIC: ", stepwise_aic))

num_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(num_data)
print(cor_matrix)

data_encoded <- model.matrix(~ Marital_Status + Occupation + Sex- 1, data = data)
cor_matrix_encoded <- cor(data_encoded)
print(cor_matrix_encoded)

x <- model.matrix(Income ~ Age + Education + Marital_Status + Occupation + Sex + Hours_PW + Nationality - 1, data = data)
y <- data$Income

cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
print(paste("Best lambda for Lasso: ", cv_lasso$lambda.min))
plot(cv_lasso)

final_lasso_model <- glmnet(x, y, alpha = 1, lambda = cv_lasso$lambda.min)
coef(final_lasso_model)

# Extract coefficients from the model_new
coefnew_df <- as.data.frame(summary(model_new)$coefficients)
coefnew_df$Variable <- rownames(coefnew_df)
coefnew_df <- coefnew_df[coefnew_df$`Pr(>|z|)` < 0.05, ]
coefnew_df$Odds_Ratio <- exp(coefnew_df$Estimate)

ggplot(coefnew_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Log-Odds of Significant Variables",
       x = "Variables",
       y = "Log-Odds (Coefficient)")

ggplot(coefnew_df, aes(x = reorder(Variable, Odds_Ratio), y = Odds_Ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = exp(Estimate - `Std. Error`), ymax = exp(Estimate + `Std. Error`)), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Odds Ratios from model_new",
       x = "Variables",
       y = "Odds Ratio")