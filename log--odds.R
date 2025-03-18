library(ggplot2)

# Extract coefficients after step wise regression
coef_df <- as.data.frame(summary(stepwise_model)$coefficients)
coef_df$Variable <- rownames(coef_df)

# Keep only significant variables
coef_df <- coef_df[coef_df$`Pr(>|z|)` < 0.05, ]

# （log-odds）
ggplot(coef_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Log-Odds of Significant Variables",
       x = "Variables",
       y = "Log-Odds (Coefficient)")