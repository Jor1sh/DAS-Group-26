library(dplyr)
library(readr) 
#Boxplot
data <- data %>%
  mutate(Hours_PW = parse_number(Hours_PW)) 
data <- data %>%
  filter(!is.na(Hours_PW))

library(ggplot2)
ggplot(data, aes(x = as.factor(Income), y = Hours_PW, fill = as.factor(Income))) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Boxplot of Hours Worked per Week by Income Group",
    x = "Income Group",
    y = "Hours Worked per Week"
  ) +
  theme(legend.position = "none") 