library(ggplot2)
library(gridExtra)

# Define colors
colors <- c("red", "blue")

# Generate Age vs. Income boxplot
plot_age <- ggplot(data, aes(x = factor(Income), y = Age, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  xlab('Income') + 
  ylab('Age') +
  ggtitle('Age vs. Income Boxplot') +
  theme_minimal()

# Generate Education vs. Income boxplot
plot_education <- ggplot(data, aes(x = factor(Income), y = Education, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  xlab('Income') + 
  ylab('Education Level') +
  ggtitle('Education Level vs. Income Boxplot') +
  theme_minimal()

# Generate Marital Status vs. Income boxplot
plot_marital_status <- ggplot(data, aes(x = factor(Income), y = Marital_Status, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  xlab('Income') + 
  ylab('Marital Status') +
  ggtitle('Marital Status vs. Income Boxplot') +
  theme_minimal()

# Generate Occupation vs. Income boxplot
plot_occupation <- ggplot(data, aes(x = factor(Income), y = Occupation, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  xlab('Income') + 
  ylab('Occupation') +
  ggtitle('Occupation vs. Income Boxplot') +
  theme_minimal()

# Generate Sex vs. Income boxplot
plot_sex <- ggplot(data, aes(x = factor(Income), y = Sex, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  xlab('Income') + 
  ylab('Sex') +
  ggtitle('Sex vs. Income Boxplot') +
  theme_minimal()

# Generate Hours per Week vs. Income boxplot
plot_hours <- ggplot(data, aes(x = factor(Income), y = Hours_PW, fill = factor(Income))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  xlab('Income') + 
  ylab('Hours per Week') +
  ggtitle('Hours per Week vs. Income Boxplot') +
  theme_minimal()

# Arrange all boxplots in a grid layout
grid.arrange(plot_age, plot_education, plot_marital_status, 
             plot_occupation, plot_sex, plot_hours, 
             ncol = 3)
