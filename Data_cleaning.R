library(dplyr)

data <- read.csv('dataset26.csv', na.strings = '?,')

data <- na.omit(data)

data <- data %>%
  mutate(across(2:ncol(data), ~ substr(.x, 1, nchar(.x) - 1)))

write.csv(data, 'cleaned_data.csv', row.names = FALSE)
