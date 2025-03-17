library(dplyr)

data <- read.csv('Cleaned_data.csv')

data$Income <- factor(data$Income)

# Group education levels by degree
data <- data %>%
  mutate(Degree = case_when(
    Education %in% c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "11th", "12th", "HS-grad") ~ "High School or Below",
    Education %in% c("Some-college", "Assoc-acdm", "Assoc-voc", "Bachelors") ~ "Bachelor",
    Education == "Masters" ~ "Masters",
    Education %in% c("Prof-school", "Doctorate") ~ "Doctorate"
  ))
data$Degree <- factor(data$Degree)
#data$Degree <- relevel(data$Degree, ref = "High School or Below") # Set "High School or Below" as the base group

# Order education level
edu_levels <- c(
  "Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", 
  "11th", "12th", "HS-grad", "Some-college", "Assoc-acdm", 
  "Assoc-voc", "Bachelors", "Masters", "Prof-school", "Doctorate"
)
data$Education <- factor(data$Education, levels = edu_levels, ordered = TRUE)
data$Education <- as.numeric(data$Education)

# Merge Occupation
data$Occupation[!data$Occupation %in% c("Exec-managerial")] <- "Other"
data$Occupation <- factor(data$Occupation)
data$Occupation <- relevel(data$Occupation, ref = "Other") # Set “Other” as the base group

# Merge Marital Status
data$Marital_Status[!data$Marital_Status %in% c("Married-civ-spouse")] <- "Other"
data$Marital_Status <- factor(data$Marital_Status)
data$Marital_Status <- relevel(data$Marital_Status, ref = "Other") # Set “Other” as the base group

# Model
model <- glm(
  Income ~ Age + Education + Marital_Status + Occupation + Hours_PW,
  data = data,
  family = binomial
)

summary(model)

