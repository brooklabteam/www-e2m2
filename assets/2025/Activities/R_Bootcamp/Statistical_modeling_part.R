library(stats)
library(dplyr)
library(ggplot2)

# Loading data
hospital_data <- read.csv(file = "simulated_hospital_dataset.csv", sep = ",")
head(hospital_data)

# Count the number of symptoms
hospital_data$number_symptoms <- sapply(strsplit(hospital_data$symptoms, ";"), length)

# Compute the stay period
hospital_data$stay_length <- as.numeric(as.Date(hospital_data$date_of_removal) - as.Date(hospital_data$date_of_admission))

# Linear Regression model using age and number of symptoms as predictors
lm_model <- lm(stay_length ~ age + number_symptoms, data = hospital_data)
summary(lm_model)

# Binary classification on ICU admission
hospital_data$in_icu <- ifelse(hospital_data$unit == "ICU", 1, 0)
# Step 1: Split each symptom string into a list
symptom_lists <- strsplit(hospital_data$symptoms, ";\\s*")
# Step 2: Flatten the list and get unique symptom names
all_symptoms <- unique(unlist(symptom_lists))
# Step 3: Create dummy variables for each symptom dynamically
for (symptom in all_symptoms) {
  hospital_data[[paste0("has_", symptom)]] <- sapply(symptom_lists, function(sym_list) symptom %in% sym_list)
}
glm_model <- glm(in_icu ~ age + number_symptoms + has_cough + has_fever + has_diarrhea, data = hospital_data, family = "binomial")
summary(glm_model)
exp(coef(glm_model))
install.packages("margins")
library(margins)
margins(glm_model)
