set.seed(2025)

n <- 1000
patient_id <- 1:n
start_date <- as.Date("2024-02-01")
date_of_admission <- start_date + sample(0:29, n, replace = TRUE)
removal_delay <- sample(c(3:15, NA), n, replace = TRUE, prob = c(rep(0.07, 13), 0.09))
date_of_removal <- date_of_admission + removal_delay
date_of_removal[is.na(removal_delay)] <- NA
age <- round(rbeta(n, 2, 1.5) * 90) + 10

# Assign units
unit_levels <- c("ICU", "General", "Reanimation", "Morgue")
unit_probs <- c(0.4, 0.3, 0.2, 0.1)
unit <- sample(unit_levels, n, replace = TRUE, prob = unit_probs)

# Define machine options per unit
get_machines <- function(u) {
  if (u == "Morgue") return("None")
  if (u == "Reanimation") {
    return(sample(c("Oxygen", "Ventilator", "Oxygen, Ventilator", 
                    "Oxygen, ECMO", "Ventilator, ECMO", 
                    "Oxygen, Ventilator, ECMO"),
                  1, prob = c(0.2, 0.2, 0.25, 0.1, 0.1, 0.15)))
  }
  if (u == "ICU") {
    return(sample(c("None", "Oxygen", "Ventilator", "Oxygen, Ventilator",
                    "ECMO", "Oxygen, ECMO", "Ventilator, ECMO"),
                  1, prob = c(0.15, 0.2, 0.2, 0.2, 0.05, 0.1, 0.1)))
  }
  if (u == "General") {
    return(sample(c("None", "Oxygen"), 1, prob = c(0.7, 0.3)))
  }
}

machines <- sapply(unit, get_machines)

# Assign status based on unit and machine
status <- character(n)
for (i in 1:n) {
  if (unit[i] == "Morgue") {
    status[i] <- "Deceased"
  } else if (grepl("ECMO", machines[i])) {
    status[i] <- sample(c("Critical", "Deceased"), 1, prob = c(0.6, 0.4))
  } else if (unit[i] == "ICU") {
    status[i] <- sample(c("Critical", "Recovered", "Deceased"), 1, prob = c(0.4, 0.5, 0.1))
  } else if (unit[i] == "Reanimation") {
    status[i] <- sample(c("Critical", "Recovered"), 1, prob = c(0.7, 0.3))
  } else {
    status[i] <- sample(c("Recovered", "Critical"), 1, prob = c(0.8, 0.2))
  }
}

# Build the final dataset
hospital_data <- data.frame(
  patient_id = patient_id,
  date_of_admission = date_of_admission,
  date_of_removal = date_of_removal,
  age = age,
  unit = unit,
  machines = machines,
  status = status,
  stringsAsFactors = FALSE
)
# Define symptom pool
symptom_pool <- c("fever", "cough", "shortness_of_breath", "fatigue", "headache", "diarrhea", "loss_of_taste", "nausea")

# Random symptoms for each patient
generate_symptoms <- function() {
  paste(sample(symptom_pool, size = sample(1:4, 1), replace = FALSE), collapse = "; ")
}

symptoms <- replicate(n, generate_symptoms())

# Add to your data frame
hospital_data$symptoms <- symptoms

# View a preview
head(hospital_data)

write.csv(x = hospital_data, file = "simulated_hospital_dataset.csv", row.names = FALSE)
