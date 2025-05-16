# Packages installation and loading
install.packages('datasauRus')
library(ggplot2)
library(dplyr)
library(datasauRus)

# Load the data
data("datasaurus_dozen")

# Summary statistics
summaries <- datasaurus_dozen %>%
  group_by(dataset) %>%
  summarize(
    n_points = n(),
    mean_x = mean(x), sd_x = sd(x), min_x = min(x), max_x = max(x), IQR_x = IQR(x),
    mean_y = mean(y), sd_y = sd(y), min_y = min(y), max_y = max(y), IQR_y = IQR(y)
  )

# View all results
print(summaries, n = Inf)

# Plot all datasets
datasaurus_plot <- ggplot(datasaurus_dozen, aes(x = x, y = y)) +
  geom_point(color = "#0072B2", size = 0.5, alpha = 0.7) +
  theme_minimal() +
  facet_wrap(~ dataset, ncol = 4) +
  labs(
    title = "Same Stats, Different Stories",
    subtitle = "Each dataset has nearly identical means, SDs\n, and correlations",
    caption = "Source: Datasaurus Dozen by Alberto Cairo & Justin Matejka"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    axis.title = element_blank()
  )
datasaurus_plot
ggsave(plot = datasaurus_plot, filename = "datasauRus_exmaple.png", units = "cm", width = 13, height = 10)

# Hospital dataset visualization
hospital_data <- read.csv(file = "simulated_hospital_dataset.csv", sep = ",")
head(hospital_data)

# Scatter plot/point geometry
number_of_admission_by_day <- hospital_data %>% group_by(date_of_admission) %>% summarize(count = n())
admission_plot <- ggplot(number_of_admission_by_day, aes(x = date_of_admission, y = count)) +
  geom_point(color = "#0072B2", size = 1.2, alpha = 0.7) +
  labs(
    title = "Daily Hospital Admissions During Outbreak",
    x = "Date of Admission",
    y = "Number of Admissions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

admission_plot

#Bar Plot
number_of_patient_by_unit <- hospital_data %>% group_by(unit) %>% summarize(count = n())

unit_plot <- ggplot(hospital_data, aes(x = unit)) +
  geom_bar(color = "#0072B2", width = 0.9, alpha = 0.7, stat= "count") +
  labs(
    title = "Number of patient by unit",
    x = "Unit",
    y = "Number of Patient"
  ) +
  theme_minimal()

unit_plot

# Histogram
age_plot <- ggplot(hospital_data, aes(x = age)) +
  geom_histogram() +
  labs(
    title = "Distribution of age",
    x = "Age"
  ) +
  theme_minimal()

age_plot

# Boxplot
ageBox_plot <- ggplot(hospital_data, aes(x = unit, y=age)) +
  geom_boxplot() +
  labs(
    title = "Distribution of age",
    x = "Unit"
  ) +
  theme_minimal()

ageBox_plot

#Line plot
admission_plot_line <- ggplot(number_of_admission_by_day, aes(x = date_of_admission, y = count)) +
  geom_line(aes(group = 1)) +
  geom_point(color = "#0072B2", size = 1.2, alpha = 0.7) +
  labs(
    title = "Daily Hospital Admissions During Outbreak",
    x = "Date of Admission",
    y = "Number of Admissions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

admission_plot_line

# Color mapping
ggplot(hospital_data, aes(x = unit, y = age, color = unit)) +
  geom_point()+
  labs(
    title = "Age by unit",
    x = "Unit",
    y = "Age"
  ) +
  theme_minimal()

# Faceting
ggplot(hospital_data, aes(x = age)) +
  geom_histogram() +
  facet_wrap(~ unit)

