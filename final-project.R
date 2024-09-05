library(tidyverse)
library(gtsummary)
library(here)
library(readr)
library(dplyr)
library(broom)
library(rmarkdown)

# Load data using here::here
data <- read_csv(here("data", "raw", "strep_tb.csv"))

# Clean data
modified_data <- data %>%
  mutate(
    baseline_condition = factor(baseline_condition, labels = c("Good", "Fair", "Poor")),
    strep_resistance = factor(strep_resistance, labels = c("Sensitive", "Moderate", "Resistant")),
    radiologic_6m = factor(radiologic_6m, labels = c("Death", "Considerable Deterioration", "Moderate Deterioration", "No Change", "Moderate Improvement", "Considerable Improvement")),
    baseline_cavitation = factor(baseline_cavitation, labels = c("Yes", "No")),
    gender = factor(gender, labels = c("Male", "Female"))
  )

#Create summary table
tbl_summary(
  modified_data,
  by = gender,
  include = c(gender, arm, dose_strep_g, baseline_cavitation,
              improved,
              baseline_condition, strep_resistance),
  label = list(
    arm ~ "Treatment",
    dose_strep_g ~ "Dose of Streptomycin (in grams)",
    baseline_condition ~ "Condition at Baseline",
    strep_resistance ~ "Resistance to Streptomycin at 6 months",
    baseline_cavitation ~ "Cavitation of Lungs at Baseline",
    improved ~ "Improved Outcome"
  ))

#Regression
tbl_uvregression(
  modified_data,
  y = improved,
  include = c(gender, arm, dose_strep_g, baseline_cavitation,
              baseline_condition),
  label = list(
    gender ~ "Gender",
    arm ~ "Treatment",
    dose_strep_g ~ "Dose of Streptomycin (in grams)",
    baseline_condition ~ "Condition at Baseline",
    baseline_cavitation ~ "Cavitation of Lungs at Baseline"
  ),
  method = glm,
  method.args = list(family = binomial),
  exponentiate = T
)

#Histogram
hist(as.numeric(modified_data$strep_resistance),
     main = "Histogram of Streptomycin Resistance",
     xlab = "Resistance Level",
     breaks = 3)  # Set breaks to 3 since there are 3 levels

#Function
new_table_function <- function(model) {
  tbl_regression(
    model,
    exponentiate = T,
    label = list(
      gender ~ "Gender",
      arm ~ "Treatment Group",
      baseline_condition ~ "Condition at Baseline",
      baseline_cavitation ~ "Cavitation of Lungs at Baseline"
    ))
}

logistic_model <- glm(improved ~ gender*arm + baseline_condition + baseline_cavitation,
                      data = modified_data,
                      family = binomial ())

new_table_function(logistic_model)


