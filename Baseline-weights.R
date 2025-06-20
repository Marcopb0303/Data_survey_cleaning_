# Load required libraries
library(dplyr)

# Step 1: Load and prepare 2020 data
data_2020 <- read.csv("2020.csv")  # Adjust path as needed

# Step 2: Create the same age and gender groupings we use for 2025
# Function to collapse age groups into 5 broader categories  
collapse_age_groups <- function(age_col) {
  case_when(
    age_col %in% c("Under 25", "25-29") ~ "Under 30",
    age_col %in% c("30-34", "35-39") ~ "30-39", 
    age_col %in% c("40-44", "45-49") ~ "40-49",
    age_col %in% c("50-54", "55-59") ~ "50-59",
    age_col %in% c("60-64", "65 or older") ~ "60+",
    TRUE ~ as.character(age_col)
  )
}

# Function to standardize gender categories
standardize_gender <- function(gender_col) {
  gender_col <- tolower(trimws(gender_col))
  case_when(
    gender_col %in% c("female", "woman") ~ "Women",
    gender_col %in% c("male", "man") ~ "Men",
    grepl("nonbinary|non-binary", gender_col) ~ "Nonbinary",
    TRUE ~ as.character(gender_col)
  )
}

# Step 3: Calculate 2020 baseline weights
calculate_2020_baseline_weights <- function(data_2020) {
  
  # Clean and group 2020 data
  baseline_data <- data_2020 %>%
    select(gender = m27, age = m25) %>%  # Using the keycode column names
    filter(!is.na(gender) & !is.na(age) & gender != "") %>%
    mutate(
      gender_clean = standardize_gender(gender),
      age_clean = collapse_age_groups(age)
    ) %>%
    filter(!is.na(gender_clean) & !is.na(age_clean))
  
  # Calculate proportions for each age-gender combination
  baseline_weights <- baseline_data %>%
    count(age_clean, gender_clean) %>%
    mutate(
      total_n = sum(n),
      weight_2020 = n / total_n
    ) %>%
    select(age_clean, gender_clean, weight_2020, n_2020 = n, total_n)
  
  # Display the baseline weights
  cat("=== 2020 Baseline Weights ===\n")
  print(baseline_weights)
  
  # Create a summary table
  baseline_summary <- baseline_weights %>%
    pivot_wider(names_from = gender_clean, values_from = c(weight_2020, n_2020), 
                values_fill = 0, names_sep = "_")
  
  cat("\n=== 2020 Baseline Summary Table ===\n")
  print(baseline_summary)
  
  # Verify weights sum to 1
  total_weight <- sum(baseline_weights$weight_2020)
  cat("\n=== Verification ===\n")
  cat("Total sample size:", unique(baseline_weights$total_n), "\n")
  cat("Sum of weights:", round(total_weight, 6), "\n")
  cat("Weights sum to 1:", abs(total_weight - 1) < 0.0001, "\n")
  
  return(baseline_weights)
}

# Step 4: Calculate and save the baseline weights
baseline_weights_2020 <- calculate_2020_baseline_weights(data_2020)

print(baseline_weights_2020)

# Step 5: Save for future use
# saveRDS(baseline_weights_2020, "baseline_weights_2020.rds")
# cat("\nBaseline weights saved to 'baseline_weights_2020.rds'\n")