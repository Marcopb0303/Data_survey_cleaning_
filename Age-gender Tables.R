# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(viridis)
library(scales)
library(tidyr)
library(RColorBrewer)

# Read the datasets
data_2020 <- read.csv("2020.csv")
data_2025 <- read.csv("crew_datset_keys_analysis_ready.csv")

# Define age order for proper sorting
age_order <- c("Under 25", "25-29", "30-34", "35-39", "40-44", "45-49",
               "50-54", "55-59", "60-64", "65 or older")

# Function to standardize gender categories
standardize_gender <- function(gender_col) {
  gender_col <- tolower(trimws(gender_col))
  case_when(
    gender_col %in% c("female", "woman") ~ "Women",
    gender_col %in% c("male", "man") ~ "Men",
    grepl("nonbinary|non-binary", gender_col) ~ "Nonbinary",
    TRUE ~ as.character(gender_col)  # Keep original if no match
  )
}

# Extract and clean relevant columns
df_2020 <- data_2020 %>%
  select(gender = m27, age = m25) %>%  # Note: using m27 for 2020 based on your output
  filter(!is.na(gender) & !is.na(age) & gender != "") %>%  # Filter out empty gender values
  mutate(
    gender = standardize_gender(gender),
    age = factor(age, levels = age_order),
    year = 2020
  )

df_2025 <- data_2025 %>%
  select(gender = m27, age = m25) %>%
  filter(!is.na(gender) & !is.na(age) & gender != "") %>%  # Filter out empty gender values
  mutate(
    gender = standardize_gender(gender),
    age = factor(age, levels = age_order),
    year = 2025
  )

# Check data quality and fix any issues
cat("=== Data Quality Check ===\n")
cat("2020 unique gender values:", unique(data_2020$m27), "\n")
cat("2025 unique gender values:", unique(data_2025$m27), "\n")

# Check for empty or problematic values
cat("2020 empty gender count:", sum(data_2020$m27 == "" | is.na(data_2020$m27)), "\n")
cat("2025 empty gender count:", sum(data_2025$m27 == "" | is.na(data_2025$m27)), "\n")
cat("=== Data Summary ===\n")
cat("2020 Dataset:\n")
cat("Sample size:", nrow(df_2020), "\n")
cat("Gender distribution:\n")
print(table(df_2020$gender))
cat("\nAge distribution:\n")
print(table(df_2020$age))

cat("\n2025 Dataset:\n")
cat("Sample size:", nrow(df_2025), "\n")
cat("Gender distribution:\n")
print(table(df_2025$gender))
cat("\nAge distribution:\n")
print(table(df_2025$age))

# Task 1: Create two-way tables for age-gender combinations
table_2020 <- table(df_2020$age, df_2020$gender)
table_2025 <- table(df_2025$age, df_2025$gender)

cat("\n=== 2020 Age-Gender Cross-tabulation ===\n")
print(table_2020)
cat("\n=== 2025 Age-Gender Cross-tabulation ===\n")
print(table_2025)

# Check for small cell sizes
min_cell_size <- 5
small_cells_2020 <- which(table_2020 < min_cell_size, arr.ind = TRUE)
small_cells_2025 <- which(table_2025 < min_cell_size, arr.ind = TRUE)

if(nrow(small_cells_2020) > 0) {
  cat("\nSmall cells in 2020 data (less than", min_cell_size, "):\n")
  print(small_cells_2020)
}

if(nrow(small_cells_2025) > 0) {
  cat("\nSmall cells in 2025 data (less than", min_cell_size, "):\n")
  print(small_cells_2025)
}

# Create visually appealing two-way tables
create_visual_table <- function(table_data, title) {
  table_df <- as.data.frame.matrix(table_data)
  table_df$Age <- rownames(table_df)
  table_df <- table_df[, c("Age", setdiff(names(table_df), "Age"))]
  
  # Debug: print structure
  cat("Table structure for", title, ":\n")
  cat("Columns:", colnames(table_df), "\n")
  cat("Number of columns:", ncol(table_df), "\n")
  
  # Create a simple table without complex headers first
  kable(table_df, caption = title, format = "html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE, position = "center") %>%
    column_spec(1, bold = TRUE, background = "#f7f7f7")
}

# Display visual tables
visual_table_2020 <- create_visual_table(table_2020, "2020 Age-Gender Distribution")
visual_table_2025 <- create_visual_table(table_2025, "2025 Age-Gender Distribution")
print(visual_table_2020)
print(visual_table_2025)

# Task 2: Calculate weights based on 2020 distribution
prop_2020 <- prop.table(table_2020)
prop_2025 <- prop.table(table_2025)

cat("\n=== Proportion Tables ===\n")
cat("2020 Proportions:\n")
print(round(prop_2020, 4))
cat("\n2025 Proportions:\n")
print(round(prop_2025, 4))

# Get all unique age-gender combinations from both datasets
all_ages <- union(rownames(prop_2020), rownames(prop_2025))
all_genders <- union(colnames(prop_2020), colnames(prop_2025))

# Find combinations that exist in 2025 but not in 2020
combinations_2020 <- expand.grid(Age = rownames(table_2020), Gender = colnames(table_2020), stringsAsFactors = FALSE)
combinations_2020$exists_2020 <- TRUE
combinations_2020$count_2020 <- apply(combinations_2020, 1, function(x) table_2020[x[1], x[2]])

combinations_2025 <- expand.grid(Age = rownames(table_2025), Gender = colnames(table_2025), stringsAsFactors = FALSE)
combinations_2025$exists_2025 <- TRUE
combinations_2025$count_2025 <- apply(combinations_2025, 1, function(x) table_2025[x[1], x[2]])

# Merge to find differences
all_combinations <- merge(combinations_2025, combinations_2020, by = c("Age", "Gender"), all.x = TRUE)
all_combinations$exists_2020[is.na(all_combinations$exists_2020)] <- FALSE
all_combinations$count_2020[is.na(all_combinations$count_2020)] <- 0

# Find combinations in 2025 but not in 2020 (or with zero count in 2020)
missing_in_2020 <- all_combinations[all_combinations$count_2020 == 0, ]

cat("\n=== Age-Gender Combinations Analysis ===\n")
cat("Combinations that exist in 2025 but have ZERO count in 2020:\n")
if(nrow(missing_in_2020) > 0) {
  missing_in_2020_display <- missing_in_2020[, c("Age", "Gender", "count_2025", "count_2020")]
  names(missing_in_2020_display) <- c("Age Group", "Gender", "2025 Count", "2020 Count")
  print(missing_in_2020_display, row.names = FALSE)
} else {
  cat("None - all 2025 combinations exist in 2020\n")
}

# Also check the reverse - combinations in 2020 but not in 2025
all_combinations_reverse <- merge(combinations_2020, combinations_2025, by = c("Age", "Gender"), all.x = TRUE)
all_combinations_reverse$exists_2025[is.na(all_combinations_reverse$exists_2025)] <- FALSE
all_combinations_reverse$count_2025[is.na(all_combinations_reverse$count_2025)] <- 0

missing_in_2025 <- all_combinations_reverse[all_combinations_reverse$count_2025 == 0, ]

cat("\nCombinations that exist in 2020 but have ZERO count in 2025:\n")
if(nrow(missing_in_2025) > 0) {
  missing_in_2025_display <- missing_in_2025[, c("Age", "Gender", "count_2020", "count_2025")]
  names(missing_in_2025_display) <- c("Age Group", "Gender", "2020 Count", "2025 Count")
  print(missing_in_2025_display, row.names = FALSE)
} else {
  cat("None - all 2020 combinations exist in 2025\n")
}

# Get all unique age-gender combinations from both datasets
all_ages <- union(rownames(prop_2020), rownames(prop_2025))
all_genders <- union(colnames(prop_2020), colnames(prop_2025))

# Create expanded tables with all combinations
prop_2020_expanded <- matrix(0, nrow = length(all_ages), ncol = length(all_genders),
                             dimnames = list(all_ages, all_genders))
prop_2025_expanded <- matrix(0, nrow = length(all_ages), ncol = length(all_genders),
                             dimnames = list(all_ages, all_genders))

# Fill in the existing values
for(age in rownames(prop_2020)) {
  for(gender in colnames(prop_2020)) {
    prop_2020_expanded[age, gender] <- prop_2020[age, gender]
  }
}

for(age in rownames(prop_2025)) {
  for(gender in colnames(prop_2025)) {
    prop_2025_expanded[age, gender] <- prop_2025[age, gender]
  }
}

# Create weight matrix (handle division by zero)
weight_matrix <- prop_2020_expanded / prop_2025_expanded
weight_matrix[is.infinite(weight_matrix)] <- 0
weight_matrix[is.na(weight_matrix)] <- 0
weight_matrix[prop_2025_expanded == 0] <- 0  # If no one in 2025 group, weight = 0

cat("\n=== Weight Matrix (2020 proportions / 2025 proportions) ===\n")
print(round(weight_matrix, 3))

# Apply weights to individual observations in 2025 data
df_2025_weighted <- df_2025 %>%
  mutate(
    age_char = as.character(age),
    gender_char = as.character(gender)
  ) %>%
  rowwise() %>%
  mutate(
    weight = ifelse(
      age_char %in% rownames(weight_matrix) & gender_char %in% colnames(weight_matrix),
      weight_matrix[age_char, gender_char],
      0
    )
  ) %>%
  ungroup() %>%
  select(-age_char, -gender_char) %>%
  filter(weight > 0)  # Remove cases with zero weights

cat("\n=== Weight Application Summary ===\n")
cat("Original 2025 sample size:", nrow(df_2025), "\n")
cat("Weighted 2025 sample size:", nrow(df_2025_weighted), "\n")
cat("Cases with zero weights:", nrow(df_2025) - nrow(df_2025_weighted), "\n")
if(nrow(df_2025_weighted) > 0) {
  cat("Weight range:", round(min(df_2025_weighted$weight), 3), "to", round(max(df_2025_weighted$weight), 3), "\n")
}

# Task 3: Create comprehensive comparison charts

# Set up color palette
gender_colors <- c("Women" = "#e74c3c", "Men" = "#3498db", "Nonbinary" = "#9b59b6")

# Prepare data for all visualizations
plot_data_2020 <- df_2020 %>%
  count(age, gender) %>%
  mutate(dataset = "2020 Original", 
         proportion = n / sum(n),
         percentage = proportion * 100)

plot_data_unweighted <- df_2025 %>%
  count(age, gender) %>%
  mutate(dataset = "2025 Unweighted", 
         proportion = n / sum(n),
         percentage = proportion * 100)

plot_data_weighted <- df_2025_weighted %>%
  count(age, gender, wt = weight) %>%
  mutate(dataset = "2025 Weighted", 
         proportion = n / sum(n),
         percentage = proportion * 100)

# Combine all data
all_plot_data <- bind_rows(plot_data_2020, plot_data_unweighted, plot_data_weighted) %>%
  mutate(dataset = factor(dataset, levels = c("2020 Original", "2025 Unweighted", "2025 Weighted")))

# Chart 1: Age-Gender Distribution Comparison (Grouped Bar Chart)
chart1 <- ggplot(all_plot_data, aes(x = age, y = percentage, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~dataset, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Age-Gender Distribution Comparison Across Datasets",
    x = "Age Group", 
    y = "Percentage (%)", 
    fill = "Gender"
  )

# Chart 2: Side-by-Side Overall Gender Distribution
gender_summary <- all_plot_data %>%
  group_by(dataset, gender) %>%
  summarise(total_percentage = sum(percentage), .groups = "drop")

chart2 <- ggplot(gender_summary, aes(x = dataset, y = total_percentage, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.7) +
  scale_fill_manual(values = gender_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Overall Gender Distribution Comparison",
    x = "Dataset", 
    y = "Percentage (%)", 
    fill = "Gender"
  ) +
  geom_text(aes(label = paste0(round(total_percentage, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3)

# Chart 3: Age Distribution Comparison
age_summary <- all_plot_data %>%
  group_by(dataset, age) %>%
  summarise(total_percentage = sum(percentage), .groups = "drop")

chart3 <- ggplot(age_summary, aes(x = age, y = total_percentage, color = dataset, group = dataset)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("#2c3e50", "#e67e22", "#27ae60")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Age Distribution Trends Across Datasets",
    x = "Age Group", 
    y = "Percentage (%)", 
    color = "Dataset"
  )

# Chart 4: Heatmap of Age-Gender Combinations
heatmap_data <- all_plot_data %>%
  select(dataset, age, gender, percentage) %>%
  pivot_wider(names_from = gender, values_from = percentage, values_fill = 0) %>%
  pivot_longer(cols = -c(dataset, age), names_to = "gender", values_to = "percentage")

chart4 <- ggplot(heatmap_data, aes(x = gender, y = age, fill = percentage)) +
  geom_tile(color = "white", size = 0.5) +
  facet_wrap(~dataset, ncol = 3) +
  scale_fill_viridis_c(name = "Percentage", option = "plasma", alpha = 0.8) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Age-Gender Distribution Heatmap",
    x = "Gender", 
    y = "Age Group"
  ) +
  geom_text(aes(label = round(percentage, 1)), color = "white", size = 3)

# Chart 5: Difference between Weighted and Unweighted
diff_data <- plot_data_weighted %>%
  select(age, gender, weighted_pct = percentage) %>%
  left_join(
    plot_data_unweighted %>% select(age, gender, unweighted_pct = percentage),
    by = c("age", "gender")
  ) %>%
  mutate(difference = weighted_pct - unweighted_pct)

chart5 <- ggplot(diff_data, aes(x = age, y = difference, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Impact of Weighting: Difference Between Weighted and Unweighted 2025 Data",
    x = "Age Group", 
    y = "Percentage Point Difference", 
    fill = "Gender",
    subtitle = "Positive values indicate weighting increased the proportion"
  )

# Chart 6: Decision-Making Chart - How Different Are the Distributions?
comparison_2020_vs_2025 <- plot_data_2020 %>%
  select(age, gender, pct_2020 = percentage) %>%
  left_join(
    plot_data_unweighted %>% select(age, gender, pct_2025_unweighted = percentage),
    by = c("age", "gender")
  ) %>%
  mutate(
    pct_2025_unweighted = ifelse(is.na(pct_2025_unweighted), 0, pct_2025_unweighted),
    difference_unweighted = pct_2025_unweighted - pct_2020,
    abs_difference = abs(difference_unweighted)
  )

chart6 <- ggplot(comparison_2020_vs_2025, aes(x = age, y = difference_unweighted, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = gender_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Demographic Shift: 2025 vs 2020 Distribution",
    x = "Age Group", 
    y = "Percentage Point Difference (2025 - 2020)", 
    fill = "Gender",
    subtitle = "Large differences suggest weighting may be beneficial"
  )

# Chart 7: Magnitude of Changes - Should You Weight?
total_absolute_change <- sum(comparison_2020_vs_2025$abs_difference, na.rm = TRUE)
max_change <- max(comparison_2020_vs_2025$abs_difference, na.rm = TRUE)

chart7_data <- comparison_2020_vs_2025 %>%
  arrange(desc(abs_difference)) %>%
  mutate(
    group_label = paste(age, gender, sep = " - "),
    cumulative_change = cumsum(abs_difference),
    cumulative_pct = cumulative_change / total_absolute_change * 100
  )

chart7 <- ggplot(chart7_data, aes(x = reorder(group_label, abs_difference), y = abs_difference, fill = gender)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = gender_colors) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Magnitude of Demographic Changes (2020 to 2025)",
    x = "Age-Gender Group", 
    y = "Absolute Percentage Point Change", 
    fill = "Gender",
    subtitle = paste("Total change:", round(total_absolute_change, 1), "percentage points")
  )

# Display all charts
print(chart1)
print(chart2)
print(chart3)
print(chart4)
print(chart5)
print(chart6)
print(chart7)

# Decision-Making Summary
cat("\n=== WEIGHTING DECISION SUMMARY ===\n")
cat("Total absolute demographic change:", round(total_absolute_change, 1), "percentage points\n")
cat("Largest single group change:", round(max_change, 1), "percentage points\n")

# Decision rules
if(total_absolute_change > 10) {
  cat("RECOMMENDATION: Consider using WEIGHTED data\n")
  cat("Reason: Large demographic shifts detected (>10 percentage points total change)\n")
} else if(total_absolute_change > 5) {
  cat("RECOMMENDATION: Weighting may be beneficial\n") 
  cat("Reason: Moderate demographic shifts detected (5-10 percentage points total change)\n")
} else {
  cat("RECOMMENDATION: Unweighted data may be sufficient\n")
  cat("Reason: Small demographic shifts detected (<5 percentage points total change)\n")
}

if(max_change > 3) {
  cat("NOTE: At least one demographic group changed by >3 percentage points\n")
  cat("Consider examining specific groups that changed most\n")
}

cat("\nKEY CHARTS FOR DECISION-MAKING:\n")
cat("- Chart 6: Shows which groups changed most between 2020-2025\n")
cat("- Chart 7: Ranks groups by magnitude of change\n") 
cat("- Chart 5: Shows impact of weighting on final results\n")
cat("- Chart 1: Direct comparison of all three distributions\n")

# Summary statistics table
summary_stats <- data.frame(
  Metric = c("2020 Sample Size", "2025 Sample Size", "2025 Effective Sample Size (Weighted)",
             "Average Weight", "Weight Range (Min-Max)"),
  Value = c(
    nrow(df_2020),
    nrow(df_2025),
    round(sum(df_2025_weighted$weight), 1),
    round(mean(df_2025_weighted$weight), 3),
    paste(round(min(df_2025_weighted$weight), 3), "-", round(max(df_2025_weighted$weight), 3))
  )
)

cat("\n=== Summary Statistics ===\n")
print(kable(summary_stats, format = "html") %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE))

# Create comparison table of proportions
comparison_table <- all_plot_data %>%
  select(dataset, age, gender, percentage) %>%
  pivot_wider(names_from = dataset, values_from = percentage, values_fill = 0) %>%
  arrange(age, gender)

cat("\n=== Detailed Proportion Comparison Table ===\n")
print(kable(comparison_table, digits = 2, format = "html",
            caption = "Percentage Distribution Across All Datasets") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                      full_width = FALSE) %>%
        column_spec(1:2, bold = TRUE))

# Export weighted data
# write.csv(df_2025_weighted, "weighted_2025_data.csv", row.names = FALSE)
# cat("\nWeighted data exported to 'weighted_2025_data.csv'\n")