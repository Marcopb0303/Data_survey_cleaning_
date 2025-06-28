library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)
library(dplyr)

crew_data <- readRDS("crew_survey_cleaned.rds") %>%
  clean_names()
write_csv(
  tibble(column_names = names(crew_data)),
  "crew_column_names.csv"
)
view(crew_data)
# crew_data_filtered <- crew_data %>%
#   filter(!is.na(what_is_your_gender_identity))
# saveRDS(crew_data_filtered, file = "crew_survey_cleaned_NA_gender.rds")

crew_data <- crew_data %>%
  mutate(
    gender_clean = case_when(
      str_to_lower(what_is_your_gender_identity) %in% c("female", "woman") ~ "Women",
      str_to_lower(what_is_your_gender_identity) %in% c("male", "man") ~ "Men",
      str_detect(str_to_lower(what_is_your_gender_identity), "nonbinary|non-binary") ~ "Nonbinary",
      TRUE ~ NA_character_
    )
  )


gender_2025 <- crew_data %>%
  filter(!is.na(gender_clean)) %>%
  count(Gender = gender_clean) %>%
  mutate(Year = 2025)

historic <- tribble(
  ~Gender, ~`2005`, ~`2010`, ~`2015`, ~`2020`,
  "Women",    1175,    1972,    1700,    2414,
  "Men",       659,     929,     482,     512,
  "Nonbinary",   0,       0,       0,       4
)

gender_2025_wide <- gender_2025 %>%
  pivot_wider(names_from = Year, values_from = n) %>%
  rename(`2025` = `2025`)  # rename to match the year style

gender_final <- historic %>%
  left_join(gender_2025_wide, by = "Gender") %>%
  mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE))

totals_row <- gender_final %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Gender = "Total") %>%
  select(Gender, everything())

gender_table_full <- bind_rows(gender_final, totals_row)

gender_table_full %>%
  kable(caption = "Respondents: Gender and Year of Study", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(nrow(gender_table_full), bold = TRUE, background = "#e6f2f5") %>%
  column_spec(1, bold = TRUE)

location_table <- crew_data %>%
  filter(!is.na(gender_clean), !is.na(in_which_country_are_you_located)) %>%
  count(Country = in_which_country_are_you_located, Gender = gender_clean) %>%
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

location_total <- location_table %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Country = "TOTAL") %>%
  select(Country, everything())

location_full <- bind_rows(location_table, location_total)

location_full %>%
  kable(caption = "Respondents by Location and Gender", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(nrow(location_full), bold = TRUE, background = "#e6f2f5") %>%
  column_spec(1, bold = TRUE)

# crew_data %>% people who responded gender but not country
#   filter(!is.na(gender_clean), is.na(in_which_country_are_you_located)) %>%
#   select(response_id, gender_clean)

crew_data %>%
  filter(!is.na(i_identify_my_ethnicity_as)) %>%
  distinct(i_identify_my_ethnicity_as) %>%
  arrange(i_identify_my_ethnicity_as)

ethnicity_table <- crew_data %>%
  filter(!is.na(gender_clean), !is.na(i_identify_my_ethnicity_as)) %>%
  count(Ethnicity = i_identify_my_ethnicity_as, Gender = gender_clean) %>%
  pivot_wider(names_from = Gender, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

ethnicity_total <- ethnicity_table %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Ethnicity = "Grand Total") %>%
  select(Ethnicity, everything())

ethnicity_full <- bind_rows(ethnicity_table, ethnicity_total)

# Show table
ethnicity_full %>%
  kable(caption = "Respondents: Ethnicity/Race/Origin", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(nrow(ethnicity_full), bold = TRUE, background = "#e6f2f5") %>%
  column_spec(1, bold = TRUE)


# crew_data %>%
#   filter(!is.na(what_is_the_highest_level_of_education_that_you_have_completed
# )) %>%
#   distinct(what_is_the_highest_level_of_education_that_you_have_completed
# ) %>%
#   arrange(what_is_the_highest_level_of_education_that_you_have_completed
# )


# Step 1: Filter to valid gender + age responses
age_data <- crew_data %>%
  filter(gender_clean %in% c("Women", "Men"),
         !is.na(which_of_the_following_ranges_best_describes_your_age_group)) %>%
  count(Gender = gender_clean,
        AgeGroup = which_of_the_following_ranges_best_describes_your_age_group) %>%
  group_by(Gender) %>%
  mutate(Percent = n / sum(n) * 100)

# Step 2: Standardize order of age groups (optional but highly recommended)
age_order <- c(
  "Under 25", "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65 or older"
)

age_data <- age_data %>%
  mutate(AgeGroup = factor(AgeGroup, levels = age_order))

# Step 3: Plot with ggplot2
ggplot(age_data, aes(x = AgeGroup, y = Percent, fill = Gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 17)) +
  scale_fill_manual(values = c("Women" = "#d94f70", "Men" = "#1f4068")) +
  labs(
    title = "Respondents: Age",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

names(crew_data)[9]
names(crew_data)[10]
names(crew_data)[139]
names(crew_data)[120:135]
names(crew_data)[197:211]

write_csv(
  tibble(column_names = names(crew_data)),
  "crew_column_names.csv"
)

# Step 1: Rename for clarity (you can keep this or skip)
crew_data <- crew_data %>%
  rename(
    `2010` = crew_network_conducts_this_benchmarking_study_every_5_years_please_indicate_below_if_you_took_the_benchmarking_survey_in_prior_years_i_took_this_survey_in_2010,
    `2015` = crew_network_conducts_this_benchmarking_study_every_5_years_please_indicate_below_if_you_took_the_benchmarking_survey_in_prior_years_i_took_this_survey_in_2015,
    `2020` = crew_network_conducts_this_benchmarking_study_every_5_years_please_indicate_below_if_you_took_the_benchmarking_survey_in_prior_years_i_took_this_survey_in_2020,
    `Never` = crew_network_conducts_this_benchmarking_study_every_5_years_please_indicate_below_if_you_took_the_benchmarking_survey_in_prior_years_i_have_not_taken_this_survey_before,
    `Unsure` = crew_network_conducts_this_benchmarking_study_every_5_years_please_indicate_below_if_you_took_the_benchmarking_survey_in_prior_years_i_have_taken_it_before_but_i_am_not_sure_when
  )

# Step 2: Reshape to long format
survey_long <- crew_data %>%
  filter(gender_clean %in% c("Women", "Men")) %>%
  pivot_longer(cols = c(`2010`, `2015`, `2020`, `Never`, `Unsure`),
               names_to = "Survey_Year", values_to = "Response") %>%
  filter(Response == "Yes") %>%
  count(Survey_Year, Gender = gender_clean) %>%
  group_by(Gender) %>%
  mutate(Percent = n / sum(n) * 100)

# Step 3: Set order for better visuals
survey_long <- survey_long %>%
  mutate(Survey_Year = factor(Survey_Year, levels = c("2010", "2015", "2020", "Unsure", "Never")))

# Step 4: Plot horizontal bar chart
ggplot(survey_long, aes(x = Percent, y = Survey_Year, fill = Gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.0f%%", Percent)),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Women" = "#d94f70", "Men" = "#1f4068")) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, max(survey_long$Percent) + 5)) +
  labs(
    title = "Prior Participation in Benchmarking Surveys",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )


# Step 1: Recode based on your exact responses
edu_data <- crew_data %>%
  filter(gender_clean %in% c("Women", "Men"),
         !is.na(what_is_the_highest_level_of_education_that_you_have_completed)) %>%
  mutate(Education = case_when(
    what_is_the_highest_level_of_education_that_you_have_completed %in% c(
      "Graduated high school (grade 12)",
      "Some high school or less (grades 0-11)"
    ) ~ "Graduated high school",
    what_is_the_highest_level_of_education_that_you_have_completed == "Technical or trade school" ~ "Technical or trade school",
    what_is_the_highest_level_of_education_that_you_have_completed == "College/university" ~ "Graduated college",
    what_is_the_highest_level_of_education_that_you_have_completed == "Some post-graduate study" ~ "Some post-graduate",
    what_is_the_highest_level_of_education_that_you_have_completed == "Post-graduate degree" ~ "Post-graduate degree",
    TRUE ~ NA_character_
  ))

# Step 2: Count and calculate percentages
edu_summary <- edu_data %>%
  count(Education, Gender = gender_clean) %>%
  group_by(Gender) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  ungroup()

# Step 3: Ensure consistent order for chart
edu_levels <- c(
  "Graduated high school",
  "Technical or trade school",
  "Some college",  # Note: Not present in your dataset, so won't appear
  "Graduated college",
  "Some post-graduate",
  "Post-graduate degree"
)

edu_summary <- edu_summary %>%
  mutate(Education = factor(Education, levels = edu_levels))

# Step 4: Create the horizontal bar chart
ggplot(edu_summary, aes(x = Percent, y = Education, fill = Gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.0f%%", Percent)),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Women" = "#d94f70", "Men" = "#1f4068")) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 60)) +
  labs(
    title = "Education (2025)",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# edu_summary %>%
#   group_by(Gender) %>%
#   summarise(Total = sum(Percent))
# edu_summary %>%
#   arrange(Education, Gender)

  