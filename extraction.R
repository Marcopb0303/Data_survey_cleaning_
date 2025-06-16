library(tidyverse)
survey_data <- read_csv("results-survey714832.csv")
names(survey_data)

install.packages("janitor")

library(janitor)

crew_data <- readRDS("crew_survey_cleaned.rds") %>%
  clean_names()
view(crew_data)
# write_csv(
#   tibble(column_names = names(survey_data)),
#   "crew_column_names.csv"
# )

names(crew_data)
# summary(crew_data)
# glimpse(crew_data)
# sort(colSums(is.na(crew_data)), decreasing = TRUE)[1:20]
# view (crew_data)
# View(as.data.frame(names(crew_data)))

crew_data <- readRDS("crew_survey_cleaned.rds")
# names(crew_data)[230]  # Gender
# names(crew_data)[42]   # Country
# names(crew_data)[225]  # Ethnicity

# Define reusable variables
gender <- crew_data$`what_is_your_gender_identity`
country <- crew_data$`in_which_country_are_you_located`
ethnicity <- crew_data$`choose_one_category_below_to_best_describe_your_ethnic_group_or_background`
gender_summary <- tibble(gender)
tibble(gender) %>%
  count(gender) %>%
  rename(Gender = gender, Count = n)

ggplot(gender_summary, aes(x = reorder(Gender, -Count), y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Respondents by Gender", x = "Gender", y = "Count") +
  theme_minimal()


crew_dataf <- readRDS("crew_survey_cleaned_fixed.rds")  %>%
  clean_names()
names(crew_dataf)



# ==============================================================================
# Gender Distribution Analysis
# ==============================================================================

# Count raw gender responses
gender_raw_counts <- crew_dataf %>%
  count(what_is_your_gender_identity, sort = TRUE) %>%
  mutate(
    percentage = round(n/sum(n)*100, 2),
    percentage_label = paste0(percentage, "%")
  )

cat("=== Raw Gender Identity Responses ===\n")
print(gender_raw_counts)

# Count using the cleaned gender variable (gender2) if it exists
if("gender2" %in% names(crew_dataf)) {
  gender_clean_counts <- crew_dataf %>%
    count(gender2, sort = TRUE) %>%
    mutate(
      percentage = round(n/sum(n)*100, 2),
      percentage_label = paste0(percentage, "%")
    )
  
  cat("\n=== Cleaned Gender Categories (gender2) ===\n")
  print(gender_clean_counts)
}

# Create a summary table
gender_summary <- crew_dataf %>%
  mutate(
    gender_category = case_when(
      str_detect(tolower(what_is_your_gender_identity), "female") ~ "Female",
      str_detect(tolower(what_is_your_gender_identity), "male") & 
        !str_detect(tolower(what_is_your_gender_identity), "female") ~ "Male",
      str_detect(tolower(what_is_your_gender_identity), "nonbinary|non-binary|gender variant") ~ "Non-Binary",
      is.na(what_is_your_gender_identity) | 
        what_is_your_gender_identity == "" | 
        str_detect(tolower(what_is_your_gender_identity), "decline|prefer not") ~ "Not Specified",
      TRUE ~ "Other"
    )
  ) %>%
  count(gender_category, sort = TRUE) %>%
  mutate(
    percentage = round(n/sum(n)*100, 2),
    percentage_label = paste0(percentage, "%")
  )

cat("\n=== Gender Distribution Summary ===\n")
print(gender_summary)

# Total count verification
total_responses <- nrow(crew_dataf)
total_with_gender <- sum(!is.na(crew_dataf$what_is_your_gender_identity) & 
                           crew_dataf$what_is_your_gender_identity != "")

cat("\n=== Summary Statistics ===\n")
cat("Total survey responses:", total_responses, "\n")
cat("Responses with gender data:", total_with_gender, "\n")
cat("Missing gender data:", total_responses - total_with_gender, "\n")
cat("Response rate for gender question:", round(total_with_gender/total_responses*100, 1), "%\n")

# Create a simple visualization summary
cat("\n=== Quick Visual Summary ===\n")
gender_summary %>%
  arrange(desc(n)) %>%
  mutate(
    bar = strrep("█", round(percentage/2)), # Create simple bar chart
    display = paste0(gender_category, ": ", n, " (", percentage_label, ") ", bar)
  ) %>%
  pull(display) %>%
  cat(sep = "\n")

write_csv(
  tibble(column_names = names(crew_dataf)),
  "crew_column_names.csv"
  )

# names(survey_data)
# 
# 
# unique_words <- unique(unlist(strsplit(tolower(paste(survey_data$"the_following_questions_relate_to_compensation_please_select_the_currency_you_will_be_using_to_answer_these_questions_must_be_the_same_for_all_questions_if_the_currency_in_which_you_are_paid_is_not_here_please_coose_one_of_the_currencies_below_and_provide_an_approximation_at_current_exchange_rates", collapse = " ")), "\\W+")))
# print(unique_words)

# edu_summary %>%
#   group_by(Gender) %>%
#   summarise(Total = sum(Percent))
# edu_summary %>%
#   arrange(Education, Gender)
