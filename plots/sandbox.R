library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)
library(dplyr)
library(stringr)


crew_data <- readRDS("crew_dataset_keys_analysis_ready.rds")
# Clean gender column to include Nonbinary and NA
crew_data <- crew_data %>%
  mutate(
    gender_clean = case_when(
      str_to_lower(m27) %in% c("female", "woman") ~ "Women",
      str_to_lower(m27) %in% c("male", "man") ~ "Men",
      str_detect(str_to_lower(m27), "nonbinary|variant") ~ "Nonbinary",
      TRUE ~ NA_character_
    )
  )
# Total number of women and men respondents (who answered any asset class question)
total_women <- sum(crew_data$gender_clean == "Women", na.rm = TRUE)
total_men   <- sum(crew_data$gender_clean == "Men", na.rm = TRUE)

calculate_asset_stats <- function(column_name) {
  total_count <- sum(crew_data[[column_name]] == TRUE, na.rm = TRUE)
  asset_respondents <- crew_data %>% filter(!!sym(column_name) == TRUE)
  
  women_count <- sum(asset_respondents$gender_clean == "Women", na.rm = TRUE)
  men_count <- sum(asset_respondents$gender_clean == "Men", na.rm = TRUE)
  other_count <- total_count - women_count - men_count
  
  women_pct <- if (total_count > 0) round((women_count / total_count) * 100, 1) else 0
  men_pct <- if (total_count > 0) round((men_count / total_count) * 100, 1) else 0
  other_pct <- if (total_count > 0) round((other_count / total_count) * 100, 1) else 0
  
  return(list(
    total_count = total_count,
    women_count = women_count,
    men_count = men_count,
    other_count = other_count,
    women_pct = women_pct,
    men_pct = men_pct,
    other_pct = other_pct
  ))
}

# Apply function to asset classes
office_stats <- calculate_asset_stats("m2B[SQ001]")
multifamily_stats <- calculate_asset_stats("m2B[SQ004]") 
retail_stats <- calculate_asset_stats("m2B[SQ003]")
industrial_stats <- calculate_asset_stats("m2B[SQ002]")
mixed_stats <- calculate_asset_stats("m2B[SQ006]")
healthcare_stats <- calculate_asset_stats("m2B[SQ007]")
hotel_stats <- calculate_asset_stats("m2B[SQ005]")
education_stats <- calculate_asset_stats("m2B[SQ008]")
public_stats <- calculate_asset_stats("m2B[SQ009]")

# Build the summary table
asset_detailed <- data.frame(
  Asset_Class = c("Office", "Multifamily", "Retail", "Industrial/Manufacturing", 
                  "Mixed-Use", "Healthcare", "Hotel/Motel", "Education", "Public Sector"),
  Total_Respondents = c(office_stats$total_count, multifamily_stats$total_count, 
                        retail_stats$total_count, industrial_stats$total_count,
                        mixed_stats$total_count, healthcare_stats$total_count,
                        hotel_stats$total_count, education_stats$total_count, 
                        public_stats$total_count),
  Women_Count = c(office_stats$women_count, multifamily_stats$women_count,
                  retail_stats$women_count, industrial_stats$women_count,
                  mixed_stats$women_count, healthcare_stats$women_count,
                  hotel_stats$women_count, education_stats$women_count,
                  public_stats$women_count),
  Women_Percent = c(office_stats$women_pct, multifamily_stats$women_pct,
                    retail_stats$women_pct, industrial_stats$women_pct,
                    mixed_stats$women_pct, healthcare_stats$women_pct,
                    hotel_stats$women_pct, education_stats$women_pct,
                    public_stats$women_pct),
  Men_Count = c(office_stats$men_count, multifamily_stats$men_count,
                retail_stats$men_count, industrial_stats$men_count,
                mixed_stats$men_count, healthcare_stats$men_count,
                hotel_stats$men_count, education_stats$men_count,
                public_stats$men_count),
  Men_Percent = c(office_stats$men_pct, multifamily_stats$men_pct,
                  retail_stats$men_pct, industrial_stats$men_pct,
                  mixed_stats$men_pct, healthcare_stats$men_pct,
                  hotel_stats$men_pct, education_stats$men_pct,
                  public_stats$men_pct),
  Other_Count = c(office_stats$other_count, multifamily_stats$other_count,
                  retail_stats$other_count, industrial_stats$other_count,
                  mixed_stats$other_count, healthcare_stats$other_count,
                  hotel_stats$other_count, education_stats$other_count,
                  public_stats$other_count),
  Other_Percent = c(office_stats$other_pct, multifamily_stats$other_pct,
                    retail_stats$other_pct, industrial_stats$other_pct,
                    mixed_stats$other_pct, healthcare_stats$other_pct,
                    hotel_stats$other_pct, education_stats$other_pct,
                    public_stats$other_pct)
) %>%
  arrange(desc(Total_Respondents)) %>%
  mutate(
    Women_Pref = round((Women_Count / total_women) * 100, 1),
    Men_Pref = round((Men_Count / total_men) * 100, 1),
    Women_Percent = paste0(Women_Percent, "%"),
    Men_Percent = paste0(Men_Percent, "%"),
    Other_Percent = paste0(Other_Percent, "%"),
    Women_Pref = paste0(Women_Pref, "%"),
    Men_Pref = paste0(Men_Pref, "%")
  )

# Render the table
asset_detailed %>%
  kable(
    caption = "Respondents: Asset Class Distribution with Gender Breakdown and Preference",
    col.names = c("Asset Class", "Total", "Women", "% Women", "Men", "% Men", "Other/NA", "% Other/NA", "% of All Women", "% of All Men"),
    align = "lcccccccccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = TRUE
  ) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 2, "Women" = 2, "Men" = 2, "Other/NA" = 2, "Asset Preference" = 2)) %>%
  add_footnote("Note: Preference columns show percent of all women/men who selected each asset class", notation = "none")


crew_data_spec <- crew_data %>%
  filter(!is.na(gender_clean)) %>%
  mutate(
    main_category = case_when(
      m1B %in% c("Asset/Property Management", "Corporate Real Estate", "Portfolio Management") ~ "Asset_Property_Facilities",
      m1B %in% c("Brokerage/Sales/Leasing") ~ "Brokerage_Sales_Leasing",
      m1B %in% c("Acquisitions/ Dispositions", "Architecture and Design", "Construction", 
                 "Development", "Economic Development", "Engineering", "Environmental",
                 "Interior Design", "Investments") ~ "Development_Services",
      m1B %in% c("Accounting", "Appraisal/Valuation", "Consulting", "Executive Management",
                 "Finance/Lending/Mortgage", "Human Resources", "Law", 
                 "Marketing/Business Development", "Sustainability", "Title/Escrow") ~ "Financial_Professional",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(main_category))

main_category_stats <- crew_data_spec %>%
  count(main_category, gender_clean) %>%
  pivot_wider(names_from = gender_clean, values_from = n, values_fill = 0) %>%
  mutate(
    Total = Women + Men + ifelse("Nonbinary" %in% names(.), Nonbinary, 0),
    Women_Percent = round((Women / Total) * 100, 1),
    Men_Percent = round((Men / Total) * 100, 1)
  ) %>%
  arrange(desc(Total))

total_women <- sum(crew_data_spec$gender_clean == "Women", na.rm = TRUE)
total_men   <- sum(crew_data_spec$gender_clean == "Men", na.rm = TRUE)
specialization_summary <- main_category_stats %>%
  mutate(
    Category = case_when(
      main_category == "Asset_Property_Facilities" ~ "Asset • Property • Facilities Management",
      main_category == "Brokerage_Sales_Leasing" ~ "Brokerage • Sales • Leasing", 
      main_category == "Development_Services" ~ "Development • Development Services",
      main_category == "Financial_Professional" ~ "Financial • Professional Services",
      main_category == "Other" ~ "Other",
      TRUE ~ main_category
    ),
    Women_Percent_Display = paste0(Women_Percent, "%"),
    Men_Percent_Display = paste0(Men_Percent, "%"),
    Women_Pref = paste0(round((Women / total_women) * 100, 1), "%"),
    Men_Pref = paste0(round((Men / total_men) * 100, 1), "%")
  ) %>%
  select(Category, Women, Women_Percent_Display, Women_Pref,
         Men, Men_Percent_Display, Men_Pref, Total)%>%
  arrange(desc(Total))

specialization_summary %>%
  kable(
    caption = "Specialization by Respondents with Gender Preference",
    col.names = c("Specialization", "Women", "% of Category", "% of All Women", 
                  "Men", "% of Category", "% of All Men", "Total"),
    align = "lccccccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  column_spec(1, bold = TRUE, width = "4cm") %>%
  row_spec(0, bold = TRUE, background = "#f8f9fa") %>%
  add_footnote("‘% of Category’ is the gender breakdown within the specialization; ‘% of All’ shows how many women/men chose that category.", notation = "none")


workplace_gender_detailed <- crew_data %>%
  filter(!is.na(`m9G[SQ002]`), !is.na(gender_clean), 
         gender_clean %in% c("Women", "Men")) %>%
  mutate(
    female_range = case_when(
      `m9G[SQ002]` >= 0 & `m9G[SQ002]` < 25 ~ "< 25% women",
      `m9G[SQ002]` >= 25 & `m9G[SQ002]` < 50 ~ "25-49% women", 
      `m9G[SQ002]` >= 50 & `m9G[SQ002]` < 75 ~ "50-74% women",
      `m9G[SQ002]` >= 75 ~ "75%+ women",
      TRUE ~ "Unknown"
    ),
    female_range = factor(female_range, levels = c("< 25% women", "25-49% women", 
                                                   "50-74% women", "75%+ women"))
  ) %>%
  count(female_range, Gender = gender_clean) %>%
  group_by(female_range) %>%
  mutate(
    Total = sum(n),
    Percent_within_range = n / Total * 100
  ) %>%
  ungroup()

ggplot(workplace_gender_detailed, aes(x = reorder(female_range, desc(Total)), y = n, fill = Gender)) +
  geom_col(position = "stack", alpha = 0.9) +
  geom_text(data = workplace_gender_detailed %>% 
              group_by(female_range) %>% 
              summarise(Total = sum(n), .groups = 'drop'),
            aes(x = female_range, y = Total, 
                label = paste0("n=", Total)),
            inherit.aes = FALSE, hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Women" = "#d94f70", "Men" = "#1f4068")) +
  coord_flip() +
  labs(
    title = "Specialization Gender Distribution (As Estimated by Respondents)",
    subtitle = paste0("Total respondents: ", sum(workplace_gender_detailed$n)),
    x = "Estimated Women Representation at your Specialization", 
    y = "Number of Respondents",
    fill = "Respondent Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    panel.grid.major.y = element_blank()
  )