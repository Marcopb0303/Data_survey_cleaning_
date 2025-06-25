# ====================================================================
# DATA EXPLORATION: UNIQUE VALUES CHECK FOR ALL PLOTTING VARIABLES
# ====================================================================

# Function to safely check unique values and handle potential issues
check_unique_values <- function(data, column_name, max_display = 20) {
  cat("\n=== COLUMN:", column_name, "===\n")
  
  if (!column_name %in% names(data)) {
    cat("❌ COLUMN NOT FOUND IN DATASET\n")
    return(NULL)
  }
  
  values <- data[[column_name]]
  unique_vals <- unique(values)
  
  cat("Total observations:", length(values), "\n")
  cat("Non-missing values:", sum(!is.na(values)), "\n") 
  cat("Missing values:", sum(is.na(values)), "\n")
  cat("Unique values count:", length(unique_vals), "\n")
  
  if (length(unique_vals) <= max_display) {
    cat("All unique values:\n")
    print(sort(unique_vals, na.last = TRUE))
  } else {
    cat("First", max_display, "unique values:\n")
    print(sort(unique_vals, na.last = TRUE)[1:max_display])
    cat("... and", length(unique_vals) - max_display, "more values\n")
  }
  
  # Check for TRUE/FALSE if it looks like a checkbox column
  if (any(values %in% c("TRUE", "FALSE", TRUE, FALSE), na.rm = TRUE)) {
    cat("TRUE/FALSE distribution:\n")
    print(table(values, useNA = "ifany"))
  }
  
  cat("----------------------------------------\n")
  return(unique_vals)
}

# ====================================================================
# 1. BASIC DEMOGRAPHICS & SAMPLE COMPOSITION
# ====================================================================

cat("🔍 BASIC DEMOGRAPHICS & SAMPLE COMPOSITION\n")
cat("====================================================================\n")

crew_data <- readRDS("crew_dataset_keys_analysis_ready.rds")

# Gender
check_unique_values(crew_data, "m27")

# Age  
check_unique_values(crew_data, "m25")

# Location
check_unique_values(crew_data, "WHEREFROM")

# Ethnicity variables
check_unique_values(crew_data, "G20Q70")
check_unique_values(crew_data, "G20Q69") 
check_unique_values(crew_data, "G20Q68")

# Education
check_unique_values(crew_data, "m28A")

# Years of experience
check_unique_values(crew_data, "m4A")

# Previous survey participation
check_unique_values(crew_data, "i0[2]")  # 2010
check_unique_values(crew_data, "i0[3]")  # 2015  
check_unique_values(crew_data, "i0[6]")  # 2020
check_unique_values(crew_data, "i0[4]")  # Never
check_unique_values(crew_data, "i0[5]")  # Unsure

# ====================================================================
# 2. INDUSTRY & ROLE STRUCTURE
# ====================================================================

cat("\n🏢 INDUSTRY & ROLE STRUCTURE\n")
cat("====================================================================\n")

# Company primary industry
check_unique_values(crew_data, "m1A")
check_unique_values(crew_data, "m1A[other]")

# Specialization
check_unique_values(crew_data, "m1B")
check_unique_values(crew_data, "m1B[other]")

# Current position
check_unique_values(crew_data, "m3A")

# Employment status
check_unique_values(crew_data, "i2")

# Asset classes (all checkbox variables)
cat("\n📊 ASSET CLASSES:\n")
check_unique_values(crew_data, "m2B[SQ001]")  # Office
check_unique_values(crew_data, "m2B[SQ002]")  # Industrial
check_unique_values(crew_data, "m2B[SQ003]")  # Retail
check_unique_values(crew_data, "m2B[SQ004]")  # Multifamily
check_unique_values(crew_data, "m2B[SQ005]")  # Hotel/Motel
check_unique_values(crew_data, "m2B[SQ006]")  # Mixed-use
check_unique_values(crew_data, "m2B[SQ007]")  # Healthcare
check_unique_values(crew_data, "m2B[SQ008]")  # Education
check_unique_values(crew_data, "m2B[SQ009]")  # Public Sector

# Company size and structure
check_unique_values(crew_data, "m8")   # Employees at location
check_unique_values(crew_data, "m29")  # Annual revenue
check_unique_values(crew_data, "m30A") # Company type
check_unique_values(crew_data, "m30B") # Ownership structure

# ====================================================================
# 3. WORKFORCE COMPOSITION
# ====================================================================

cat("\n👥 WORKFORCE COMPOSITION\n") 
cat("====================================================================\n")

# Number of CRE professionals
check_unique_values(crew_data, "m9A")

# Gender percentages at workplace
check_unique_values(crew_data, "m9C[SQ001]")  # Male %
check_unique_values(crew_data, "m9C[SQ002]")  # Female %
check_unique_values(crew_data, "m9C[SQ003]")  # Nonbinary %

# Alternative gender counts
check_unique_values(crew_data, "G06Q77[SQ001]")  # Female count
check_unique_values(crew_data, "G06Q77[SQ002]")  # Male count
check_unique_values(crew_data, "G06Q77[SQ003]")  # Nonbinary count

# Specialization team size and gender
check_unique_values(crew_data, "m9E")  # Professionals in specialization

# Senior roles
check_unique_values(crew_data, "m10A")  # Senior level roles count

# ====================================================================
# 4. MANAGEMENT STRUCTURE  
# ====================================================================

cat("\n👔 MANAGEMENT STRUCTURE\n")
cat("====================================================================\n")

# Direct reports
check_unique_values(crew_data, "m11A")     # Has direct reports
check_unique_values(crew_data, "G10Q78")   # Number of reports

# Gender of direct reports percentages
check_unique_values(crew_data, "m11B[SQ001]")  # Male %
check_unique_values(crew_data, "m11B[SQ002]")  # Female %

# ====================================================================
# 5. WORK ARRANGEMENTS (2025 SPECIFIC)
# ====================================================================

cat("\n💼 WORK ARRANGEMENTS (2025 SPECIFIC)\n")
cat("====================================================================\n")

# Work hours
check_unique_values(crew_data, "HOURS[SQ001_SQ001]")  # Office hours
check_unique_values(crew_data, "HOURS[SQ002_SQ001]")  # Remote hours  
check_unique_values(crew_data, "HOURS[SQ003_SQ001]")  # Field hours

# ====================================================================
# 6. COVID QUESTIONS (2025 SPECIFIC)
# ====================================================================

cat("\n🦠 COVID IMPACT (2025 SPECIFIC)\n")
cat("====================================================================\n")

# All COVID impact questions
covid_cols <- c(
  "COVID1[SQ003]",  # Increased flexibility
  "COVID1[SQ004]",  # Reduced interactions
  "COVID1[SQ005]",  # Impeded advancement
  "COVID1[SQ006]",  # Harder due to turnover
  "COVID1[SQ007]",  # Opened opportunities
  "COVID1[SQ008]",  # Limited client access
  "COVID1[SQ009]",  # More productive
  "COVID1[SQ010]",  # More stress
  "COVID1[SQ011]"   # Other
)

for (col in covid_cols) {
  check_unique_values(crew_data, col)
}

# ====================================================================
# 7. COMPENSATION VARIABLES (for future weighted analysis)
# ====================================================================

cat("\n💰 COMPENSATION VARIABLES (Preview for weighted analysis)\n")
cat("====================================================================\n")

# Currency
check_unique_values(crew_data, "CUR")

# 2024 compensation components
check_unique_values(crew_data, "m21A[1_SQ001]")  # Base salary
check_unique_values(crew_data, "m21A[2_SQ001]")  # Bonus
check_unique_values(crew_data, "m21A[3_SQ001]")  # Commission
check_unique_values(crew_data, "m21A[4_SQ001]")  # Profit sharing
check_unique_values(crew_data, "m21A[5_SQ001]")  # Long-term incentive

# Total compensation
check_unique_values(crew_data, "m28B")

# ====================================================================
# SUMMARY REPORT
# ====================================================================

cat("\n📋 SUMMARY REPORT\n")
cat("====================================================================\n")
cat("Data exploration completed!\n")
cat("Total columns checked:", length(names(crew_data)), "\n")
cat("Please review the output above to understand:\n")
cat("1. Which columns exist in your dataset\n") 
cat("2. What the actual values/labels are\n")
cat("3. Missing data patterns\n")
cat("4. Data types (TRUE/FALSE vs text vs numeric)\n")
cat("5. Need for data cleaning/recoding\n\n")

cat("Next steps:\n")
cat("- Review unexpected values or missing columns\n")
cat("- Update plotting code based on actual data values\n")
cat("- Handle missing data appropriately\n")
cat("- Proceed with corrected plotting code\n")