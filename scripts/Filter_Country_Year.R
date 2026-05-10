############################################################
# FILTER POOLED DHS DATA TO ONE COUNTRY-YEAR
############################################################

############################################################
# 0. LOAD LIBRARIES
############################################################
library(haven)
library(dplyr)

############################################################
# 1. USER SETTINGS
############################################################

# Pooled DHS extract
pooled_file <- "/Users/serafeimkaznessis/Downloads/idhs_00004.dta"

# Country code
# Examples:
# Nigeria = 566
# Uganda = 800
# Zambia = 894
target_country <- 566

# Survey year
target_year <- 2008

# Output path for filtered dataset
output_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"

############################################################
# 2. LOAD DATA
############################################################
data <- read_dta(pooled_file)

############################################################
# 3. INSPECT AVAILABLE VARIABLES
############################################################
cat("Potential country/year variables:\n")
print(names(data)[grepl("country|year|survey|sample", names(data), ignore.case = TRUE)])

cat("\nCountry distribution:\n")
print(table(data$country, useNA = "ifany"))

############################################################
# 4. FILTER TO TARGET COUNTRY
############################################################
data_country <- data %>%
  filter(country == target_country)

cat("\nAvailable years in selected country:\n")
print(table(data_country$year, useNA = "ifany"))

############################################################
# 5. FILTER TO TARGET YEAR
############################################################
data_country_year <- data_country %>%
  filter(year == target_year)

############################################################
# 6. SANITY CHECKS
############################################################
cat("\nRows in filtered dataset:", nrow(data_country_year), "\n")

cat("\nCountry check:\n")
print(table(data_country_year$country, useNA = "ifany"))

cat("\nYear check:\n")
print(table(data_country_year$year, useNA = "ifany"))

############################################################
# 7. SAVE FILTERED DATASET
############################################################
write_dta(data_country_year, output_file)

cat("\nSaved filtered dataset to:\n", output_file, "\n")

