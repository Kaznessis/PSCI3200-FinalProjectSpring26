############################################################
# INSPECT ONE VARIABLE IN A FILTERED DHS .DTA FILE
############################################################

############################################################
# 0. LOAD LIBRARIES
############################################################
library(haven)
library(dplyr)

############################################################
# 1. USER SETTINGS
############################################################
dta_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"
analysis_var <- "bhcdistance"

############################################################
# 2. LOAD DATA
############################################################
data <- read_dta(dta_file)

############################################################
# 3. CHECK WHETHER VARIABLE EXISTS
############################################################
cat("Variable exists:", analysis_var %in% names(data), "\n")

if (!(analysis_var %in% names(data))) {
  stop("Variable not found in dataset.")
}

############################################################
# 4. BASIC VARIABLE INFO
############################################################
cat("\nVariable name:\n")
print(analysis_var)

cat("\nClass / type:\n")
print(class(data[[analysis_var]]))
print(typeof(data[[analysis_var]]))

cat("\nLabels attribute:\n")
print(attr(data[[analysis_var]], "labels"))

############################################################
# 5. SPREAD / DISTRIBUTION
############################################################
cat("\nFrequency table:\n")
print(table(data[[analysis_var]], useNA = "ifany"))

cat("\nProportions:\n")
print(prop.table(table(data[[analysis_var]], useNA = "ifany")))

cat("\nSummary:\n")
print(summary(data[[analysis_var]]))

cat("\nNon-missing count:\n")
print(sum(!is.na(data[[analysis_var]])))

cat("\nMissing count:\n")
print(sum(is.na(data[[analysis_var]])))

############################################################
# 6. OPTIONAL: SHOW FIRST FEW NON-MISSING VALUES
############################################################
cat("\nFirst non-missing values:\n")
print(head(data[[analysis_var]][!is.na(data[[analysis_var]])]))
