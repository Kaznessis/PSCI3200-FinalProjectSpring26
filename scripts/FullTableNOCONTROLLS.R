## Nigeria Analysis: Descriptive Statistics Table
## Selected Healthcare Barrier Variables Only

############################################################
# 0. LOAD LIBRARIES
############################################################
library(haven)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

############################################################
# 1. USER SETTINGS: CHANGE THESE
############################################################

# Filtered country-year DHS extract created from your prep script
dhs_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"

# DHS GPS shapefile (.shp)
gps_file <- "/Users/serafeimkaznessis/Downloads/CMGE42FL/CMGE42FL.shp"

# Murdock ethnic shapefile
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"

# Nunn slave exports file
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

# Country to analyze
country_name <- "Cameroon"

# Minimum number of women per cluster
min_cluster_n <- 5

# Selected healthcare access barrier variables
barrier_vars <- c(
  "bhcatthw",
  "bhcnodrug",
  "bhcnoprov",
  "bhcnofemdr",
  "bhcdistance",
  "bhcpermit",
  "bhctaketran",
  "bhcalone"
)

# Variable descriptions for output table
var_descriptions <- data.frame(
  Variable = c(
    "ln_total_exports",
    "bhcatthw",
    "bhcnodrug",
    "bhcnoprov",
    "bhcnofemdr",
    "bhcdistance",
    "bhcpermit",
    "bhctaketran",
    "bhcalone"
  ),
  Description = c(
    "Logged historical slave exports by ethnic homeland",
    "Bad attitude of healthcare worker barrier",
    "Lack of medicine barrier",
    "Lack of provider barrier",
    "Lack of female doctor barrier",
    "Distance barrier to healthcare",
    "Permission barrier",
    "Transportation barrier",
    "Not wanting to go alone barrier"
  )
)

# Output folder
output_dir <- "/Users/serafeimkaznessis/Desktop/PSCI3200-FinalProjectSpring26/outputs/tables/CountrySpecific"

############################################################
# 2. LOAD DATA
############################################################
data <- read_dta(dhs_file)
gps_sf <- st_read(gps_file, quiet = TRUE)
ethnic <- st_read(ethnic_file, quiet = TRUE)
nunn <- read_dta(nunn_file)

############################################################
# 3. CHECK THAT VARIABLES EXIST
############################################################

missing_vars <- barrier_vars[!barrier_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop(
    paste(
      "These variables are missing from the dataset:",
      paste(missing_vars, collapse = ", ")
    )
  )
}

############################################################
# 4. INSPECT VARIABLE CODING FIRST
############################################################

for (analysis_var in barrier_vars) {
  cat("\n############################################################\n")
  cat("Inspecting variable:", analysis_var, "\n")
  cat("############################################################\n")
  
  print(table(data[[analysis_var]], useNA = "ifany"))
  print(attr(data[[analysis_var]], "labels"))
}

############################################################
# 5. BASIC CLEANING AND RECODING
############################################################

# This uses the same recoding as the single-variable analysis:
# 20 = barrier present
# 10, 11, 12 = barrier absent
# 98, 99 = missing
data_clean <- data %>%
  mutate(
    wt = perweight / 1000000
  )

for (analysis_var in barrier_vars) {
  data_clean[[paste0(analysis_var, "_outcome")]] <- case_when(
    data_clean[[analysis_var]] == 20 ~ 1,
    data_clean[[analysis_var]] %in% c(10, 11, 12) ~ 0,
    data_clean[[analysis_var]] %in% c(98, 99) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

############################################################
# 6. CLUSTER-LEVEL AGGREGATION
############################################################

cluster_data <- data_clean %>%
  group_by(clusterno) %>%
  summarize(
    bhcatthw = weighted.mean(bhcatthw_outcome, wt, na.rm = TRUE),
    bhcnodrug = weighted.mean(bhcnodrug_outcome, wt, na.rm = TRUE),
    bhcnoprov = weighted.mean(bhcnoprov_outcome, wt, na.rm = TRUE),
    bhcnofemdr = weighted.mean(bhcnofemdr_outcome, wt, na.rm = TRUE),
    bhcdistance = weighted.mean(bhcdistance_outcome, wt, na.rm = TRUE),
    bhcpermit = weighted.mean(bhcpermit_outcome, wt, na.rm = TRUE),
    bhctaketran = weighted.mean(bhctaketran_outcome, wt, na.rm = TRUE),
    bhcalone = weighted.mean(bhcalone_outcome, wt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_cluster_n)


############################################################
# 7. PREP GPS DATA
############################################################

gps_df <- gps_sf %>%
  st_drop_geometry() %>%
  select(DHSCLUST, LATNUM, LONGNUM)

############################################################
# 8. MERGE CLUSTER DATA WITH GPS
############################################################

merged <- cluster_data %>%
  left_join(gps_df, by = c("clusterno" = "DHSCLUST")) %>%
  filter(!is.na(LATNUM), !is.na(LONGNUM))

############################################################
# 9. CONVERT CLUSTERS TO SPATIAL POINTS
############################################################

cluster_sf <- st_as_sf(
  merged,
  coords = c("LONGNUM", "LATNUM"),
  crs = 4326
)

############################################################
# 10. LOAD COUNTRY BOUNDARY AND CLIP ETHNIC POLYGONS
############################################################

country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == country_name)

ethnic <- st_transform(ethnic, 4326)
country_sf <- st_transform(country_sf, 4326)

ethnic_country <- st_intersection(ethnic, country_sf)

############################################################
# 11. SPATIAL JOIN: CLUSTERS -> ETHNIC HOMELANDS
############################################################

cluster_joined <- st_join(cluster_sf, ethnic_country)

print(names(ethnic_country))

############################################################
# 12. AGGREGATE TO ETHNIC LEVEL
############################################################

ethnic_summary <- cluster_joined %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarize(
    bhcatthw = weighted.mean(bhcatthw, w = n, na.rm = TRUE),
    bhcnodrug = weighted.mean(bhcnodrug, w = n, na.rm = TRUE),
    bhcnoprov = weighted.mean(bhcnoprov, w = n, na.rm = TRUE),
    bhcnofemdr = weighted.mean(bhcnofemdr, w = n, na.rm = TRUE),
    bhcdistance = weighted.mean(bhcdistance, w = n, na.rm = TRUE),
    bhcpermit = weighted.mean(bhcpermit, w = n, na.rm = TRUE),
    bhctaketran = weighted.mean(bhctaketran, w = n, na.rm = TRUE),
    bhcalone = weighted.mean(bhcalone, w = n, na.rm = TRUE),
    n_clusters = dplyr::n(),
    total_women = sum(n, na.rm = TRUE),
    .groups = "drop"
  )



############################################################
# 13. MERGE ETHNIC-LEVEL OUTCOMES BACK TO POLYGONS
############################################################

ethnic_country <- ethnic_country %>%
  left_join(ethnic_summary, by = "NAME")

############################################################
# 14. PREP NUNN EXTRACTION DATA
############################################################

nunn <- nunn %>%
  mutate(
    total_exports = atlantic_all_years + indian_all_years,
    ln_total_exports = log1p(total_exports)
  )

nunn_ethnic <- nunn %>%
  select(murdock_name, total_exports, ln_total_exports) %>%
  distinct()

ethnic_country <- ethnic_country %>%
  mutate(NAME = str_to_upper(str_trim(NAME)))

nunn_ethnic <- nunn_ethnic %>%
  mutate(murdock_name = str_to_upper(str_trim(murdock_name)))

############################################################
# 15. MERGE EXTRACTION DATA ONTO ETHNIC POLYGONS
############################################################

ethnic_country <- ethnic_country %>%
  select(-matches("^total_exports|^ln_total_exports")) %>%
  left_join(nunn_ethnic, by = c("NAME" = "murdock_name"))

cat(
  "Matched ethnic groups with extraction data:",
  sum(!is.na(ethnic_country$ln_total_exports)),
  "\n"
)

############################################################
# 16. BUILD ETHNIC-HOMELAND ANALYSIS DATASET
############################################################

analysis_data <- ethnic_country %>%
  st_drop_geometry() %>%
  select(
    NAME,
    ln_total_exports,
    all_of(barrier_vars),
    n_clusters,
    total_women
  )

############################################################
# 17. CREATE DESCRIPTIVE STATISTICS TABLE
############################################################

summary_vars <- c(
  "ln_total_exports",
  barrier_vars
)

descriptive_table <- lapply(summary_vars, function(var) {
  x <- analysis_data[[var]]
  
  data.frame(
    Variable = var,
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}) %>%
  bind_rows() %>%
  left_join(var_descriptions, by = "Variable") %>%
  select(Variable, Description, Mean, SD, Min, Max)

############################################################
# 18. PRINT DESCRIPTIVE STATISTICS TABLE
############################################################

print(descriptive_table)

############################################################
# 19. SAVE DESCRIPTIVE STATISTICS TABLE
############################################################

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file <- paste0(
  output_dir,
  "/",
  country_name,
  "_selected_barriers_descriptive_statistics_table.csv"
)

write.csv(
  descriptive_table,
  output_file,
  row.names = FALSE
)

cat("Saved descriptive statistics table to:", output_file, "\n")


