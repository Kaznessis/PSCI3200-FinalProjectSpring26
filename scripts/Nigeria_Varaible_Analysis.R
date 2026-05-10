############################################################
# NIGERIA: HEALTH BEHAVIOR ~ EXTRACTION + WEALTH + URBAN
############################################################

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
# 1. USER SETTINGS
############################################################
dhs_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"
gps_file <- "/Users/serafeimkaznessis/Downloads/NGGE8AFL/NGGE8AFL.shp"
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

country_name <- "Nigeria"
analysis_var <- "bhcatthw"
min_cluster_n <- 5

############################################################
# 2. LOAD DATA
############################################################
data <- read_dta(dhs_file)
gps_sf <- st_read(gps_file, quiet = TRUE)
ethnic <- st_read(ethnic_file, quiet = TRUE)
nunn <- read_dta(nunn_file)

############################################################
# 3. CHECK CODING
############################################################
print(table(data[[analysis_var]], useNA = "ifany"))
print(attr(data[[analysis_var]], "labels"))

############################################################
# 4. CLEAN AND RECODE OUTCOME
############################################################
data_clean <- data %>%
  mutate(
    wt = perweight / 1000000,
    var_num = as.numeric(.data[[analysis_var]]),
    outcome = case_when(
      var_num == 20 ~ 1,
      var_num %in% c(10, 11, 12) ~ 0,
      var_num %in% c(98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(outcome))

############################################################
# 5. BUILD CLUSTER-LEVEL DATA
############################################################
cluster_data <- data_clean %>%
  group_by(clusterno) %>%
  summarize(
    outcome_rate = weighted.mean(outcome, wt, na.rm = TRUE),
    urban = weighted.mean(urban == 1, wt, na.rm = TRUE),
    wealth = weighted.mean(wealthq, wt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_cluster_n)

############################################################
# 6. MERGE IN GPS COORDINATES
############################################################
gps_df <- gps_sf %>%
  st_drop_geometry() %>%
  select(DHSCLUST, LATNUM, LONGNUM)

merged <- cluster_data %>%
  left_join(gps_df, by = c("clusterno" = "DHSCLUST")) %>%
  filter(!is.na(LATNUM), !is.na(LONGNUM))

cluster_sf <- st_as_sf(
  merged,
  coords = c("LONGNUM", "LATNUM"),
  crs = 4326
)

############################################################
# 7. COUNTRY BOUNDARY + ETHNIC HOMELANDS
############################################################
country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == country_name) %>%
  st_transform(4326)

ethnic <- st_transform(ethnic, 4326)

ethnic_country <- st_intersection(ethnic, country_sf) %>%
  mutate(NAME = str_to_upper(str_trim(NAME)))

############################################################
# 8. JOIN CLUSTERS TO ETHNIC HOMELANDS
############################################################
cluster_joined <- st_join(cluster_sf, ethnic_country)

############################################################
# 9. AGGREGATE TO ETHNIC LEVEL
############################################################
ethnic_summary <- cluster_joined %>%
  st_drop_geometry() %>%
  filter(!is.na(NAME)) %>%
  group_by(NAME) %>%
  summarize(
    mean_outcome = weighted.mean(outcome_rate, w = n, na.rm = TRUE),
    mean_urban = weighted.mean(urban, w = n, na.rm = TRUE),
    mean_wealth = weighted.mean(wealth, w = n, na.rm = TRUE),
    n_clusters = dplyr::n(),
    total_women = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
# 10. PREP EXTRACTION DATA
############################################################
nunn_ethnic <- nunn %>%
  mutate(
    total_exports = atlantic_all_years + indian_all_years,
    ln_total_exports = log1p(total_exports),
    murdock_name = str_to_upper(str_trim(murdock_name))
  ) %>%
  select(murdock_name, total_exports, ln_total_exports) %>%
  distinct()

############################################################
# 11. BUILD REGRESSION DATASET
############################################################
plot_data <- ethnic_summary %>%
  mutate(NAME = str_to_upper(str_trim(NAME))) %>%
  left_join(nunn_ethnic, by = c("NAME" = "murdock_name")) %>%
  filter(
    !is.na(mean_outcome),
    !is.na(mean_urban),
    !is.na(mean_wealth),
    !is.na(ln_total_exports)
  )

cat("Rows in regression dataset:", nrow(plot_data), "\n")

############################################################
# 12. RUN ONLY THE CONTROLLED MODEL
############################################################
model_controls <- lm(
  mean_outcome ~ ln_total_exports + mean_urban + mean_wealth,
  data = plot_data
)

summary(model_controls)

############################################################
# 13. SAVE SUMMARY
############################################################

