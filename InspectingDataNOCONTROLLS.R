## Nigeria Analysis

############################################################
# 0. LOAD LIBRARIES
############################################################
library(haven)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

############################################################
# 1. USER SETTINGS: CHANGE THESE
############################################################

# Filtered country-year DHS extract created from your prep script
dhs_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"

# DHS GPS shapefile (.shp)
# Nigeria: gps_file <- "/Users/serafeimkaznessis/Downloads/NGGE8AFL/NGGE8AFL.shp"
gps_file <- "/Users/serafeimkaznessis/Downloads/CMGE42FL/CMGE42FL.shp"

# Murdock ethnic shapefile
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"

# Nunn slave exports file
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

# Country to analyze
country_name <- "Cameroon"

# Variable of analysis
analysis_var <- "bhcatthw"

# Minimum number of women per cluster
min_cluster_n <- 5

############################################################
# 2. LOAD DATA
############################################################
data <- read_dta(dhs_file)
gps_sf <- st_read(gps_file, quiet = TRUE)
ethnic <- st_read(ethnic_file, quiet = TRUE)
nunn <- read_dta(nunn_file)

############################################################
# 3. INSPECT VARIABLE CODING FIRST
############################################################
print(table(data[[analysis_var]], useNA = "ifany"))
print(attr(data[[analysis_var]], "labels"))

############################################################
# 4. BASIC CLEANING
############################################################
data_clean <- data %>%
  mutate(
    wt = perweight / 1000000,
    outcome = case_when(
      .data[[analysis_var]] == 20 ~ 1,
      .data[[analysis_var]] %in% c(10, 11, 12) ~ 0,
      .data[[analysis_var]] %in% c(98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(outcome))

############################################################
# 5. CLUSTER-LEVEL AGGREGATION
############################################################
cluster_data <- data_clean %>%
  group_by(clusterno) %>%
  summarize(
    outcome_rate = weighted.mean(outcome, wt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_cluster_n)

############################################################
# 6. PREP GPS DATA
############################################################
gps_df <- gps_sf %>%
  st_drop_geometry() %>%
  select(DHSCLUST, LATNUM, LONGNUM)

############################################################
# 7. MERGE CLUSTER DATA WITH GPS
############################################################
merged <- cluster_data %>%
  left_join(gps_df, by = c("clusterno" = "DHSCLUST")) %>%
  filter(!is.na(LATNUM), !is.na(LONGNUM))

############################################################
# 8. CONVERT CLUSTERS TO SPATIAL POINTS
############################################################
cluster_sf <- st_as_sf(
  merged,
  coords = c("LONGNUM", "LATNUM"),
  crs = 4326
)

############################################################
# 9. LOAD COUNTRY BOUNDARY AND CLIP ETHNIC POLYGONS
############################################################
country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == country_name)

ethnic <- st_transform(ethnic, 4326)
country_sf <- st_transform(country_sf, 4326)

ethnic_country <- st_intersection(ethnic, country_sf)

############################################################
# 10. SPATIAL JOIN: CLUSTERS -> ETHNIC HOMELANDS
############################################################
cluster_joined <- st_join(cluster_sf, ethnic_country)
print(names(ethnic_country))

############################################################
# 11. AGGREGATE TO ETHNIC LEVEL
############################################################
ethnic_summary <- cluster_joined %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarize(
    mean_outcome = weighted.mean(outcome_rate, w = n, na.rm = TRUE),
    n_clusters = dplyr::n(),
    total_women = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
# 12. MERGE ETHNIC-LEVEL OUTCOME BACK TO POLYGONS
############################################################
ethnic_country <- ethnic_country %>%
  left_join(ethnic_summary, by = "NAME")

############################################################
# 13. PREP NUNN EXTRACTION DATA
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
# 14. MERGE EXTRACTION DATA ONTO ETHNIC POLYGONS
############################################################
ethnic_country <- ethnic_country %>%
  select(-matches("^total_exports|^ln_total_exports")) %>%
  left_join(nunn_ethnic, by = c("NAME" = "murdock_name"))

cat("Matched ethnic groups with extraction data:",
    sum(!is.na(ethnic_country$ln_total_exports)), "\n")

############################################################
# 15. MAP 1: OUTCOME BY ETHNIC TERRITORY
############################################################
ggplot() +
  geom_sf(data = ethnic_country, aes(fill = mean_outcome), color = NA) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste0("Ethnic-Level Outcome: ", analysis_var, " (", country_name, ")"),
    fill = "Outcome Rate"
  )

############################################################
# 16. MAP 2: EXTRACTION INTENSITY BY ETHNIC TERRITORY
############################################################
ggplot() +
  geom_sf(data = ethnic_country, aes(fill = ln_total_exports), color = NA) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste0("Historical Extraction Intensity by Ethnicity (", country_name, ")"),
    fill = "Log Exports"
  )

############################################################
# 17. SCATTERPLOT: EXTRACTION VS OUTCOME
############################################################
plot_data <- ethnic_country %>%
  st_drop_geometry() %>%
  filter(!is.na(ln_total_exports), !is.na(mean_outcome))

ggplot(plot_data, aes(x = ln_total_exports, y = mean_outcome)) +
  geom_point(size = 2, alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = paste0("Extraction Intensity vs ", analysis_var, " (", country_name, ")"),
    x = "Log Historical Exports",
    y = "Ethnic Mean Outcome"
  )

############################################################
# 18. REGRESSIONS
############################################################
model <- lm(mean_outcome ~ ln_total_exports, data = plot_data)
summary(model)

plot_data$any_barrier <- plot_data$mean_outcome > 0

model_extensive <- glm(
  any_barrier ~ ln_total_exports,
  data = plot_data,
  family = binomial
)
summary(model_extensive)

intensive_data <- plot_data %>%
  filter(mean_outcome > 0)

if (nrow(intensive_data) > 0) {
  model_intensive <- lm(mean_outcome ~ ln_total_exports, data = intensive_data)
  summary(model_intensive)
}

model_fractional <- glm(
  mean_outcome ~ ln_total_exports,
  data = plot_data,
  family = quasibinomial
)
summary(model_fractional)

############################################################
# 19. OPTIONAL: MAP RAW CLUSTERS FOR SANITY CHECK
############################################################
ggplot() +
  geom_sf(data = ethnic_country, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = cluster_sf, aes(color = outcome_rate), size = 1) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = paste0("Cluster-Level ", analysis_var, " (", country_name, ")"),
    color = "Cluster Rate"
  )
##base_dir <- "/Users/serafeimkaznessis/Desktop/PSCI3200-FinalProjectSpring26/outputs/tables/CountrySpecific"

# Pull the country being analyzed from the dataset if possible
##country_from_data <- unique(na.omit(data$country_name))

# Fallback to country_name object if country_name is not stored in data
##if (length(country_from_data) == 0) {
##  country_folder <- country_name
##} else {
##  country_folder <- as.character(country_from_data[1])
##}

##country_dir <- file.path(base_dir, country_folder)

##dir.create(country_dir, showWarnings = FALSE, recursive = TRUE)

##output_file <- file.path(
##  country_dir,
##  paste0(country_folder, "_", analysis_var, "_model_controls_summary.txt")
##)

##capture.output(summary(model_controls), file = output_file)

output_file <- paste0(
  "/Users/serafeimkaznessis/Desktop/PSCI3200-FinalProjectSpring26/outputs/tables/CountrySpecific/",
  country_name, "_",
  analysis_var,
  "_simple_regression_summary.txt"
)

capture.output(summary(model), file = output_file)
