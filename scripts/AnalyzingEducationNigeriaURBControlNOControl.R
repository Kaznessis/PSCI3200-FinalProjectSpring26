############################################################
# NIGERIA PLACEBO TEST: EDUCATION VS HISTORICAL EXTRACTION
# THREE MODELS:
# 1. NO CONTROLS
# 2. CONTROLLING ONLY FOR URBANIZATION
# 3. CONTROLLING FOR URBANIZATION AND WEALTH
############################################################

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
# 1. USER SETTINGS
############################################################
dhs_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"
gps_file <- "/Users/serafeimkaznessis/Downloads/NGGE52FL/NGGE52FL.shp"
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

country_name <- "Nigeria"
min_cluster_n <- 5

############################################################
# 2. LOAD DATA
############################################################
data <- read_dta(dhs_file)
gps_sf <- st_read(gps_file, quiet = TRUE)
ethnic <- st_read(ethnic_file, quiet = TRUE)
nunn <- read_dta(nunn_file)

############################################################
# 3. PREP DHS CLUSTER-LEVEL EDUCATION, URBANIZATION, AND WEALTH
############################################################
data <- data %>%
  mutate(wt = perweight / 1000000)

cluster_data <- data %>%
  filter(
    !is.na(edyrtotal),
    !is.na(urban),
    !is.na(wealthq),
    !is.na(clusterno)
  ) %>%
  group_by(clusterno) %>%
  summarize(
    mean_education = weighted.mean(edyrtotal, wt, na.rm = TRUE),
    mean_urban = weighted.mean(urban == 1, wt, na.rm = TRUE),
    mean_wealth = weighted.mean(wealthq, wt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_cluster_n)

############################################################
# 4. PREP GPS DATA
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
# 5. COUNTRY BOUNDARY AND ETHNIC POLYGONS
############################################################
country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == country_name)

ethnic <- st_transform(ethnic, 4326)
country_sf <- st_transform(country_sf, 4326)

ethnic_country <- st_intersection(ethnic, country_sf)

############################################################
# 6. SPATIAL JOIN: CLUSTERS TO ETHNIC HOMELANDS
############################################################
cluster_joined <- st_join(cluster_sf, ethnic_country)

print(names(ethnic_country))

############################################################
# 7. AGGREGATE EDUCATION, URBANIZATION, AND WEALTH TO ETHNIC LEVEL
############################################################
ethnic_summary <- cluster_joined %>%
  st_drop_geometry() %>%
  filter(!is.na(NAME)) %>%
  group_by(NAME) %>%
  summarize(
    mean_education = weighted.mean(mean_education, w = n, na.rm = TRUE),
    mean_urban = weighted.mean(mean_urban, w = n, na.rm = TRUE),
    mean_wealth = weighted.mean(mean_wealth, w = n, na.rm = TRUE),
    n_clusters = dplyr::n(),
    total_women = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

ethnic_country <- ethnic_country %>%
  left_join(ethnic_summary, by = "NAME")

############################################################
# 8. PREP NUNN EXTRACTION DATA
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
# 9. MERGE EXTRACTION DATA
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
# 10. BUILD REGRESSION DATASET
############################################################
plot_data <- ethnic_country %>%
  st_drop_geometry() %>%
  filter(
    !is.na(mean_education),
    !is.na(mean_urban),
    !is.na(mean_wealth),
    !is.na(ln_total_exports)
  )

cat("Rows in placebo regression dataset:", nrow(plot_data), "\n")

############################################################
# 11. PLACEBO REGRESSION: NO CONTROLS
############################################################
model_placebo_no_controls <- lm(
  mean_education ~ ln_total_exports,
  data = plot_data
)

summary(model_placebo_no_controls)

############################################################
# 12. PLACEBO REGRESSION: CONTROLLING ONLY FOR URBANIZATION
############################################################
model_placebo_urban_control <- lm(
  mean_education ~ ln_total_exports + mean_urban,
  data = plot_data
)

summary(model_placebo_urban_control)

############################################################
# 13. PLACEBO REGRESSION: CONTROLLING FOR URBANIZATION AND WEALTH
############################################################
model_placebo_urban_wealth_control <- lm(
  mean_education ~ ln_total_exports + mean_urban + mean_wealth,
  data = plot_data
)

summary(model_placebo_urban_wealth_control)

############################################################
# 14. COEFFICIENT TABLES
############################################################
cat("\nNo-control model coefficients:\n")
print(coef(summary(model_placebo_no_controls)))

cat("\nUrban-control model coefficients:\n")
print(coef(summary(model_placebo_urban_control)))

cat("\nUrban-and-wealth-control model coefficients:\n")
print(coef(summary(model_placebo_urban_wealth_control)))

############################################################
# 15. READABLE REGRESSION EQUATIONS
############################################################
no_controls_table <- coef(summary(model_placebo_no_controls))
urban_control_table <- coef(summary(model_placebo_urban_control))
urban_wealth_control_table <- coef(summary(model_placebo_urban_wealth_control))

cat("\nNo-control equation:\n")
cat(
  "mean_education = ",
  round(no_controls_table["(Intercept)", "Estimate"], 4),
  " + ",
  round(no_controls_table["ln_total_exports", "Estimate"], 4),
  " * ln_total_exports\n",
  sep = ""
)

cat("\nUrban-control equation:\n")
cat(
  "mean_education = ",
  round(urban_control_table["(Intercept)", "Estimate"], 4),
  " + ",
  round(urban_control_table["ln_total_exports", "Estimate"], 4),
  " * ln_total_exports + ",
  round(urban_control_table["mean_urban", "Estimate"], 4),
  " * mean_urban\n",
  sep = ""
)

cat("\nUrban-and-wealth-control equation:\n")
cat(
  "mean_education = ",
  round(urban_wealth_control_table["(Intercept)", "Estimate"], 4),
  " + ",
  round(urban_wealth_control_table["ln_total_exports", "Estimate"], 4),
  " * ln_total_exports + ",
  round(urban_wealth_control_table["mean_urban", "Estimate"], 4),
  " * mean_urban + ",
  round(urban_wealth_control_table["mean_wealth", "Estimate"], 4),
  " * mean_wealth\n",
  sep = ""
)

############################################################
# 16. OPTIONAL PLOT
############################################################
p <- ggplot(plot_data, aes(x = ln_total_exports, y = mean_education)) +
  geom_point(size = 2, alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  theme_minimal() +
  labs(
    title = paste0("Placebo Test: Education vs Historical Extraction (", country_name, ")"),
    x = "Log Historical Exports",
    y = "Mean Years of Education"
  )

print(p)

############################################################
# 17. SAVE OUTPUTS
############################################################
output_dir <- "/Users/serafeimkaznessis/Desktop/PSCI3200-FinalProjectSpring26/outputs/assignment3/nigeria_variable_analysis"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

capture.output(
  summary(model_placebo_no_controls),
  file = paste0(output_dir, "/Nigeria_edyrtotal_placebo_no_controls_summary.txt")
)

capture.output(
  summary(model_placebo_urban_control),
  file = paste0(output_dir, "/Nigeria_edyrtotal_placebo_urban_control_summary.txt")
)

capture.output(
  summary(model_placebo_urban_wealth_control),
  file = paste0(output_dir, "/Nigeria_edyrtotal_placebo_urban_wealth_control_summary.txt")
)

ggsave(
  filename = paste0(output_dir, "/Nigeria_edyrtotal_placebo_plot.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

############################################################
# 18. SAVE CLEAN COEFFICIENT TABLE WITH ALL THREE MODELS
############################################################

# Pull coefficient tables
no_controls_coef <- coef(summary(model_placebo_no_controls))
urban_control_coef <- coef(summary(model_placebo_urban_control))
urban_wealth_control_coef <- coef(summary(model_placebo_urban_wealth_control))

# Convert no-control model to table
no_controls_table_out <- data.frame(
  country = country_name,
  model = "No controls",
  dependent_variable = "mean_education",
  n_ethnic_groups = nrow(plot_data),
  term = rownames(no_controls_coef),
  coefficient = no_controls_coef[, "Estimate"],
  standard_error = no_controls_coef[, "Std. Error"],
  t_value = no_controls_coef[, "t value"],
  p_value = no_controls_coef[, "Pr(>|t|)"],
  row.names = NULL
)

# Convert urban-control model to table
urban_control_table_out <- data.frame(
  country = country_name,
  model = "Urban control only",
  dependent_variable = "mean_education",
  n_ethnic_groups = nrow(plot_data),
  term = rownames(urban_control_coef),
  coefficient = urban_control_coef[, "Estimate"],
  standard_error = urban_control_coef[, "Std. Error"],
  t_value = urban_control_coef[, "t value"],
  p_value = urban_control_coef[, "Pr(>|t|)"],
  row.names = NULL
)

# Convert urban-and-wealth-control model to table
urban_wealth_control_table_out <- data.frame(
  country = country_name,
  model = "Urban and wealth controls",
  dependent_variable = "mean_education",
  n_ethnic_groups = nrow(plot_data),
  term = rownames(urban_wealth_control_coef),
  coefficient = urban_wealth_control_coef[, "Estimate"],
  standard_error = urban_wealth_control_coef[, "Std. Error"],
  t_value = urban_wealth_control_coef[, "t value"],
  p_value = urban_wealth_control_coef[, "Pr(>|t|)"],
  row.names = NULL
)

# Combine all three model tables
placebo_regression_table <- bind_rows(
  no_controls_table_out,
  urban_control_table_out,
  urban_wealth_control_table_out
)

# Print combined table
print(placebo_regression_table)

# Save combined table as CSV
write.csv(
  placebo_regression_table,
  paste0(output_dir, "/Nigeria_edyrtotal_placebo_all_models_table.csv"),
  row.names = FALSE
)

cat("\nSaved placebo outputs to:", output_dir, "\n")

