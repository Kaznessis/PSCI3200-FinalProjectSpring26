############################################################
# POOLED BHC DISTANCE ANALYSIS WITH COUNTRY FIXED EFFECTS
############################################################

############################################################
# 0. LOAD LIBRARIES
############################################################
library(haven)
library(dplyr)
library(sf)
library(stringr)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

############################################################
# 1. USER SETTINGS
############################################################
#dhs_file <- "/Users/serafeimkaznessis/Downloads/idhs_00004.dta"
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

analysis_var <- "bhcmoney"
min_cluster_n <- 5

survey_list <- tibble::tribble(
  ~country_code, ~country_name, ~year, ~gps_file,
  566, "Nigeria",  2008, "/Users/serafeimkaznessis/Downloads/NGGE52FL/NGGE52FL.shp",
  894, "Zambia",   2013, "/Users/serafeimkaznessis/Downloads/ZMGE71FL/ZMGE71FL.shp",
  800, "Uganda",   2006, "/Users/serafeimkaznessis/Downloads/UGGE61FL/UGGE61FL.shp",
  120, "Cameroon", 2004, "/Users/serafeimkaznessis/Downloads/CMGE42FL/CMGE42FL.shp",
  404, "Kenya",    2014, "/Users/serafeimkaznessis/Downloads/KEGE71FL/KEGE71FL.shp"
)

############################################################
# 2. LOAD SHARED DATA
############################################################
#data <- read_dta(dhs_file)
ethnic <- st_read(ethnic_file, quiet = TRUE) %>%
  st_transform(4326)
nunn <- read_dta(nunn_file)

if (!(analysis_var %in% names(data))) {
  stop("analysis_var not found in pooled DHS extract.")
}

############################################################
# 3. PREP NUNN DATA
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
# 4. PROCESS ONE COUNTRY-YEAR
############################################################
process_survey <- function(country_code, country_name, year, gps_file) {
  cat("Processing:", country_name, year, "\n")
  
  survey_data <- data %>%
    filter(country == country_code, year == !!year)
  
  if (nrow(survey_data) == 0) {
    cat("  No rows for this country-year.\n")
    return(NULL)
  }
  
  print(table(survey_data[[analysis_var]], useNA = "ifany"))
  
  survey_data <- survey_data %>%
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
  
  if (nrow(survey_data) == 0) {
    cat("  Variable has no usable observations after recode.\n")
    return(NULL)
  }
  
  cluster_data <- survey_data %>%
    group_by(clusterno) %>%
    summarize(
      outcome_rate = weighted.mean(outcome, wt, na.rm = TRUE),
      urban_share = weighted.mean(urban == 1, wt, na.rm = TRUE),
      wealth_mean = weighted.mean(wealthq, wt, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= min_cluster_n)
  
  if (nrow(cluster_data) == 0) {
    cat("  No clusters pass min_cluster_n.\n")
    return(NULL)
  }
  
  gps_sf <- st_read(gps_file, quiet = TRUE)
  gps_df <- gps_sf %>%
    st_drop_geometry() %>%
    select(DHSCLUST, LATNUM, LONGNUM)
  
  merged <- cluster_data %>%
    left_join(gps_df, by = c("clusterno" = "DHSCLUST")) %>%
    filter(!is.na(LATNUM), !is.na(LONGNUM))
  
  if (nrow(merged) == 0) {
    cat("  No cluster GPS matches.\n")
    return(NULL)
  }
  
  cluster_sf <- st_as_sf(
    merged,
    coords = c("LONGNUM", "LATNUM"),
    crs = 4326
  )
  
  country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin == country_name) %>%
    st_transform(4326)
  
  if (nrow(country_sf) == 0) {
    cat("  Country boundary not found.\n")
    return(NULL)
  }
  
  ethnic_country <- st_intersection(ethnic, country_sf) %>%
    mutate(NAME = str_to_upper(str_trim(NAME)))
  
  cluster_joined <- st_join(cluster_sf, ethnic_country)
  
  ethnic_summary <- cluster_joined %>%
    st_drop_geometry() %>%
    filter(!is.na(NAME)) %>%
    group_by(NAME) %>%
    summarize(
      mean_outcome = weighted.mean(outcome_rate, w = n, na.rm = TRUE),
      mean_urban = weighted.mean(urban_share, w = n, na.rm = TRUE),
      mean_wealth = weighted.mean(wealth_mean, w = n, na.rm = TRUE),
      n_clusters = dplyr::n(),
      total_women = sum(n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      country_code = country_code,
      country_name = country_name,
      year = year
    )
  
  if (nrow(ethnic_summary) == 0) {
    cat("  No ethnic matches after spatial join.\n")
    return(NULL)
  }
  
  ethnic_summary %>%
    left_join(nunn_ethnic, by = c("NAME" = "murdock_name"))
}

############################################################
# 5. BUILD POOLED DATASET
############################################################
pooled_data <- purrr::pmap_dfr(survey_list, process_survey)

############################################################
# 6. FINAL CHECKS
############################################################
cat("\nRows in pooled analysis file:", nrow(pooled_data), "\n")
print(table(pooled_data$country_name, useNA = "ifany"))

plot_data <- pooled_data %>%
  filter(
    !is.na(mean_outcome),
    !is.na(ln_total_exports),
    !is.na(mean_urban),
    !is.na(mean_wealth)
  )

cat("\nRows with complete model data:", nrow(plot_data), "\n")

############################################################
# 7. REGRESSIONS
############################################################
model_fe <- lm(
  mean_outcome ~ ln_total_exports + mean_urban + mean_wealth + factor(country_name),
  data = plot_data,
  weights = total_women
)
summary(model_fe)

model_simple <- lm(
  mean_outcome ~ ln_total_exports + factor(country_name),
  data = plot_data,
  weights = total_women
)
summary(model_simple)

############################################################
# 8. OPTIONAL PLOT
############################################################
ggplot(plot_data, aes(x = ln_total_exports, y = mean_outcome, color = country_name)) +
  geom_point(aes(size = total_women), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "BHC No Drug Barrier vs Historical Extraction",
    x = "Log Historical Exports",
    y = "Ethnic Mean Outcome",
    color = "Country",
    size = "Women"
  )


write.csv(plot_data, "outputs/tables/bhcmoney_plot_data.csv", row.names = FALSE)
capture.output(summary(model_simple), file = "outputs/tables/bhcmoney_model_simple.txt")
capture.output(summary(model_fe), file = "outputs/tables/bhcmoney_model_fe.txt")
capture.output(summary(model_positive), file = "outputs/tables/bhcmoney_model_positive.txt")
ggsave("outputs/figures/bhcmoney_scatter.png", width = 8, height = 6, dpi = 300)

                                                                            