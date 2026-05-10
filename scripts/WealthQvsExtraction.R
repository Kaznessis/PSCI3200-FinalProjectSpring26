############################################################
# NIGERIA: MAP ETHNIC-LEVEL WEALTH VS HISTORICAL EXTRACTION
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
# 3. PREP DHS CLUSTER-LEVEL WEALTH
############################################################
data <- data %>%
  mutate(wt = perweight / 1000000)

cluster_data <- data %>%
  filter(!is.na(wealthq), !is.na(clusterno)) %>%
  group_by(clusterno) %>%
  summarize(
    mean_wealth_cluster = weighted.mean(wealthq, wt, na.rm = TRUE),
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
# 7. AGGREGATE WEALTH TO ETHNIC LEVEL
############################################################
ethnic_summary <- cluster_joined %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarize(
    mean_wealth = weighted.mean(mean_wealth_cluster, w = n, na.rm = TRUE),
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

cat("Matched ethnic groups with extraction data:",
    sum(!is.na(ethnic_country$ln_total_exports)), "\n")

############################################################
# 10. MAP 1: ETHNIC-LEVEL WEALTH
############################################################
ggplot() +
  geom_sf(data = ethnic_country, aes(fill = mean_wealth), color = NA) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste0("Ethnic-Level Wealth (", country_name, ")"),
    fill = "Mean Wealth"
  )

############################################################
# 11. MAP 2: EXTRACTION INTENSITY
############################################################
ggplot() +
  geom_sf(data = ethnic_country, aes(fill = ln_total_exports), color = NA) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste0("Historical Extraction Intensity (", country_name, ")"),
    fill = "Log Exports"
  )

############################################################
# 12. SCATTERPLOT: EXTRACTION VS WEALTH
############################################################
plot_data <- ethnic_country %>%
  st_drop_geometry() %>%
  filter(!is.na(mean_wealth), !is.na(ln_total_exports))

ggplot(plot_data, aes(x = ln_total_exports, y = mean_wealth)) +
  geom_point(size = 2, alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = paste0("Historical Extraction vs Ethnic-Level Wealth (", country_name, ")"),
    x = "Log Historical Exports",
    y = "Mean Wealth"
  )

############################################################
# 13. REGRESSION: WEALTH AS DEPENDENT VARIABLE
############################################################
model_wealth <- lm(mean_wealth ~ ln_total_exports, data = plot_data)
summary(model_wealth)
