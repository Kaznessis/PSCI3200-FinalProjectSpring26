############################################################
# NIGERIA: CLUSTER-LEVEL WEALTHS VS HISTORICAL EXTRACTION
############################################################

library(haven)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

dhs_file <- "/Users/serafeimkaznessis/Downloads/dhs_country_year_filtered.dta"
gps_file <- "/Users/serafeimkaznessis/Downloads/NGGE52FL/NGGE52FL.shp"
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

country_name <- "Nigeria"
min_cluster_n <- 5

data <- read_dta(dhs_file)
gps_sf <- st_read(gps_file, quiet = TRUE)
ethnic <- st_read(ethnic_file, quiet = TRUE)
nunn <- read_dta(nunn_file)

data <- data %>%
  mutate(wt = perweight / 1000000)

cluster_data <- data %>%
  filter(!is.na(wealths), !is.na(clusterno)) %>%
  group_by(clusterno) %>%
  summarize(
    mean_wealth_cluster = weighted.mean(wealths, wt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= min_cluster_n)

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

country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == country_name)

ethnic <- st_transform(ethnic, 4326)
country_sf <- st_transform(country_sf, 4326)

ethnic_country <- st_intersection(ethnic, country_sf) %>%
  mutate(NAME = str_to_upper(str_trim(NAME)))

cluster_joined <- st_join(cluster_sf, ethnic_country)

nunn_ethnic <- nunn %>%
  mutate(
    total_exports = atlantic_all_years + indian_all_years,
    ln_total_exports = log1p(total_exports),
    murdock_name = str_to_upper(str_trim(murdock_name))
  ) %>%
  select(murdock_name, total_exports, ln_total_exports) %>%
  distinct()

cluster_level_data <- cluster_joined %>%
  st_drop_geometry() %>%
  filter(!is.na(NAME)) %>%
  left_join(nunn_ethnic, by = c("NAME" = "murdock_name")) %>%
  filter(!is.na(ln_total_exports), !is.na(mean_wealth_cluster))

ggplot(cluster_level_data, aes(x = ln_total_exports, y = mean_wealth_cluster)) +
  geom_point(size = 2, alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = paste0("Cluster-Level Wealth vs Historical Extraction (", country_name, ")"),
    x = "Log Historical Exports",
    y = "Cluster Mean Wealth Score"
  )

model_wealth_cluster <- lm(mean_wealth_cluster ~ ln_total_exports, data = cluster_level_data)
summary(model_wealth_cluster)

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/assignment3", showWarnings = FALSE, recursive = TRUE)

capture.output(
  summary(model_wealth_cluster),
  file = "outputs/assignment3/Nigeria_2008_cluster_wealth_regression_summary.txt"
)

ggsave(
  filename = "outputs/assignment3/Nigeria_2008_cluster_wealth_regression_plot.png",
  plot = last_plot(),
  width = 8,
  height = 6,
  dpi = 300
)


