############################################################
# MAP HISTORICAL EXTRACTION TO ETHNIC HOMELANDS
# FOR ANY INPUT COUNTRY
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

# Country name must match Natural Earth admin names
country_name <- "Lesotho"

# Murdock ethnic shapefile
ethnic_file <- "/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp"

# Nunn slave exports file
nunn_file <- "/Users/serafeimkaznessis/Downloads/tribe_level_slave_exports_Atlantic_Indian.dta"

# Output map path
output_map <- "/Users/serafeimkaznessis/Downloads/extraction_ethnic_map.png"

############################################################
# 2. LOAD DATA
############################################################
ethnic <- st_read(ethnic_file, quiet = TRUE)
nunn <- read_dta(nunn_file)

############################################################
# 3. LOAD COUNTRY BOUNDARY
############################################################
country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == country_name)

if (nrow(country_sf) == 0) {
  stop("country_name did not match a Natural Earth country name.")
}

############################################################
# 4. ALIGN CRS AND CLIP ETHNIC HOMELANDS TO COUNTRY
############################################################
ethnic <- st_transform(ethnic, 4326)
country_sf <- st_transform(country_sf, 4326)

ethnic_country <- st_intersection(ethnic, country_sf)

############################################################
# 5. CHECK ETHNIC NAME COLUMN
############################################################
print(names(ethnic_country))

# Assumes ethnic name column is NAME
# Change this if your shapefile uses another field
ethnic_country <- ethnic_country %>%
  mutate(NAME = str_to_upper(str_trim(NAME)))

############################################################
# 6. PREP NUNN EXTRACTION DATA
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
# 7. MERGE EXTRACTION DATA TO ETHNIC HOMELANDS
############################################################
ethnic_country <- ethnic_country %>%
  select(-matches("^total_exports|^ln_total_exports")) %>%
  left_join(nunn_ethnic, by = c("NAME" = "murdock_name"))

cat("Matched ethnic groups with extraction data:",
    sum(!is.na(ethnic_country$ln_total_exports)), "\n")

############################################################
# 8. PLOT EXTRACTION MAP
############################################################
p <- ggplot() +
  geom_sf(data = ethnic_country, aes(fill = ln_total_exports), color = NA) +
  geom_sf(data = country_sf, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste0("Historical Extraction Intensity by Ethnic Homeland (", country_name, ")"),
    fill = "Log Exports"
  )

print(p)

############################################################
# 9. SAVE MAP
############################################################
ggsave(
  filename = output_map,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Saved map to:\n", output_map, "\n")
