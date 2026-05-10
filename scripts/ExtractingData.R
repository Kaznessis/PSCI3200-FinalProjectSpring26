#install.packages("sf")
#library(haven)
#library(sf)
#data <- read_dta("/Users/serafeimkaznessis/Downloads/112479-V1/NUNN_WANTCHEKON_AER_2011_REPLICATION_FILES/Nunn_Wantchekon_AER_2011.dta")

#head(data)
#names(data)
#ethnic_exports <- data[, c("ethnicity", "ln_exports")]
#ethnic_exports <- unique(ethnic_exports)

#ethnic_map <- st_read("/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp")

#head(ethnic_map)

#data <- read_dta("/Users/serafeimkaznessis/Downloads/EGIR61DT/EGIR61FL.DTA")
#View(data)


library(haven)

data <- read_dta("/Users/serafeimkaznessis/Downloads/idhs_00003.dta")

data$cervcanexam
names(data)
subset_data <- data[!is.na(data$cervcanexam), ]
table(subset_data$cervcanexam)

clean_data <- data %>%
  filter(cervcanexam %in% c(0,1))

cluster_data <- clean_data %>%
  group_by(clusterno) %>%
  summarize(
    screening_rate = mean(cervcanexam == 1),  # if using as_factor
    n = n(),
    .groups = "drop"
  )

cluster_data <- cluster_data %>%
  filter(n >= 20)

cluster_data <- clean_data %>%
  group_by(clusterno, country, urban) %>%
  summarize(
    screening_rate = mean(cervcanexam == 1),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

write.csv(cluster_data, "cluster_screening_data.csv", row.names = FALSE)


library(sf)

ca_data <- cluster_data %>%
  filter(country == 120)

gps_ca <- st_read("/Users/serafeimkaznessis/Downloads/CMGE71FL/CMGE71FL.shp")

merged_ca <- ca_data %>%
  left_join(gps_ca, by = c("clusterno" = "DHSCLUST"))

summary(merged_ca$LONGNUM)

final_sf <- st_as_sf(
  merged_ca,
  coords = c("LONGNUM", "LATNUM"),
  crs = 4326
)

plot(final_sf$geometry)

library(ggplot2)

ggplot() +
  geom_sf(data = final_sf, aes(color = screening_rate)) +
  scale_color_viridis_c() +
  theme_minimal()

# Load sf package (for spatial data)
library(sf)

# Read your Murdock shapefile
# (change path to where your file actually is)
ethnic <- st_read("/Users/serafeimkaznessis/Downloads/Murdock_shapefile_v2_2024/Murdock_Map_2020.shp")

# Ensure same coordinate system as DHS points
ethnic <- st_transform(ethnic, crs = 4326)

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(rnaturalearth)
library(dplyr)

# Get Cameroon boundary
cameroon <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Cameroon")

# Clip ethnic polygons to Cameroon only
ethnic_cm <- st_intersection(ethnic, cameroon)

library(ggplot2)

ggplot() +
  geom_sf(data = ethnic_cm, fill = NA, color = "black") +   # ethnic regions
  geom_sf(data = final_sf, aes(color = screening_rate)) +      # your points
  scale_color_viridis_c() +
  theme_minimal()

cameroon_joined <- st_join (final_sf, ethnic_cm)

table(is.na(cameroon_joined$NAME))

#aggregate by ethnic group
ethnic_summary <- cameroon_joined %>%
  group_by(NAME) %>%
  summarize(
    mean_screening = mean(screening_rate, na.rm = TRUE),
    n_clusters = n(),
    .groups = "drop"
  )

ethnic_summary <- st_drop_geometry(ethnic_summary)

ethnic_cm <- ethnic_cm %>%
  left_join(ethnic_summary, by = "NAME")

ggplot() +
  geom_sf(data = ethnic_cm, aes(fill = mean_screening)) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal()


names(ethnic)

nrow(merged_sa)

summary(merged_sa$LONGNUM)









sa_data <- data %>%
  filter(cervcanexam %in% c(0,1)) %>%
  filter(country == 710) %>%
  group_by(clusterno) %>%
  summarize(
    screening_rate = mean(cervcanexam == 1),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

merged_sa <- sa_data %>%
  left_join(gps_sa, by = c("clusterno" = "DHSCLUST"))

table(merged_sa$clusterno)

table_ethnicity <- ethnic_summary %>%
  arrange(desc(mean_screening))   # sort high → low

print(table_ethnicity, n = 50)

table_ethnicity <- table_ethnicity %>%
  rename(
    Ethnicity = NAME,
    ScreeningRate = mean_screening,
    Clusters = n_clusters
  )

View (table_ethnicity)