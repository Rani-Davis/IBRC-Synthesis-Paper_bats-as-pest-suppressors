# Code author: Rani Davis
# Last updated: 22 July 2026
#
# ----------------------------------------
# Packages
# ----------------------------------------
library(rnaturalearth)
library(sf)
library(dplyr)

# ========================================
# Exact latitudes per closest city here (from survey 1):
# ========================================
exact_GPS <- read.csv("Analysis/clean data/Map data_study system coordinates_no Jitter.csv")

exact_GPS_unique <- exact_GPS %>%
  select(
    Respondent.ID,
    Respondent.entry.ID,
    Exact_latitude = Latitude,
    Exact_longitude = Longitude
  ) %>%
  distinct()

# make sure they are all characters to allow the join:
exact_GPS_unique <- exact_GPS_unique %>%
  mutate(Respondent.entry.ID = as.character(Respondent.entry.ID))

Survey_1_long_indices <- Survey_1_long_indices  %>%
  mutate(Respondent.entry.ID = as.character(Respondent.entry.ID))
Survey_1_long_indices <- Survey_1_long_indices  %>%
  mutate(Respondent.ID = as.character(Respondent.ID))
Survey_2_long_indices <- Survey_2_long_indices  %>%
  mutate(Respondent.entry.ID = as.character(Respondent.entry.ID))
Survey_2_long_indices <- Survey_2_long_indices  %>%
  mutate(Respondent.ID = as.character(Respondent.ID))

# Join to indices datasets:
Survey_1_long_indices_wLatLong <- Survey_1_long_indices %>%
  left_join(
    exact_GPS_unique,
    by = c("Respondent.ID", "Respondent.entry.ID"))

Survey_2_long_indices_wLatLong <- Survey_2_long_indices %>%
  left_join(
    exact_GPS_unique,
    by = c("Respondent.ID", "Respondent.entry.ID"))


# ========================================
# Get coarse latitude/longitude from centroid of each country
# ========================================
# "Coarse" latitude/longitude = the geographic centroid of the respondent's
# country (from rnaturalearth country polygons), used as a location estimate
# alongside the exact respondent GPS coordinates joined in above.

world <- ne_countries(scale = "medium", returnclass = "sf")

country_centroids <- world %>%
  st_centroid() %>%
  mutate(
    Coarse_longitude = st_coordinates(.)[,1],
    Coarse_latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(
    Country.clean = name,
    Coarse_latitude,
    Coarse_longitude
  )

# ---- Rename countries to match survey names ----
country_centroids <- country_centroids %>%
  mutate(
    Country.clean = case_when(
      Country.clean == "United States of America" ~ "USA",
      Country.clean == "United Kingdom" ~ "UK",
      TRUE ~ Country.clean
    )
  )

# ---- Guard against duplicate country rows in the source data ----
# Natural Earth occasionally returns more than one feature for the same
# country name (disputed territories, multi-part sovereign states, etc.).
# Check before collapsing, so you know what's being kept vs dropped:
country_centroids %>% count(Country.clean) %>% filter(n > 1)

country_centroids <- country_centroids %>%
  distinct(Country.clean, .keep_all = TRUE)

# ---- Add Australia & New Zealand (Stuart Parsons work across both countries) using Australian centroid ----
aus_coords <- country_centroids %>%
  filter(Country.clean == "Australia") %>%
  mutate(Country.clean = "Australia & New Zealand")

# ---- Add Peru & Colombia using midpoint of centroids ----
peru_col_coords <- country_centroids %>%
  filter(Country.clean %in% c("Peru", "Colombia")) %>%
  summarise(
    Coarse_latitude  = mean(Coarse_latitude),
    Coarse_longitude = mean(Coarse_longitude)
  ) %>%
  mutate(Country.clean = "Peru & Colombia")

# ---- Add Eswatini if missing ----
eswatini_coords <- world %>%
  st_centroid() %>%
  mutate(
    Coarse_longitude = st_coordinates(.)[,1],
    Coarse_latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  filter(name %in% c("Eswatini", "Swaziland", "eSwatini")) %>%
  select(
    Country.clean = name,
    Coarse_latitude,
    Coarse_longitude
  )

# ---- Combine all centroid data ----
country_centroids <- bind_rows(
  country_centroids,
  aus_coords,
  peru_col_coords,
  eswatini_coords
)

# ---- Match Eswatini naming ----
country_centroids <- country_centroids %>%
  mutate(
    Country.clean = ifelse(
      Country.clean == "eSwatini",
      "Eswatini",
      Country.clean)
  )

# ---- Final safety net before joining ----
# aus_coords / peru_col_coords / eswatini_coords shouldn't introduce new
# duplicates, but re-check in case any of those labels already existed
country_centroids <- country_centroids %>%
  distinct(Country.clean, .keep_all = TRUE)

# ---- Join to survey data ----
Survey_1_long_indices_wLatLong <- Survey_1_long_indices_wLatLong %>%
  left_join(
    country_centroids,
    by = "Country.clean")

Survey_2_long_indices_wLatLong <- Survey_2_long_indices_wLatLong %>%
  left_join(
    country_centroids,
    by = "Country.clean")

# ---- Check unmatched countries ----
Survey_1_long_indices_wLatLong %>%
  filter(is.na(Coarse_latitude)) %>%
  distinct(Country.clean)

Survey_2_long_indices_wLatLong %>%
  filter(is.na(Coarse_latitude)) %>%
  distinct(Country.clean)

names(Survey_1_long_indices_wLatLong)
names(Survey_2_long_indices_wLatLong)

# Remove duplicates
#Survey_1_long_indices_wLatLong <- Survey_1_long_indices_wLatLong %>%
 # rename(Exact_latitude = Exact_latitude.x, Exact_longitude = Exact_longitude.x) %>%
  #select(-Exact_latitude.y, -Exact_longitude.y)

#Survey_2_long_indices_wLatLong <- Survey_2_long_indices_wLatLong %>%
 # rename(Exact_latitude = Exact_latitude.x, Exact_longitude = Exact_longitude.x) %>%
  #select(-Exact_latitude.y, -Exact_longitude.y)

#Survey_1_long_indices_wLatLong <- Survey_1_long_indices_wLatLong %>%
 # select(-Coarse_latitude.x, -Coarse_longitude.x, -Coarse_latitude.y, -Coarse_longitude.y)

#Survey_2_long_indices_wLatLong <- Survey_2_long_indices_wLatLong %>%
 # select(-Coarse_latitude.x, -Coarse_longitude.x, -Coarse_latitude.y, -Coarse_longitude.y)



# Confirm they're gone:
names(Survey_1_long_indices_wLatLong)
names(Survey_2_long_indices)

write.csv(Survey_1_long_indices_wLatLong,"Analysis/clean data/Survey 1_scored_with indices_w Lat Long.csv", row.names = FALSE)
write.csv(Survey_2_long_indices_wLatLong,"Analysis/clean data/Survey 2_scored_with indices_w Lat Long.csv", row.names = FALSE)
