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
Survey_1_long_indices <- Survey_1_long_indices %>%
  left_join(
    exact_GPS_unique,
    by = c("Respondent.ID", "Respondent.entry.ID"))

Survey_2_long_indices <- Survey_2_long_indices %>%
  left_join(
    exact_GPS_unique,
    by = c("Respondent.ID", "Respondent.entry.ID"))


# ========================================
# Get coarse latitude from centroid of each country
# ========================================

world <- ne_countries(scale = "medium", returnclass = "sf")

country_centroids <- world %>%
  st_centroid() %>%
  mutate(Longitude = st_coordinates(.)[,1],
    Coarse_latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(
    Country.clean = name,
    Coarse_latitude,
    Longitude
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

# ---- Add Australia & New Zealand (Stuart Parsons work across both countries) using Australian centroid ----
aus_coords <- country_centroids %>%
  filter(Country.clean == "Australia") %>%
  mutate(Country.clean = "Australia & New Zealand")

# ---- Add Peru & Colombia using midpoint of centroids ----
peru_col_coords <- country_centroids %>%
  filter(Country.clean %in% c("Peru", "Colombia")) %>%
  summarise(
    Coarse_latitude = mean(Coarse_latitude),
    Longitude = mean(Longitude)
  ) %>%
  mutate(Country.clean = "Peru & Colombia")

# ---- Add Eswatini if missing ----
eswatini_coords <- world %>%
  st_centroid() %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Coarse_latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  filter(name %in% c("Eswatini", "Swaziland", "eSwatini")) %>%
  select(
    Country.clean = name,
    Coarse_latitude,
    Longitude
  )

# ---- Combine all centroid data ----
country_centroids <- bind_rows(
  country_centroids,
  aus_coords,
  peru_col_coords,
  eswatini_coords)

# ---- Match Eswatini naming ----
country_centroids <- country_centroids %>%
  mutate(
    Country.clean = ifelse(
      Country.clean == "eSwatini",
      "Eswatini",
      Country.clean)
  )

# ---- Join to survey data ----
Survey_1_long_indices <- Survey_1_long_indices %>%
  left_join(
    country_centroids,
    by = "Country.clean")

Survey_2_long_indices <- Survey_2_long_indices %>%
  left_join(country_centroids,
    by = "Country.clean"
  )

# ---- Check unmatched countries ----
Survey_1_long_indices %>%
  filter(is.na(Coarse_latitude)) %>%
  distinct(Country.clean)

Survey_2_long_indices %>%
  filter(is.na(Coarse_latitude)) %>%
  distinct(Country.clean)


