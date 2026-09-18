# Code author: Rani Davis
# Last updated: 16 July 2026

# ----------------------------------------
# Packages
# ----------------------------------------
library(tidyverse)
library(ggrepel)     # for non-overlapping plot labels
library(scales)      # for axis formatting
library(ggpubr)      # for stat_cor for correlation on plots
library(readxl)      # to read Excel files
library(GGally)      # for ggpairs correlation matrix
library(broom)       # for tidy model outputs
library(patchwork)   # to combine plots
library(countrycode) #

# Indices datasets
library(WDI)         # World Bank data
library(OECD)        # OECD data (optional)
library(FAOSTAT)     # FAO data (optional - requires login)
# GRAPE dataset from this paper = https://www.nature.com/articles/s41597-025-05331-y?utm_source=researchgate.net&utm_medium=article#Sec10

# ========================================
# 0. BUILD COUNTRY LOOKUP (shared across all index pulls)
# ========================================
country_recode <- c(
  "USA"                     = "United States",
  "UK"                      = "United Kingdom",
  "Peru & Colombia"         = "Peru",
  "Australia & New Zealand" = "Australia",
  "Taiwan"                  = "Taiwan, China"
)

all_countries_wb <- union(
  unique(survey_1$Country.clean),
  unique(survey_2$Country.clean)
) %>%
  na.omit() %>%
  recode(!!!country_recode) %>%
  unique()

iso_codes <- countrycode(all_countries_wb, origin = "country.name", destination = "iso2c")
all_countries_wb[is.na(iso_codes)]                          # check for unmatched names
iso_codes[all_countries_wb == "Taiwan, China"] <- "TW"       # hardcode Taiwan's WB-style code
iso_codes <- unique(na.omit(iso_codes))

# WDI doesn't recognise Taiwan as a reporting entity -- exclude from WDI calls only
iso_codes_wdi <- iso_codes[iso_codes != "TW"]


# ========================================
# 1. PULL WORLD BANK GDP DATA (scoped to your countries)
# ========================================
# Can search through different indices online at https://data.worldbank.org/indicator
WDIsearch(string = "NV.AGR.TOTL.ZS", field = "indicator", cache = NULL, short = FALSE)
WDIsearch(string = "GB.XPD.RSDV.GD.ZS", field = "indicator", cache = NULL, short = FALSE)

gdp_data <- WDI(
  country   = iso_codes_wdi,
  indicator = "NY.GDP.PCAP.CD",
  start = 2016, end = 2026,
  extra = TRUE
) %>%
  filter(year == 2023) %>%
  select(country, iso2c, GDP_per_capita = NY.GDP.PCAP.CD)

wdi_raw <- WDI(
  country   = iso_codes_wdi,
  indicator = c(
    AgForestryFish_ValueAdded_percentGDP = "NV.AGR.TOTL.ZS",
    AllResearchAndDev_percentGPD         = "GB.XPD.RSDV.GD.ZS"
  ),
  start = 2018, end = 2023
)

wdi_indicators <- wdi_raw %>%
  group_by(country) %>%
  summarise(
    AgForestryFish_ValueAdded_percentGDP = last(na.omit(AgForestryFish_ValueAdded_percentGDP)),
    AllResearchAndDev_percentGPD         = last(na.omit(AllResearchAndDev_percentGPD)),
    .groups = "drop"
  )


# ========================================
# 2. PULL EPI DATA (filtered to your countries after download --
#    EPI doesn't offer a country-scoped API call, so filter locally)
# ========================================
epi_raw <- read_csv("Analysis/raw data/epi2026results2026-07-07.csv") # downloaded the 2026 results here - https://epi.yale.edu/downloads

epi_data <- epi_raw %>%
  select(country = country, EnviroPerformance_score = EPI.new) %>%
  mutate(country = recode(country, "United States of America" = "United States")) %>%
  filter(country %in% all_countries_wb)


# ========================================
# 3. PULL GRAPE AG R&D DATA (filtered to your countries after read)
# ========================================
# https://www.nature.com/articles/s41597-025-05331-y?utm_source=researchgate.net&utm_medium=article#Sec10
# Can download here - https://zenodo.org/records/15507361
all_countries_grape <- recode(all_countries_wb, "Taiwan, China" = "Taiwan")

grape_raw <- read_xlsx("Analysis/raw data/grape_v1.0.0.xlsx")

grape_clean <- grape_raw %>%
  filter(variable == "RD", country %in% all_countries_grape) %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarise(
    AgResearchAndDev_PPP2017 = first(value),
    AgResearchAndDev_year    = first(year),
    iso3c                    = first(iso3c),
    .groups = "drop"
  )


# ========================================
# CHECK FOR NAME-MATCHING FAILURES (vs genuine no-data NAs)
# ========================================
epi_unmatched <- setdiff(all_countries_wb, epi_data$country)
epi_unmatched     # should be only "Taiwan, China"

grape_unmatched <- setdiff(all_countries_grape, unique(grape_raw$country))
grape_unmatched   # should be empty

wdi_unmatched <- setdiff(all_countries_wb, unique(wdi_raw$country))
wdi_unmatched     # should be only "Taiwan, China" (WB has no Taiwan data)


# ========================================
# BUILD COVERAGE TABLE, WITH TAIWAN IMPUTED FROM CHINA
# (single source of truth -- all downstream joins use this table)
# ========================================
china_vals <- tibble(Country.WB = all_countries_wb) %>%
  left_join(gdp_data,       by = c("Country.WB" = "country")) %>%
  left_join(wdi_indicators, by = c("Country.WB" = "country")) %>%
  left_join(epi_data,       by = c("Country.WB" = "country")) %>%
  filter(Country.WB == "China")

indices_coverage_check <- tibble(Country.WB = all_countries_wb) %>%
  mutate(Country.GRAPE = recode(Country.WB, "Taiwan, China" = "Taiwan")) %>%
  left_join(gdp_data,       by = c("Country.WB" = "country")) %>%
  left_join(wdi_indicators, by = c("Country.WB" = "country")) %>%
  left_join(epi_data,       by = c("Country.WB" = "country")) %>%
  left_join(grape_clean,    by = c("Country.GRAPE" = "country")) %>%
  mutate(
    Taiwan_imputed_from_China = Country.WB == "Taiwan, China" &
      if_any(c(GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
               AllResearchAndDev_percentGPD, EnviroPerformance_score), is.na),
    GDP_per_capita = if_else(Country.WB == "Taiwan, China" & is.na(GDP_per_capita),
                             china_vals$GDP_per_capita, GDP_per_capita),
    AgForestryFish_ValueAdded_percentGDP = if_else(Country.WB == "Taiwan, China" & is.na(AgForestryFish_ValueAdded_percentGDP),
                                                   china_vals$AgForestryFish_ValueAdded_percentGDP, AgForestryFish_ValueAdded_percentGDP),
    AllResearchAndDev_percentGPD = if_else(Country.WB == "Taiwan, China" & is.na(AllResearchAndDev_percentGPD),
                                           china_vals$AllResearchAndDev_percentGPD, AllResearchAndDev_percentGPD),
    EnviroPerformance_score = if_else(Country.WB == "Taiwan, China" & is.na(EnviroPerformance_score),
                                      china_vals$EnviroPerformance_score, EnviroPerformance_score)
  ) %>%
  select(Country.WB, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017,
         Taiwan_imputed_from_China)

print(indices_coverage_check, n = Inf)



