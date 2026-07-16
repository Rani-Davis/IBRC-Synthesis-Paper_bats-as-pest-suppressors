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
epi_raw <- read_csv("https://epi.yale.edu/downloads/epi2024results.csv", show_col_types = FALSE)

epi_data <- epi_raw %>%
  select(country = country, EnviroPerformance_score = EPI.new) %>%
  mutate(country = recode(country, "United States of America" = "United States")) %>%
  filter(country %in% all_countries_wb)

# If URL fails, download manually from https://epi.yale.edu and read locally:
# epi_data <- read_csv("epi2024results.csv") %>% select(country, EnviroPerformance_score = EPI.new) %>% filter(country %in% all_countries_wb)


# ========================================
# 3. PULL GRAPE AG R&D DATA (filtered to your countries after read)
# ========================================
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




# ========================================
# 4. JOIN INDICES TO SURVEY 1 DATA
# ========================================
Survey_1_long_indices <- Survey_1_long %>%
  mutate(Country.WB = recode(Country.clean, !!!country_recode)) %>%
  left_join(gdp_data,       by = c("Country.WB" = "country")) %>%
  left_join(wdi_indicators, by = c("Country.WB" = "country")) %>%
  left_join(epi_data,       by = c("Country.WB" = "country")) %>%
  mutate(Country.GRAPE = recode(Country.WB, "Taiwan, China" = "Taiwan")) %>%
  left_join(grape_clean, by = c("Country.GRAPE" = "country"))

# Check coverage
Survey_1_long_indices %>%
  summarise(
    n_gdp   = sum(!is.na(GDP_per_capita)),
    n_ag_va = sum(!is.na(AgForestryFish_ValueAdded_percentGDP)),
    n_rd    = sum(!is.na(AllResearchAndDev_percentGPD)),
    n_epi   = sum(!is.na(EnviroPerformance_score)),
    n_ag_rd = sum(!is.na(AgResearchAndDev_PPP2017))
  )

# Check unmatched countries
Survey_1_long_indices %>%
  filter(!is.na(Country.WB), is.na(GDP_per_capita)) %>%
  distinct(Country.WB)


# ========================================
# 5. SUMMARISE TO COUNTRY LEVEL
# ========================================
country_indices <- Survey_1_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  group_by(Country.clean, Country.WB,
           GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
           AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  summarise(
    Mean.Total.Score = mean(TotalScore, na.rm = TRUE),
    n = n_distinct(Respondent.ID),
    .groups = "drop"
  ) %>%
  mutate(Country.n.label = paste0(Country.clean, "\n(n=", n, ")"))

country_indices <- country_indices %>%
  mutate(across(
    c(GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
      AllResearchAndDev_percentGPD, EnviroPerformance_score),
    ~ if_else(
      Country.WB == "Taiwan, China" & is.na(.x),
      .x[Country.WB == "China"][1],
      .x
    )
  ))


# ========================================
# 8. EXPLORE COLLINEARITY BETWEEN INDICES
# ========================================
country_indices %>%
  select(Mean.Total.Score, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

country_indices %>%
  select(Mean.Total.Score, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  ggpairs()

indices <- c("GDP_per_capita", "AgForestryFish_ValueAdded_percentGDP",
             "AllResearchAndDev_percentGPD", "EnviroPerformance_score", "AgResearchAndDev_PPP2017")

Survey1_Indice_cor_table <- map_df(indices, function(var) {
  d <- country_indices %>% filter(!is.na(.data[[var]]), !is.na(Mean.Total.Score))
  test <- cor.test(d[[var]], d$Mean.Total.Score, method = "pearson")
  tibble(
    Index   = var,
    n       = nrow(d),
    r       = round(test$estimate, 3),
    p_value = round(test$p.value, 3),
    sig     = case_when(
      test$p.value < 0.001 ~ "***",
      test$p.value < 0.01  ~ "**",
      test$p.value < 0.05  ~ "*",
      test$p.value < 0.1   ~ ".",
      TRUE                 ~ "ns"
    )
  )
})

print(Survey1_Indice_cor_table)


# ========================================
# 9. PLOT EACH INDEX VS MEAN TOTAL SCORE (country means)
# ========================================
ggplot(country_indices %>% filter(!is.na(GDP_per_capita)),
       aes(x = GDP_per_capita, y = Mean.Total.Score,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#2166AC") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_y_continuous(breaks = 0:12) +
  scale_x_log10(labels = scales::dollar_format(scale = 1, prefix = "$")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "GDP per capita (USD)",
       y = "Mean Total Score for Survey 1\n(Knowledge Pathway) per country") +
  ggtitle("Country GDP vs Bat Research Score")

ggplot(country_indices %>% filter(!is.na(AllResearchAndDev_percentGPD)),
       aes(x = AllResearchAndDev_percentGPD, y = Mean.Total.Score,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#2166AC") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_y_continuous(breaks = 0:12) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Mean Total Score for Survey 1\n(Knowledge Pathway) per country") +
  ggtitle("National R&D Investment (all sectors) vs Bat Research Score")

ggplot(country_indices %>% filter(!is.na(AgForestryFish_ValueAdded_percentGDP)),
       aes(x = AgForestryFish_ValueAdded_percentGDP, y = Mean.Total.Score,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#2166AC") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_y_continuous(breaks = 0:12) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 10)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Mean Total Score for Survey 1\n(Knowledge Pathway) per country") +
  ggtitle("Agricultural Economy Dependence vs Bat Research Score")

ggplot(country_indices %>% filter(!is.na(AgResearchAndDev_PPP2017)),
       aes(x = AgResearchAndDev_PPP2017, y = Mean.Total.Score,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#2166AC") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_y_continuous(breaks = 0:12) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Mean Total Score for Survey 1\n(Knowledge Pathway) per country") +
  ggtitle("Agricultural R&D Investment vs Bat Research Score")

ggplot(country_indices %>% filter(!is.na(EnviroPerformance_score)),
       aes(x = EnviroPerformance_score, y = Mean.Total.Score,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#2166AC") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_y_continuous(breaks = 0:12) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Mean Total Score for Survey 1\n(Knowledge Pathway) per country") +
  ggtitle("Environmental Performance vs Bat Research Score")


# ========================================
# 10. RAW ENTRIES VS COUNTRY MEANS COMPARISON (not as country means)
# ========================================
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(GDP_per_capita)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = GDP_per_capita, y = TotalScore)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  stat_cor(method = "pearson", label.x.npc = "left") +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)", y = "Total Score for Survey 1 (Knowledge pathway)",
       title = "Raw entries", colour = "World Region") +
  theme(panel.grid.minor = element_blank())

ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AllResearchAndDev_percentGPD)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AllResearchAndDev_percentGPD, y = TotalScore)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  stat_cor(method = "pearson", label.x.npc = "left") +
  theme_minimal() +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Total Score for Survey 1 (Knowledge pathway)",
       title = "Raw entries - National R&D Investment (all sectors) vs Bat Research Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AgResearchAndDev_PPP2017)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgResearchAndDev_PPP2017, y = TotalScore)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  stat_cor(method = "pearson", label.x.npc = "left") +
  theme_minimal() +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Total Score for Survey 1 (Knowledge pathway)",
       title = "Raw entries - Agricultural R&D Investment vs Bat Research Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(EnviroPerformance_score)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = EnviroPerformance_score, y = TotalScore)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  stat_cor(method = "pearson", label.x.npc = "left") +
  theme_minimal() +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Total Score for Survey 1 (Knowledge pathway)",
       title = "Raw entries - Environmental Performance vs Bat Research Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())