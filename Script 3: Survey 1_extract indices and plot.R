# Code author: Rani Davis
# Last updated: 6 July 2026

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

# Indices datasets
library(WDI)         # World Bank data
library(OECD)        # OECD data (optional)
library(FAOSTAT)     # FAO data (optional - requires login)


# ========================================
# 1. PULL WORLD BANK GDP DATA
# ========================================
gdp_data <- WDI(indicator = "NY.GDP.PCAP.CD",
                start = 2022, end = 2023,
                extra = TRUE) %>%
  filter(year == 2023) %>%
  select(country, iso2c, GDP_per_capita = NY.GDP.PCAP.CD)

# Can search through different indices online at https://data.worldbank.org/indicator

# Look up specific indices
WDIsearch(string = "NV.AGR.TOTL.ZS", field = "indicator", cache = NULL, short = FALSE)
WDIsearch(string = "GB.XPD.RSDV.GD.ZS", field = "indicator", cache = NULL, short = FALSE)

# Pull WDI ag value added and R&D spending
wdi_raw <- WDI(
  indicator = c(
    AgForestryFish_ValueAdded_percentGDP = "NV.AGR.TOTL.ZS",
    AllResearchAndDev_percentGPD       = "GB.XPD.RSDV.GD.ZS"
  ),
  start = 2018, end = 2023,
  extra = TRUE
)

wdi_indicators <- wdi_raw %>%
  group_by(country) %>%
  summarise(
    AgForestryFish_ValueAdded_percentGDP = last(na.omit(AgForestryFish_ValueAdded_percentGDP)),
    AllResearchAndDev_percentGPD       = last(na.omit(AllResearchAndDev_percentGPD )),
    .groups = "drop"
  ) %>%
  select(country, AgForestryFish_ValueAdded_percentGDP,AllResearchAndDev_percentGPD )


# ========================================
# 2. PULL EPI DATA
# Yale Environmental Performance Index
# Score 0-100, higher = better environmental performance
# Good proxy for environmental consciousness / policy maturity
# ========================================
epi_data <- read_csv("https://epi.yale.edu/downloads/epi2024results.csv") %>%
  select(country = country, EnviroPerformance_score = EPI.new)
# If URL fails, download manually from https://epi.yale.edu and read locally:
# epi_data <- read_csv("epi2024results.csv") %>% select(country, epi_score = EPI.new)


# ========================================
# 3. PULL GRAPE AG R&D DATA
# WUR-ERS Global Research on Agriculture: Personnel & Expenditures
# Download grape_v1.0.0.xlsx dataset directly from zenodo.org/records/15828189
# ========================================
grape_raw <- read_xlsx("raw data/grape_v1.0.0.xlsx")

# Filter to RD (expenditure) series, most recent year per country
grape_clean <- grape_raw %>%
  filter(variable == "RD") %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarise(
    AgResearchAndDev_PPP2017 = first(value),  # ag R&D in 2017 PPP$ = Purchasing Power Parity for 2017
    AgResearchAndDev_year  = first(year),
    iso3c       = first(iso3c), # iso3c = 3 letter code for each country
    .groups = "drop"
  )


# ========================================
# 4. FAOSTAT (optional - but would require an account login, which I dont have...)
# https://www.fao.org/faostat
# ========================================
# faostat_login(username = "your@email.com", password = "yourpassword")
# search_dataset("land")

does 

# ========================================
# 8. EXPLORE COLLINEARITY BETWEEN INDICES
# ========================================
# Correlation matrix
country_indices %>%
  select(Mean.Total.Score, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

# Pairwise plot matrix
country_indices %>%
  select(Mean.Total.Score, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  ggpairs()

# Correlation table with significance
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
# Significant correlations include:
# - AgForestryFish_ValueAdded_percentGDP: Agriculture, Forestry & Fisheries as % of GDP (negative - wealthier, more
#   industrialised countries have smaller ag share of GDP and more research capacity)
# - EnviroPerformance_score: Environmental Performance Index (positive - environmentally
#   conscious countries score higher)


# ========================================
# 9. PLOT EACH INDEX VS MEAN TOTAL SCORE (country means)
# ========================================
# Colour palette for world regions
region_colours <- c(
  "Europe"                     = "#2166AC",
  "Africa"                     = "#D6604D",
  "Latin America & Caribbean"  = "#33A02C",
  "Australasia / Pacific"      = "#00B4D8",
  "North America"              = "#7B2D8B",
  "Middle East / Western Asia" = "#FF8C00",
  "South Asia"                 = "#E7298A",
  "East Asia"                  = "#E6C619",
  "Southeast Asia"             = "#7DB82A"
)

# ----------------------------------------
# GDP per capita (log scale)
# ----------------------------------------
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

# ----------------------------------------
# General R&D spending % of GDP
# Flat: general R&D spending doesn't predict bat-agriculture knowledge
# ----------------------------------------
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

# ----------------------------------------
# Agriculture % of GDP
# Negative: wealthier, industrialised countries have smaller ag share of GDP
# and likely more research capacity and bat knowledge
# ----------------------------------------
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

# ----------------------------------------
# Ag R&D spending (log scale)
# ----------------------------------------
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

# ----------------------------------------
# EPI score
# ----------------------------------------
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
# Raw entries plot - one point per respondent entry, coloured by world region
# To see spread of total scores

# GDP
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

# All R&D spending
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

# Ag R&D
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

# Environmental Performance index
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

