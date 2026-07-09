# R Script created by Rani Davis
# Last updated 16.6.26
# ----------------------------------------
# Packages
# ----------------------------------------
library(tidyverse)
library(WDI)         # World Bank data
library(ggrepel)     # non-overlapping plot labels
library(scales)      # axis formatting
library(ggpubr)      # stat_cor for correlation on plots
library(readxl)      # read Excel files
library(GGally)      # ggpairs correlation matrix
library(broom)       # tidy model outputs
library(patchwork)   # combine plots
library(OECD)        # OECD data (optional)
library(FAOSTAT)     # FAO data (optional - requires login)


# ========================================
# 1. PULL WORLD BANK GDP DATA
# ========================================
gdp_data <- WDI(indicator = "NY.GDP.PCAP.CD",
                start = 2022, end = 2023,
                extra = TRUE) %>%
  filter(year == 2023) %>%
  select(country, iso2c, gdp_per_capita = NY.GDP.PCAP.CD)

# Can search through different indices online at https://data.worldbank.org/indicator

# Look up specific indices
WDIsearch(string = "NV.AGR.TOTL.ZS", field = "indicator", cache = NULL, short = FALSE)
WDIsearch(string = "GB.XPD.RSDV.GD.ZS", field = "indicator", cache = NULL, short = FALSE)

# Pull WDI ag value added and R&D spending
wdi_raw <- WDI(
  indicator = c(
    ag_value_added_pct = "NV.AGR.TOTL.ZS",
    rd_spend_pct       = "GB.XPD.RSDV.GD.ZS"
  ),
  start = 2018, end = 2023,
  extra = TRUE
)

wdi_indicators <- wdi_raw %>%
  group_by(country) %>%
  summarise(
    ag_value_added_pct = last(na.omit(ag_value_added_pct)),
    rd_spend_pct       = last(na.omit(rd_spend_pct)),
    .groups = "drop"
  ) %>%
  select(country, ag_value_added_pct, rd_spend_pct)


# ========================================
# 2. PULL EPI DATA
# Yale Environmental Performance Index
# Score 0-100, higher = better environmental performance
# Good proxy for environmental consciousness / policy maturity
# ========================================
epi_data <- read_csv("https://epi.yale.edu/downloads/epi2024results.csv") %>%
  select(country = country, epi_score = EPI.new)
# If URL fails, download manually from https://epi.yale.edu and read locally:
# epi_data <- read_csv("epi2024results.csv") %>% select(country, epi_score = EPI.new)


# ========================================
# 3. PULL GRAPE AG R&D DATA
# WUR-ERS Global Research on Agriculture: Personnel & Expenditures
# Download grape_v1.0.0.xlsx dataset directly from zenodo.org/records/15828189
# ========================================
grape_raw <- read_xlsx("grape_v1.0.0.xlsx")

# Filter to RD (expenditure) series, most recent year per country
grape_clean <- grape_raw %>%
  filter(variable == "RD") %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarise(
    ag_rd_spend = first(value),  # ag R&D in 2017 PPP$
    ag_rd_year  = first(year),
    iso3c       = first(iso3c),
    .groups = "drop"
  )


# ========================================
# 4. FAOSTAT (optional - but would require an account login, which I dont have...)
# https://www.fao.org/faostat
# ========================================
# faostat_login(username = "your@email.com", password = "yourpassword")
# search_dataset("land")


# ========================================
# 5. IMPORT SURVEY 1 DATA (long format)
# ========================================
Survey_1_long_wGDP <- read_csv("Survey 1_scored_near complete_long_16.6.26.csv")

# Recode country names to match World Bank naming
Survey_1_long_wGDP <- Survey_1_long_wGDP %>%
  mutate(Country.WB = recode(Country.clean,
                             "USA"                     = "United States",
                             "UK"                      = "United Kingdom",
                             "Peru & Colombia"         = "Peru",           # spans two countries - assigned to first
                             "Australia & New Zealand" = "Australia",      # spans two countries - assigned to first
                             "Taiwan"                  = "Taiwan, China"   # World Bank name
  )) %>%
  left_join(gdp_data, by = c("Country.WB" = "country"))


# ========================================
# 6. JOIN ALL INDICES TO SURVEY DATA
# ========================================
Survey_1_long_indices <- Survey_1_long_wGDP %>%
  mutate(Country.GRAPE = recode(Country.WB,
                                "Taiwan, China" = "Taiwan"  # GRAPE uses "Taiwan" not "Taiwan, China"
  )) %>%
  left_join(wdi_indicators, by = c("Country.WB" = "country")) %>%
  left_join(epi_data,       by = c("Country.WB" = "country")) %>%
  left_join(grape_clean,    by = c("Country.GRAPE" = "country"))

# Check coverage - how many rows have each index
Survey_1_long_indices %>%
  summarise(
    n_gdp   = sum(!is.na(gdp_per_capita)),
    n_ag_va = sum(!is.na(ag_value_added_pct)),
    n_rd    = sum(!is.na(rd_spend_pct)),
    n_epi   = sum(!is.na(epi_score)),
    n_ag_rd = sum(!is.na(ag_rd_spend))
  )

# Check for unmatched countries in GRAPE
Survey_1_long_indices %>%
  filter(!is.na(Country.WB), is.na(ag_rd_spend)) %>%
  distinct(Country.WB)


# ========================================
# 7. SUMMARISE TO COUNTRY LEVEL FOR PLOTTING
# ========================================
country_indices <- Survey_1_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  group_by(Country.clean, Country.WB,
           gdp_per_capita, ag_value_added_pct,
           rd_spend_pct, epi_score, ag_rd_spend) %>%
  summarise(
    Mean.Total.Score = mean(TotalScore, na.rm = TRUE),
    n = n_distinct(Respondent.entry.label),
    .groups = "drop"
  ) %>%
  mutate(Country.n.label = paste0(Country.clean, "\n(n=", n, ")"))


# ========================================
# 8. EXPLORE COLLINEARITY BETWEEN INDICES
# ========================================
# Correlation matrix
country_indices %>%
  select(Mean.Total.Score, gdp_per_capita, ag_value_added_pct,
         rd_spend_pct, epi_score, ag_rd_spend) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

# Pairwise plot matrix
country_indices %>%
  select(Mean.Total.Score, gdp_per_capita, ag_value_added_pct,
         rd_spend_pct, epi_score, ag_rd_spend) %>%
  ggpairs()

# Correlation table with significance
indices <- c("gdp_per_capita", "ag_value_added_pct",
             "rd_spend_pct", "epi_score", "ag_rd_spend")

s1xIndice_cor_table <- map_df(indices, function(var) {
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

print(s1xIndice_cor_table)
# Significant correlations include:
# - ag_value_added_pct: Agriculture as % of GDP (negative - wealthier, more
#   industrialised countries have smaller ag share of GDP and more research capacity)
# - epi_score: Environmental Performance Index (positive - environmentally
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

# Helper function - plots country means with r and p value
plot_index <- function(data, x_var, x_label) {
  ggplot(data %>% filter(!is.na(.data[[x_var]])),
         aes(x = .data[[x_var]], y = Mean.Total.Score,
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
    theme_minimal() +
    labs(x = x_label, y = "Mean Total Score\nalong Knowledge Pathway") +
    theme(axis.text = element_text(size = 10),
          panel.grid.minor = element_blank())
}

# GDP per capita (log scale)
plot_index(country_indices, "gdp_per_capita", "GDP per capita (USD)") +
  scale_x_log10(labels = scales::dollar_format(scale = 1, prefix = "$")) +
  ggtitle("Country GDP vs Bat Research Score")

# Agriculture % of GDP
# Negative: wealthier, industrialised countries have smaller ag share of GDP
# and likely more research capacity and bat knowledge
plot_index(country_indices, "ag_value_added_pct", "Agriculture as % of GDP") +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 10)) +
  ggtitle("Agricultural Economy Dependence vs Bat Research Score")

# General R&D spending % of GDP
# Flat: general R&D spending doesn't predict bat-agriculture knowledge
plot_index(country_indices, "rd_spend_pct", "National R&D spending (% of GDP)") +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  ggtitle("National R&D Investment (all sectors) vs Bat Research Score")

# EPI score
plot_index(country_indices, "epi_score", "Environmental Performance Index (0-100)") +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  ggtitle("Environmental Performance vs Bat Research Score")

# Ag R&D spending (log scale)
plot_index(country_indices, "ag_rd_spend", "Public Ag R&D spending (million 2017 PPP$)") +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  ggtitle("Agricultural R&D Investment vs Bat Research Score")


# ========================================
# 10. RAW ENTRIES VS COUNTRY MEANS COMPARISON (GDP example)
# ========================================
# Raw entries plot - one point per respondent entry, coloured by world region
p_raw <- ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(gdp_per_capita)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = gdp_per_capita, y = TotalScore)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  stat_cor(method = "pearson", label.x.npc = "left") +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)", y = "Total Score",
       title = "Raw entries", colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# Country means plot - one point per country, sized by n entries
p_country <- ggplot(
  country_indices %>% filter(!is.na(gdp_per_capita)),
  aes(x = gdp_per_capita, y = Mean.Total.Score)) +
  geom_point(aes(size = n, colour = Country.clean), alpha = 0.7) +
  geom_text_repel(aes(label = Country.n.label), size = 2.5,
                  box.padding = 0.4, segment.colour = "grey60") +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40", linewidth = 0.8) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  stat_cor(method = "pearson", label.x.npc = "left") +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)", y = "Mean Total Score",
       title = "Country means (sized by n entries)") +
  theme(panel.grid.minor = element_blank())

# Side by side comparison
p_raw + p_country + plot_layout(guides = "collect")
