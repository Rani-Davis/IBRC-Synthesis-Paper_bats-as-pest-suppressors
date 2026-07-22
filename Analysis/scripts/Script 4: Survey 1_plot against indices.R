# Code author: Rani Davis
# Last updated: 16 July 2026

library(ggrepel)      # geom_text_repel() -- non-overlapping country labels
library(ggpubr)       # stat_cor() -- correlation stats overlaid on plots
library(scales)       # dollar_format(), percent_format() -- axis label formatting
library(GGally)       # ggpairs() -- pairwise correlation matrix plot
library(tidyverse)   # dplyr, tidyr, ggplot2, purrr (map_df, pmap_df), readr (read_csv), tibble -- covers most of the pipeline


# ========================================
# 1. JOIN INDICES TO SURVEY 1 DATA (single join, using the patched coverage table)
# ========================================
Survey_1_long_indices <- Survey_1_long %>%
  mutate(Country.WB = recode(Country.clean, !!!country_recode)) %>%
  left_join(indices_coverage_check, by = "Country.WB")

# Check coverage
Survey_1_long_indices %>%
  summarise(
    n_gdp   = sum(!is.na(GDP_per_capita)),
    n_ag_va = sum(!is.na(AgForestryFish_ValueAdded_percentGDP)),
    n_rd    = sum(!is.na(AllResearchAndDev_percentGPD)),
    n_epi   = sum(!is.na(EnviroPerformance_score)),
    n_ag_rd = sum(!is.na(AgResearchAndDev_PPP2017))
  )

Survey_1_long_indices %>%
  filter(!is.na(Country.WB), is.na(GDP_per_capita)) %>%
  distinct(Country.WB)

write.csv(Survey_1_long_indices,"Analysis/clean data/Survey 1_scored_with indices.csv", row.names = FALSE)


# ========================================
# 2. SUMMARISE TO COUNTRY LEVEL
# ========================================
# NOTE ON GEOMETRIC MEAN: Geometric mean scores are NOT included here.
# 22% of raw item scores are exactly 0 (checked via mean(Score == 0))
# We could add 1 to every score to use geometric mean.
# However geometric mean is designed for multiplicative/ratio-based data (e.g. growth rates), 
# and these scores are additive with along the pathway.

country_indices <- Survey_1_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  # Collapse long-format data down to ONE row per unique respondent-entry before averaging. 
  # Without this step, TotalScore.allSteps/MeanScore.allSteps values ...
  # ...   (which are repeated across every study system-row for a given respondent-entry) 
  # get counted once per item when taking the country mean
  distinct(Respondent.clean, Respondent.entry.ID, .keep_all = TRUE) %>%
  group_by(Country.clean, Country.WB,
           GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
           AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017,
           Taiwan_imputed_from_China) %>%
  summarise(
    
    # Country-level average of respondents' TOTAL scores (summed across all items each respondent answered), 
    # computed on one value per respondent-entry.
    # CAUTION: still confounded with number of systems per respondent, which varies
    # widely - respondents who answered more times will tend to have higher totals
    # regardless of per-item performance.
    Country.Mean.of.RespondentTotalScore = mean(TotalScore.allSteps, na.rm = TRUE),
    
    # Country-level average of respondents' MEAN scores (each respondent's own average score across the items THEY answered), 
    # computed on one value per respondent-entry. 
    # This is the preferred, fairer metric - it puts respondents who answered few items on the same scale as respondents who answered many.
    Country.Mean.of.RespondentMeanScore = mean(MeanScore.allSteps, na.rm = TRUE),
    
    # Number of unique respondent-entries contributing to this country's summary
    n = n_distinct(Respondent.entry.label),
    
    .groups = "drop"
  ) %>%
  mutate(Country.n.label = paste0(Country.clean, "\n(n=", n, ")"))

# ========================================
# 3. CHECK ALL OF THE DATA MADE IT THROUGH
# ========================================
# ----------------------------------------
# DIAGNOSTIC a: Baseline count of unique respondent-entries per country
# ----------------------------------------
# Respondent.entry.label uniquely identifies one study (one respondent-entry).
# - how many studies SHOULD show up per country.
baseline_counts_S1 <- Survey_1_long %>%
  filter(!is.na(Country.clean)) %>%
  distinct(Country.clean, Respondent.entry.label) %>%
  count(Country.clean, name = "n_studies_baseline") %>%
  arrange(desc(n_studies_baseline))
print(baseline_counts_S1, n = 30)

# Did the join change the number of respondent-entries per country?
post_join_counts_S1 <- Survey_1_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  distinct(Country.clean, Respondent.entry.label) %>%
  count(Country.clean, name = "n_studies_post_join")
print(post_join_counts_S1, n = 30)

# ----------------------------------------
# DIAGNOSTIC b: Does country_indices$n match the known-correct study counts?
# ----------------------------------------
compare_n_S1 <- post_join_counts_S1 %>%
  full_join(
    country_indices %>% select(Country.clean, n_in_country_indices = n),
    by = "Country.clean"
  ) %>%
  mutate(diff = n_in_country_indices - n_studies_post_join) %>%
  arrange(desc(abs(diff)))

print(compare_n_S1, n = 30) # good, they are the same.



# ========================================
# 4. EXPLORE COLLINEARITY BETWEEN INDICES
# ========================================
country_indices %>%
  select(Country.Mean.of.RespondentTotalScore , Country.Mean.of.RespondentMeanScore, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

country_indices %>%
  select(Country.Mean.of.RespondentTotalScore , Country.Mean.of.RespondentMeanScore, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  ggpairs()

indices <- c("GDP_per_capita", "AgForestryFish_ValueAdded_percentGDP",
             "AllResearchAndDev_percentGPD", "EnviroPerformance_score", "AgResearchAndDev_PPP2017")


# ========================================
# 4. Visualise country-level means of total and mean scores across the entire pathway, plus system-level means and total scores
# ========================================
# ----------------------------------------
# FOR COUNTRY LEVEL MEANS (mean total score, 'mean mean' score)
# ----------------------------------------
# ---- GDP per capita (colour: blue #2166AC) ----

# GDP vs Total
ggplot(country_indices %>% filter(!is.na(GDP_per_capita)),
       aes(x = GDP_per_capita, y = Country.Mean.of.RespondentTotalScore,
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
       y = "Total Knowledge Pathway Score, summed across 5 steps\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Country GDP vs Total Knowledge Pathway Score")

# GDP vs Mean
ggplot(country_indices %>% filter(!is.na(GDP_per_capita)),
       aes(x = GDP_per_capita, y = Country.Mean.of.RespondentMeanScore,
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
  scale_x_log10(labels = scales::dollar_format(scale = 1, prefix = "$")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "GDP per capita (USD)",
       y = "Average Knowledge Pathway Score per Country\n(mean across 5 steps, averaged across respondents per country)") +
  ggtitle("Simple linear model - Country GDP vs Mean Knowledge Pathway Score")


# ---- National R&D spending, all sectors (colour: red #D6604D) ----

# R&D vs Total
ggplot(country_indices %>% filter(!is.na(AllResearchAndDev_percentGPD)),
       aes(x = AllResearchAndDev_percentGPD, y = Country.Mean.of.RespondentTotalScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#D6604D") +
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
       y = "Total Knowledge Pathway Score, summed across 5 steps\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - National R&D Investment (all sectors) vs Total Knowledge Pathway Score")

# R&D vs Mean
ggplot(country_indices %>% filter(!is.na(AllResearchAndDev_percentGPD)),
       aes(x = AllResearchAndDev_percentGPD, y = Country.Mean.of.RespondentMeanScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#D6604D") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Average Knowledge Pathway Score per Country\n(mean across 5 steps, averaged across respondents per country)") +
  ggtitle("Simple linear model - National R&D Investment (all sectors) vs Mean Knowledge Pathway Score")


# ---- Agricultural economy dependence (colour: green #4DAF4A) ----

# Ag Economy vs Total
ggplot(country_indices %>% filter(!is.na(AgForestryFish_ValueAdded_percentGDP)),
       aes(x = AgForestryFish_ValueAdded_percentGDP, y = Country.Mean.of.RespondentTotalScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#4DAF4A") +
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
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Total Knowledge Pathway Score, summed across 5 steps\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Agricultural Economy Dependence vs Total Knowledge Pathway Score")

# Ag Economy vs Mean
ggplot(country_indices %>% filter(!is.na(AgForestryFish_ValueAdded_percentGDP)),
       aes(x = AgForestryFish_ValueAdded_percentGDP, y = Country.Mean.of.RespondentMeanScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#4DAF4A") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Average Knowledge Pathway Score per Country\n(mean across 5 steps, averaged across respondents per country)") +
  ggtitle("Simple linear model - Agricultural Economy Dependence vs Mean Knowledge Pathway Score")


# ---- Agricultural R&D investment (colour: purple #984EA3) ----

# Ag R&D vs Total
ggplot(country_indices %>% filter(!is.na(AgResearchAndDev_PPP2017)),
       aes(x = AgResearchAndDev_PPP2017, y = Country.Mean.of.RespondentTotalScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#984EA3") +
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
       y = "Total Knowledge Pathway Score, summed across 5 steps\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Agricultural R&D Investment vs Total Knowledge Pathway Score")

# Ag R&D vs Mean
ggplot(country_indices %>% filter(!is.na(AgResearchAndDev_PPP2017)),
       aes(x = AgResearchAndDev_PPP2017, y = Country.Mean.of.RespondentMeanScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#984EA3") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Average Knowledge Pathway Score per Country\n(mean across 5 steps, averaged across respondents per country)") +
  ggtitle("Simple linear model - Agricultural R&D Investment vs Mean Knowledge Pathway Score")


# ---- Environmental Performance Index (colour: orange #FF7F00) ----

# EPI vs Total
ggplot(country_indices %>% filter(!is.na(EnviroPerformance_score)),
       aes(x = EnviroPerformance_score, y = Country.Mean.of.RespondentTotalScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#FF7F00") +
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
       y = "Total Knowledge Pathway Score, summed across 5 steps\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Environmental Performance vs Total Knowledge Pathway Score")

# EPI vs Mean
ggplot(country_indices %>% filter(!is.na(EnviroPerformance_score)),
       aes(x = EnviroPerformance_score, y = Country.Mean.of.RespondentMeanScore,
           size = n, label = Country.n.label)) +
  geom_point(alpha = 0.7, colour = "#FF7F00") +
  geom_text_repel(size = 2.8, box.padding = 0.4, point.padding = 0.3,
                  max.overlaps = 20, segment.colour = "grey60",
                  segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              colour = "grey40", show.legend = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",
           label.y.npc = "top", size = 3.5) +
  scale_size_continuous(name = "n entries", range = c(3, 10)) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Average Knowledge Pathway Score per Country\n(mean across 5 steps, averaged across respondents per country)") +
  ggtitle("Simple linear model - Environmental Performance vs Mean Knowledge Pathway Score")


# ----------------------------------------
# RAW ENTRIES VS COUNTRY MEANS COMPARISON (not as country means)
# ----------------------------------------
# stat_cor is intentionally omitted.
# every respondent within a country shares the same x-value (that country's index score), 
# so respondent-level points are not independent observations, meaning a correlation test  would be pseudoreplicated 

# ---- GDP per capita (index colour: blue #2166AC) ----

# GDP vs Total (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(GDP_per_capita)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = GDP_per_capita, y = TotalScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#2166AC", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)",
       y = "Total Knowledge Pathway Score\n(summed across 5 steps, per respondent)",
       title = "Raw entries - Country GDP vs Total Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# GDP vs Mean (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(GDP_per_capita)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = GDP_per_capita, y = MeanScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#2166AC", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)",
       y = "Average Knowledge Pathway Score\n(mean across 5 steps, per respondent)",
       title = "Raw entries - Country GDP vs Mean Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- National R&D spending, all sectors (index colour: red #D6604D) ----

# R&D vs Total (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AllResearchAndDev_percentGPD)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AllResearchAndDev_percentGPD, y = TotalScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#D6604D", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Total Knowledge Pathway Score\n(summed across 5 steps, per respondent)",
       title = "Raw entries - National R&D Investment (all sectors) vs Total Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# R&D vs Mean (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AllResearchAndDev_percentGPD)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AllResearchAndDev_percentGPD, y = MeanScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#D6604D", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Average Knowledge Pathway Score\n(mean across 5 steps, per respondent)",
       title = "Raw entries - National R&D Investment (all sectors) vs Mean Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- Agricultural economy dependence (index colour: green #4DAF4A) ----

# Ag Economy vs Total (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AgForestryFish_ValueAdded_percentGDP)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgForestryFish_ValueAdded_percentGDP, y = TotalScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#4DAF4A", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Total Knowledge Pathway Score\n(summed across 5 steps, per respondent)",
       title = "Raw entries - Agricultural Economy Dependence vs Total Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# Ag Economy vs Mean (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AgForestryFish_ValueAdded_percentGDP)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgForestryFish_ValueAdded_percentGDP, y = MeanScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#4DAF4A", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Average Knowledge Pathway Score\n(mean across 5 steps, per respondent)",
       title = "Raw entries - Agricultural Economy Dependence vs Mean Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- Agricultural R&D investment (index colour: purple #984EA3) ----

# Ag R&D vs Total (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AgResearchAndDev_PPP2017)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgResearchAndDev_PPP2017, y = TotalScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#984EA3", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Total Knowledge Pathway Score\n(summed across 5 steps, per respondent)",
       title = "Raw entries - Agricultural R&D Investment vs Total Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# Ag R&D vs Mean (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(AgResearchAndDev_PPP2017)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgResearchAndDev_PPP2017, y = MeanScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#984EA3", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Average Knowledge Pathway Score\n(mean across 5 steps, per respondent)",
       title = "Raw entries - Agricultural R&D Investment vs Mean Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- Environmental Performance Index (index colour: orange #FF7F00) ----

# EPI vs Total (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(EnviroPerformance_score)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = EnviroPerformance_score, y = TotalScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#FF7F00", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Total Knowledge Pathway Score\n(summed across 5 steps, per respondent)",
       title = "Raw entries - Environmental Performance vs Total Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# EPI vs Mean (raw)
ggplot(
  Survey_1_long_indices %>%
    filter(!is.na(EnviroPerformance_score)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = EnviroPerformance_score, y = MeanScore.allSteps)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#FF7F00", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Average Knowledge Pathway Score\n(mean across 5 steps, per respondent)",
       title = "Raw entries - Environmental Performance vs Mean Knowledge Pathway Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())
