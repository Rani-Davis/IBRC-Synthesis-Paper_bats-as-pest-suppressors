# Code author: Rani Davis
# Last updated: 20 July 2026
#
# ----------------------------------------
# Packages
# ----------------------------------------
library(ggrepel)      # geom_text_repel() -- non-overlapping country labels
library(ggpubr)       # stat_cor() -- correlation stats overlaid on plots
library(scales)       # dollar_format(), percent_format() -- axis label formatting
library(GGally)       # ggpairs() -- pairwise correlation matrix plot
library(tidyverse)   # dplyr, tidyr, ggplot2, purrr (map_df, pmap_df), readr (read_csv), tibble -- covers most of the pipeline


# ========================================
# JOIN INDICES TO SURVEY 2 DATA
# ========================================
Survey_2_long_indices <- Survey_2_long %>%
  mutate(Country.WB = recode(Country.clean, !!!country_recode)) %>%
  left_join(indices_coverage_check, by = "Country.WB")

# Check coverage
Survey_2_long_indices %>%
  summarise(
    n_gdp   = sum(!is.na(GDP_per_capita)),
    n_ag_va = sum(!is.na(AgForestryFish_ValueAdded_percentGDP)),
    n_rd    = sum(!is.na(AllResearchAndDev_percentGPD)),
    n_epi   = sum(!is.na(EnviroPerformance_score)),
    n_ag_rd = sum(!is.na(AgResearchAndDev_PPP2017))
  )

# Check unmatched countries
Survey_2_long_indices %>%
  filter(!is.na(Country.WB), is.na(GDP_per_capita)) %>%
  distinct(Country.WB)

write.csv(Survey_2_long_indices,"Analysis/clean data/Survey 2_scored_with indices.csv", row.names = FALSE)

# ========================================
# SUMMARISE TO COUNTRY LEVEL
# ========================================
country_indices_s2 <- Survey_2_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  group_by(Country.clean, Country.WB,
           GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
           AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017,
           Taiwan_imputed_from_China) %>%
  summarise(
    Country.Mean.of.RespondentTotalScore = mean(TotalScore.allInterventions, na.rm = TRUE),
    Country.Mean.of.RespondentMeanScore = mean(MeanScore.allInterventions, na.rm = TRUE),
    
    n = n_distinct(Respondent.entry.label),
    .groups = "drop"
  ) %>%
  mutate(Country.n.label = paste0(Country.clean, "\n(n=", n, ")"))

# ----------------------------------------
# Check all the data made it through - DIAGNOSTIC a: Baseline count of unique respondent-entries per country
# ----------------------------------------
# Respondent.entry.label uniquely identifies one study (one respondent-entry).
# - how many studies SHOULD show up per country.
baseline_counts_S2 <- Survey_2_long %>%
  filter(!is.na(Country.clean)) %>%
  distinct(Country.clean, Respondent.entry.label) %>%
  count(Country.clean, name = "n_studies_baseline") %>%
  arrange(desc(n_studies_baseline))
print(baseline_counts_S2, n = 30)

# Did the join change the number of respondent-entries per country?
post_join_counts_S2 <- Survey_2_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  distinct(Country.clean, Respondent.entry.label) %>%
  count(Country.clean, name = "n_studies_post_join")
print(post_join_counts_S2, n = 30)

# ----------------------------------------
# DIAGNOSTIC b: Does country_indices$n match the known-correct study counts?
# ----------------------------------------
compare_n_S2 <- post_join_counts_S2 %>%
  full_join(
    country_indices_s2 %>% select(Country.clean, n_in_country_indices = n),
    by = "Country.clean"
  ) %>%
  mutate(diff = n_in_country_indices - n_studies_post_join) %>%
  arrange(desc(abs(diff)))

print(compare_n_S2, n = 30)



# ========================================
# CORRELATION TABLE — total score vs indices
# ========================================
country_indices_s2 %>%
  select(Country.Mean.of.RespondentTotalScore , Country.Mean.of.RespondentMeanScore, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

country_indices_s2 %>%
  select(Country.Mean.of.RespondentTotalScore , Country.Mean.of.RespondentMeanScore, GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
         AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017) %>%
  ggpairs()





# ========================================
# CORRELATION TABLE — per intervention
# ========================================
interventions_s2 <- unique(Survey_2_long_indices$Score.type)

intervention_cor_table_s2 <- expand_grid(intervention = interventions_s2, index = indices) %>%
  pmap_df(function(intervention, index) {
    d <- Survey_2_long_indices %>%
      filter(Score.type == intervention, !is.na(.data[[index]]), !is.na(Score))
    if (nrow(d) < 5) return(NULL)
    test <- cor.test(d[[index]], d$Score, method = "pearson")
    tibble(
      Intervention = intervention,
      Index        = index,
      n            = nrow(d),
      r            = round(test$estimate, 3),
      p_value      = round(test$p.value, 3),
      sig          = case_when(
        test$p.value < 0.001 ~ "***",
        test$p.value < 0.01  ~ "**",
        test$p.value < 0.05  ~ "*",
        test$p.value < 0.1   ~ ".",
        TRUE                 ~ "ns"
      )
    )
  })

print(intervention_cor_table_s2, n = 30)


# ========================================
# PLOTS
# ========================================

# ----------------------------------------
# SURVEY 2 - COUNTRY LEVEL MEANS
# ----------------------------------------

# ---- GDP per capita (colour: blue #2166AC) ----

# GDP vs Total
ggplot(country_indices_s2 %>% filter(!is.na(GDP_per_capita)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_log10(labels = scales::dollar_format(scale = 1, prefix = "$")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "GDP per capita (USD)",
       y = "Total Intervention Score, summed across all interventions\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Country GDP vs Total Intervention Score")

# GDP vs Mean
ggplot(country_indices_s2 %>% filter(!is.na(GDP_per_capita)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_log10(labels = scales::dollar_format(scale = 1, prefix = "$")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "GDP per capita (USD)",
       y = "Average Intervention Score per Country\n(mean across interventions, averaged across respondents per country)") +
  ggtitle("Simple linear model - Country GDP vs Mean Intervention Score")


# ---- National R&D spending, all sectors (colour: red #D6604D) ----

# R&D vs Total
ggplot(country_indices_s2 %>% filter(!is.na(AllResearchAndDev_percentGPD)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Total Intervention Score, summed across all interventions\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - National R&D Investment (all sectors) vs Total Intervention Score")

# R&D vs Mean
ggplot(country_indices_s2 %>% filter(!is.na(AllResearchAndDev_percentGPD)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Average Intervention Score per Country\n(mean across interventions, averaged across respondents per country)") +
  ggtitle("Simple linear model - National R&D Investment (all sectors) vs Mean Intervention Score")


# ---- Agricultural economy dependence (colour: green #4DAF4A) ----

# Ag Economy vs Total
ggplot(country_indices_s2 %>% filter(!is.na(AgForestryFish_ValueAdded_percentGDP)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Total Intervention Score, summed across all interventions\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Agricultural Economy Dependence vs Total Intervention Score")

# Ag Economy vs Mean
ggplot(country_indices_s2 %>% filter(!is.na(AgForestryFish_ValueAdded_percentGDP)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Average Intervention Score per Country\n(mean across interventions, averaged across respondents per country)") +
  ggtitle("Simple linear model - Agricultural Economy Dependence vs Mean Intervention Score")


# ---- Agricultural R&D investment (colour: purple #984EA3) ----

# Ag R&D vs Total
ggplot(country_indices_s2 %>% filter(!is.na(AgResearchAndDev_PPP2017)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Total Intervention Score, summed across all interventions\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Agricultural R&D Investment vs Total Intervention Score")

# Ag R&D vs Mean
ggplot(country_indices_s2 %>% filter(!is.na(AgResearchAndDev_PPP2017)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Average Intervention Score per Country\n(mean across interventions, averaged across respondents per country)") +
  ggtitle("Simple linear model - Agricultural R&D Investment vs Mean Intervention Score")


# ---- Environmental Performance Index (colour: orange #FF7F00) ----

# EPI vs Total
ggplot(country_indices_s2 %>% filter(!is.na(EnviroPerformance_score)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Total Intervention Score, summed across all interventions\n(country mean of respondent totals)") +
  ggtitle("Simple linear model - Environmental Performance vs Total Intervention Score")

# EPI vs Mean
ggplot(country_indices_s2 %>% filter(!is.na(EnviroPerformance_score)),
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
  scale_size_area(name = "n entries", max_size = 12) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Average Intervention Score per Country\n(mean across interventions, averaged across respondents per country)") +
  ggtitle("Simple linear model - Environmental Performance vs Mean Intervention Score")


# ========================================
# SURVEY 2 - RAW ENTRIES VS INDEX
# ========================================
# stat_cor omitted: respondents within a country share the same x-value,
# so respondent-level points aren't independent observations (pseudoreplication).

# ---- GDP per capita (index colour: blue #2166AC) ----

# GDP vs Total (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(GDP_per_capita)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = GDP_per_capita, y = TotalScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#2166AC", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)",
       y = "Total Intervention Score\n(summed across all interventions, per respondent)",
       title = "Raw entries - Country GDP vs Total Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# GDP vs Mean (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(GDP_per_capita)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = GDP_per_capita, y = MeanScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#2166AC", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  labs(x = "GDP per capita (log, USD)",
       y = "Average Intervention Score\n(mean across interventions, per respondent)",
       title = "Raw entries - Country GDP vs Mean Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- National R&D spending, all sectors (index colour: red #D6604D) ----

# R&D vs Total (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(AllResearchAndDev_percentGPD)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AllResearchAndDev_percentGPD, y = TotalScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#D6604D", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Total Intervention Score\n(summed across all interventions, per respondent)",
       title = "Raw entries - National R&D Investment (all sectors) vs Total Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# R&D vs Mean (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(AllResearchAndDev_percentGPD)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AllResearchAndDev_percentGPD, y = MeanScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#D6604D", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 5, by = 0.5)) +
  theme_minimal() +
  labs(x = "National R&D spending (all sectors) (% of GDP)",
       y = "Average Intervention Score\n(mean across interventions, per respondent)",
       title = "Raw entries - National R&D Investment (all sectors) vs Mean Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- Agricultural economy dependence (index colour: green #4DAF4A) ----

# Ag Economy vs Total (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(AgForestryFish_ValueAdded_percentGDP)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgForestryFish_ValueAdded_percentGDP, y = TotalScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#4DAF4A", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Total Intervention Score\n(summed across all interventions, per respondent)",
       title = "Raw entries - Agricultural Economy Dependence vs Total Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# Ag Economy vs Mean (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(AgForestryFish_ValueAdded_percentGDP)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgForestryFish_ValueAdded_percentGDP, y = MeanScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#4DAF4A", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 80, by = 2)) +
  theme_minimal() +
  labs(x = "Agriculture, Forestry & Fisheries as % of GDP",
       y = "Average Intervention Score\n(mean across interventions, per respondent)",
       title = "Raw entries - Agricultural Economy Dependence vs Mean Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- Agricultural R&D investment (index colour: purple #984EA3) ----

# Ag R&D vs Total (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(AgResearchAndDev_PPP2017)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgResearchAndDev_PPP2017, y = TotalScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#984EA3", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Total Intervention Score\n(summed across all interventions, per respondent)",
       title = "Raw entries - Agricultural R&D Investment vs Total Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# Ag R&D vs Mean (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(AgResearchAndDev_PPP2017)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = AgResearchAndDev_PPP2017, y = MeanScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#984EA3", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_log10(labels = scales::dollar_format(prefix = "$", suffix = "M", scale = 1)) +
  theme_minimal() +
  labs(x = "Public Ag R&D spending (million 2017 Purchasing Power Parity)",
       y = "Average Intervention Score\n(mean across interventions, per respondent)",
       title = "Raw entries - Agricultural R&D Investment vs Mean Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())


# ---- Environmental Performance Index (index colour: orange #FF7F00) ----

# EPI vs Total (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(EnviroPerformance_score)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = EnviroPerformance_score, y = TotalScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#FF7F00", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Total Intervention Score\n(summed across all interventions, per respondent)",
       title = "Raw entries - Environmental Performance vs Total Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())

# EPI vs Mean (raw)
ggplot(
  Survey_2_long_indices %>%
    filter(!is.na(EnviroPerformance_score)) %>%
    distinct(Respondent.entry.label, .keep_all = TRUE),
  aes(x = EnviroPerformance_score, y = MeanScore.allInterventions)) +
  geom_jitter(aes(colour = World.region.clean),
              width = 0, height = 0.15, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#FF7F00", linewidth = 0.8) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  theme_minimal() +
  labs(x = "Environmental Performance Index (0-100)",
       y = "Average Intervention Score\n(mean across interventions, per respondent)",
       title = "Raw entries - Environmental Performance vs Mean Intervention Score",
       colour = "World Region") +
  theme(panel.grid.minor = element_blank())



# Correlation heatmap — intervention x index
intervention_cor_table_s2 %>%
  mutate(Index = recode(Index,
                        "GDP_per_capita"                        = "GDP per capita (USD)",
                        "AgForestryFish_ValueAdded_percentGDP"  = "Agriculture as % of GDP",
                        "AllResearchAndDev_percentGPD"          = "National R&D spending (% of GDP)",
                        "EnviroPerformance_score"                = "Environmental Performance Index",
                        "AgResearchAndDev_PPP2017"               = "Public Ag R&D spending (PPP$)"
  )) %>%
  ggplot(aes(x = Index, y = Intervention, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(r, "\n", sig)), size = 3) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(x = NULL, y = "Intervention", fill = "Pearson r",
       title = "Country-level indices vs intervention testing scores",
       caption = "Scores are per respondent entry (0-3 scale); country indices joined by respondent country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(size = 8, hjust = 0))
