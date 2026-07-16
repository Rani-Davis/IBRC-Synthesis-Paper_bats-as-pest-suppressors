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

#write.csv(Survey_1_long_indices,"Analysis/clean data/Survey 1_scored_with indices.csv", row.names = FALSE)



# ========================================
# 2. SUMMARISE TO COUNTRY LEVEL
# ========================================
country_indices <- Survey_1_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  group_by(Country.clean, Country.WB,
           GDP_per_capita, AgForestryFish_ValueAdded_percentGDP,
           AllResearchAndDev_percentGPD, EnviroPerformance_score, AgResearchAndDev_PPP2017,
           Taiwan_imputed_from_China) %>%
  summarise(
    Mean.Total.Score = mean(TotalScore, na.rm = TRUE),
    n = n_distinct(Respondent.ID),
    .groups = "drop"
  ) %>%
  mutate(Country.n.label = paste0(Country.clean, "\n(n=", n, ")"))


# ========================================
# 3. EXPLORE COLLINEARITY BETWEEN INDICES
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

========================================
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
  ggtitle("Simple linear model - Country GDP vs Knowledge Pathway Score")

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
  ggtitle("Simple linear model - National R&D Investment (all sectors) vs Knowledge Pathway Score")

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
  ggtitle("Simple linear model - Agricultural Economy Dependence vs Knowledge Pathway Score")

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
  ggtitle("Simple linear model - Agricultural R&D Investment vs Knowledge Pathway Score")

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
  ggtitle("Simple linear model - Environmental Performance vs Knowledge Pathway Score")


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