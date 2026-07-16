# Code author: Rani Davis
# Last updated: 16 July 2026
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

#write.csv(Survey_2_long_indices,"Analysis/clean data/Survey 2_scored_with indices.csv", row.names = FALSE)

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
    Mean.Total.Score = mean(TotalScore, na.rm = TRUE),
    n = n_distinct(Respondent.ID),
    .groups = "drop"
  ) %>%
  mutate(Country.n.label = paste0(Country.clean, "\n(n=", n, ")"))


# ========================================
# CORRELATION TABLE — total score vs indices
# ========================================
s2xIndice_cor_table <- map_df(indices, function(var) {
  d <- country_indices_s2 %>% filter(!is.na(.data[[var]]), !is.na(Mean.Total.Score))
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

print(s2xIndice_cor_table)


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
# Helper function - plots country means with r and p value
plot_index2 <- function(data, x_var, x_label) {
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
    labs(x = x_label, y = "Mean Total Score\nfor interventions testing") +
    theme(axis.text = element_text(size = 10),
          panel.grid.minor = element_blank())
}

plot_index2(country_indices_s2, "GDP_per_capita", "GDP per capita (USD)") +
  scale_x_log10(labels = dollar_format(prefix = "$")) +
  ggtitle("Simple linear model - GDP vs Bat Management Score (Survey 2)")

plot_index2(country_indices_s2, "AllResearchAndDev_percentGPD", "National R&D spending (all sectors) (% of GDP)") +
  scale_x_continuous(labels = percent_format(scale = 1), breaks = seq(0, 5, by = 0.5)) +
  ggtitle("Simple linear model - National R&D Investment vs Testing of Bat Interventions (Survey 2)")

plot_index2(country_indices_s2, "AgForestryFish_ValueAdded_percentGDP", "Agriculture, Forestry & Fisheries as % of GDP") +
  scale_x_continuous(labels = percent_format(scale = 1), breaks = seq(0, 80, by = 10)) +
  ggtitle("Simple linear model - Agricultural Economy Dependence vs Testing of Bat Interventions (Survey 2)")

plot_index2(country_indices_s2, "EnviroPerformance_score", "Environmental Performance Index (0-100)") +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  ggtitle("Simple linear model - Environmental Performance vs Testing of Bat Interventions (Survey 2)")

plot_index2(country_indices_s2, "AgResearchAndDev_PPP2017", "Public Ag R&D spending (million 2017 Purchasing Power Parity)") +
  scale_x_log10(labels = dollar_format(prefix = "$", suffix = "M")) +
  ggtitle("Simple linear model - Agricultural R&D Investment vs Testing of Bat Interventions (Survey 2)")


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
