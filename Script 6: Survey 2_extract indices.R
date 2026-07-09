# Code author: Rani Davis
# Last updated: 6 July 2026
#
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
# JOIN INDICES TO SURVEY 2 DATA
# ========================================
Survey_2_long_indices <- Survey_2_long %>%
  mutate(Country.WB = recode(Country.clean,
                             "USA"                     = "United States",
                             "UK"                      = "United Kingdom",
                             "Peru & Colombia"         = "Peru",
                             "Australia & New Zealand" = "Australia",
                             "Taiwan"                  = "Taiwan, China"
  )) %>%
  left_join(gdp_data,       by = c("Country.WB" = "country")) %>%
  left_join(wdi_indicators, by = c("Country.WB" = "country")) %>%
  left_join(epi_data,       by = c("Country.WB" = "country")) %>%
  mutate(Country.GRAPE = recode(Country.WB,
                                "Taiwan, China" = "Taiwan"
  )) %>%
  left_join(grape_clean, by = c("Country.GRAPE" = "country"))

# Check coverage
Survey_2_long_indices %>%
  summarise(
    n_gdp   = sum(!is.na(gdp_per_capita)),
    n_ag_va = sum(!is.na(ag_value_added_pct)),
    n_rd    = sum(!is.na(rd_spend_pct)),
    n_epi   = sum(!is.na(epi_score)),
    n_ag_rd = sum(!is.na(ag_rd_spend))
  )

# Check unmatched countries - Taiwan..
Survey_2_long_indices %>%
  filter(!is.na(Country.WB), is.na(gdp_per_capita)) %>%
  distinct(Country.WB)

# ========================================
# SUMMARISE TO COUNTRY LEVEL
# ========================================
country_indices_s2 <- Survey_2_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  group_by(Country.clean, Country.WB,
           gdp_per_capita, ag_value_added_pct,
           rd_spend_pct, epi_score, ag_rd_spend) %>%
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
# plot_index helper:

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

plot_index2(country_indices_s2, "gdp_per_capita", "GDP per capita (USD)") +
  scale_x_log10(labels = dollar_format(prefix = "$")) +
  ggtitle("GDP vs Bat Management Score (Survey 2)")

plot_index2(country_indices_s2, "epi_score", "Environmental Performance Index (0-100)") +
  scale_x_continuous(limits = c(15, 85), breaks = seq(10, 90, by = 10)) +
  ggtitle("Environmental Performance vs Testing of Bat Interventions (Survey 2)")

plot_index2(country_indices_s2, "ag_rd_spend", "Public Ag R&D spending (million 2017 PPP$)") +
  scale_x_log10(labels = dollar_format(prefix = "$", suffix = "M")) +
  ggtitle("Agricultural R&D vs Testing of Bat Interventions (Survey 2)")

# Correlation heatmap — intervention x index
intervention_cor_table_s2 %>%
  mutate(Index = recode(Index,
                        "gdp_per_capita"     = "GDP per capita (USD)",
                        "ag_value_added_pct" = "Agriculture as % of GDP",
                        "rd_spend_pct"       = "National R&D spending (% of GDP)",
                        "epi_score"          = "Environmental Performance Index",
                        "ag_rd_spend"        = "Public Ag R&D spending (PPP$)"
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





