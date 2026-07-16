# Code author: Rani Davis
# Last updated: 6 July 2026
# Can view our 'knowledge pathway' here: https://www.canva.com/design/DAGs2rCMnYQ/0oo2xr8_waggqfzTKeBudg/edit
#
# ----------------------------------------
# 0. Load libraries
# ----------------------------------------
library(tidyverse); library(writexl); library(scales)


# ----------------------------------------
# 1. Import data
# ----------------------------------------
Survey_1_data <- read_csv("Analysis/clean data/Survey 1_scored_near complete_16.6.26.csv") 
head(Survey_1_data)
names(Survey_1_data)
str(Survey_1_data)
#View(Survey_1_data)


# ----------------------------------------
# 2. Pivot to long format and keep just scores for plotting
# ----------------------------------------
Survey_1_long <- Survey_1_data %>%
  select(Respondent.clean, Respondent.ID, Respondent.entry.ID,
         Region.within.country.clean, Country.clean, World.region.clean,
         Crop.clean, Crop.type.clean,
         `10a. Evidence Score`,
         `11a. Representativeness Score`,
         `12a. Limiting Factors Score`,
         `13a. Available Strategies Score`,
         `14a. Monitoring Strategies Score`,
         `15a. Implementing Strategies Score`) %>%
  pivot_longer(
    cols = ends_with("Score"),
    names_to = "Score.type",
    values_to = "Score"
  ) %>%
  mutate(Score.type = str_remove(Score.type, "^\\d+a\\. ") %>%
           str_remove(" Score$"))
#View(Survey_1_long)


# ----------------------------------------
# 3. Adjust name of 'Implementing Strategies'
# ----------------------------------------
# Rename column World.region.clean to World.region.specific.clean
# Create new column with correct groupings and call it World.region.clean

Survey_1_long <- Survey_1_long %>%
  mutate(
    Score.type = case_when(
      Score.type %in% c("Implementing Strategies") ~ "Breadth of Implementation\nof Interventions",
      Score.type %in% c("Monitoring Strategies") ~ "Monitoring Interventions",
      Score.type %in% c("Available Strategies") ~ "Available Interventions",
      TRUE ~ Score.type))



# ----------------------------------------
# 4. Create unique identifier for each respondent x entry
# ----------------------------------------
Survey_1_long <- Survey_1_long %>%
  mutate(Respondent.entry.label = paste(Respondent.ID, Respondent.entry.ID, sep = "-"))


# ----------------------------------------
# 5. Calculate total score
# ----------------------------------------
Survey_1_long <- Survey_1_long %>%
  group_by(Respondent.clean, Respondent.entry.ID) %>%
  mutate(TotalScore = sum(Score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Respondent.clean = fct_reorder(Respondent.clean, TotalScore))

Survey_1_long <- Survey_1_long %>%
  mutate(Respondent.entry.label = fct_reorder(Respondent.entry.label, TotalScore, .desc = TRUE))


# ----------------------------------------
# 6. Order questions in desired order from basic knowledge to implementation
# ----------------------------------------
names(Survey_1_long)
head(Survey_1_long)
desired_order <- c(
  "Evidence",
  "Representativeness",
  "Limiting Factors",
  "Available Interventions",
  "Monitoring Interventions",
  "Breadth of Implementation\nof Interventions"
)

Survey_1_long <- Survey_1_long %>%
  mutate(Score.type = factor(Score.type, levels = desired_order))

# Check data
#View(Survey_1_long)

#write.csv(Survey_1_long, "Analysis/clean data/Survey 1_scored_near complete_long_16.6.26.csv", row.names = FALSE)


# ----------------------------------------
# 7. VISUALISATIONS - 
# 'Heatmaps'
# ----------------------------------------
responder_column <- "Respondent.entry.label"

# Simple heatmap (no facet), labelled by crop 
ggplot(Survey_1_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_1_long$Crop.clean, Survey_1_long$Respondent.entry.label)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  coord_cartesian(clip = "off")

# Simple heatmap (no facet), labelled by country
ggplot(Survey_1_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_1_long$Country.clean, Survey_1_long$Respondent.entry.label)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  coord_cartesian(clip = "off")

# Faceted by world region
ggplot(Survey_1_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_1_long$Crop.clean, Survey_1_long$Respondent.entry.label)) +
  facet_wrap(~ World.region.clean, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  )


# ----------------------------------------
# 8. VISUALISATIONS - 
# Line plots with raw points and mean lines
# ----------------------------------------
## -- Remove 'representativeness' column  --
# In most systems, representativeness scores are low, but we don't necessarily want to plot this on our knowledge pathway, something to consider during write-up
keep_5 <- c("Evidence", "Limiting Factors", "Available Interventions",
            "Monitoring Interventions", "Breadth of Implementation\nof Interventions")

## -- Colour palette specified in 'Colour palette script' --


## -- Specify caption --
score_caption <- paste(
  "Scores (0–3) reflect knowledge/action maturity for each domain:",
  "Evidence = evidence of bats' role in pest suppression;",
  "Limiting Factors = understanding of threats/limiting factors to bats in this system;",
  "Available Interventions = knowledge of interventions to boost bat-mediated pest control;",
  "Monitoring Interventions = extent interventions have been monitored for effects on bats;",
  "Breadth of Implementation = how widely interventions have spread beyond study sites to other farmers.",
  sep = "\n")

# -- Specify x axis properly --
score_relabel_axis <- c(
  "Evidence" = "Evidence for\nbat-mediated\npest suppression",
  "Limiting Factors" = "Understanding of\nthreats to bats",
  "Available Interventions" = "Knowledge of\nManagement\ninterventions\navailable",
  "Monitoring Interventions" = "Monitoring of\nintervention\noutcomes",
  "Breadth of Implementation\nof Interventions" = "System-wide\nextent of\nintervention\nimplementation")
generic_labels <- c(`0` = " 0 =\nAbsent", `1` = " 1 =\nAnecdotal / Limited", `2` = "2 =\nPartial / Developing", `3` = "3 =\nEstablished / Widespread")

# -- 8a. By Crop Type --
summary_crop <- Survey_1_long %>%
  filter(!is.na(Crop.type.clean), Score.type %in% keep_5) %>%
  group_by(Score.type, Crop.type.clean) %>%
  summarise(Mean.Score = mean(Score, na.rm = TRUE), n = n_distinct(Respondent.entry.label), .groups = "drop") %>%
  mutate(Crop.label = paste0(Crop.type.clean, " (n=", n, ")"))

long_crop <- Survey_1_long %>%
  filter(!is.na(Crop.type.clean), Score.type %in% keep_5) %>%
  left_join(distinct(summary_crop, Crop.type.clean, Crop.label), by = "Crop.type.clean")

crop_colours_n <- setNames(crop_colours[match(unique(summary_crop$Crop.type.clean), names(crop_colours))],
                           unique(summary_crop$Crop.label))

ggplot() +
  geom_jitter(data = long_crop, aes(x = factor(Score.type, levels = keep_5), y = Score, colour = Crop.label),
              width = 0.1, height = 0.05, alpha = 0.3, size = 1.8) +
  geom_line(data = summary_crop,
            aes(x = factor(Score.type, levels = keep_5), y = Mean.Score, colour = Crop.label, group = Crop.label),
            linewidth = 1.2) +
  geom_point(data = summary_crop,
             aes(x = factor(Score.type, levels = keep_5), y = Mean.Score, colour = Crop.label), size = 3.5) +
  scale_colour_manual(values = crop_colours_n, na.value = "grey70") +
  scale_x_discrete(limits = keep_5, labels = score_relabel_axis) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = function(x) generic_labels[as.character(x)]) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = "Mean Score", colour = "Crop Type",
       title = "",
       #tag = "Knowledge of:"
  ) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.tag = element_text(face = "bold.italic", size = 12, colour = "grey10"),
        plot.tag.position = c(0.14, 0.025),  # x, y in NPC units (0-1, relative to whole plot)
        axis.text.x = element_text(size = 9, lineheight = 0.9),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        plot.margin = margin(10, 10, 25, 10),
        panel.grid.minor = element_blank())



# -- By World Region --
summary_region <- Survey_1_long %>%
  filter(!is.na(World.region.clean), Score.type %in% keep_5) %>%
  group_by(Score.type, World.region.clean) %>%
  summarise(Mean.Score = mean(Score, na.rm = TRUE), n = n_distinct(Respondent.entry.label), .groups = "drop") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n, ")"))

long_region <- Survey_1_long %>%
  filter(!is.na(World.region.clean), Score.type %in% keep_5) %>%
  left_join(distinct(summary_region, World.region.clean, Region.label), by = "World.region.clean")

region_colours_n <- setNames(region_colours[match(unique(summary_region$World.region.clean), names(region_colours))],
                             unique(summary_region$Region.label))

ggplot() +
  geom_jitter(data = long_region, aes(x = factor(Score.type, levels = keep_5), y = Score, colour = Region.label),
              width = 0.1, height = 0.05, alpha = 0.3, size = 1.8) +
  geom_line(data = summary_region,
            aes(x = factor(Score.type, levels = keep_5), y = Mean.Score, colour = Region.label, group = Region.label),
            linewidth = 1.2) +
  geom_point(data = summary_region,
             aes(x = factor(Score.type, levels = keep_5), y = Mean.Score, colour = Region.label), size = 3.5) +
  scale_colour_manual(values = region_colours_n, na.value = "grey70") +
  scale_x_discrete(limits = keep_5, labels = score_relabel_axis) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = function(x) generic_labels[as.character(x)]) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = "Mean Score", colour = "World Region",
       title = "",
       #tag = "Knowledge of:"
  ) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.tag = element_text(face = "bold.italic", size = 12, colour = "grey10"),
        plot.tag.position = c(0.14, 0.025),  # x, y in NPC units (0-1, relative to whole plot)
        axis.text.x = element_text(size = 9, lineheight = 0.9),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        plot.margin = margin(10, 10, 25, 10),
        panel.grid.minor = element_blank())



# ----------------------------------------
# 9. VISUALISATIONS - 
# Mean score +/- SE for evidence of bats role in pest suppression
# ----------------------------------------
##### NOW JUST EVIDENCE & INTERVENTIONS #####
evidence_labels <- c(`0` = "No data on bat\nactivity or interaction\nwith pests", `1` = "Bats are only\nknown to occur\nin the crop", 
                     `2` = "Bats consume\npests or overlap\nin activity with\npests", `3` = "Direct evidence of\npest suppression\nby bats")
# ----------------------------------------
# Evidence — by Crop Type
# ----------------------------------------
library(stringr)

d_evid_crop <- Survey_1_long %>%
  filter(Score.type == "Evidence", !is.na(Crop.type.clean))

summary_evid_crop <- d_evid_crop %>%
  group_by(Crop.type.clean) %>%
  summarise(mean = mean(Score, na.rm = TRUE),
            se = sd(Score, na.rm = TRUE) / sqrt(n_distinct(Respondent.entry.label)),
            n = n_distinct(Respondent.entry.label),
            .groups = "drop") %>%
  mutate(label = paste0(str_wrap(Crop.type.clean, width = 15), "\n(n=", n, ")"))

d_evid_crop <- d_evid_crop %>% 
  left_join(select(summary_evid_crop, Crop.type.clean, label), by = "Crop.type.clean")


# plot coloured by crop
ggplot(summary_evid_crop, aes(x = fct_reorder(label, mean), y = mean)) +
  geom_jitter(data = d_evid_crop, aes(x = label, y = Score, colour = Crop.type.clean),
              width = 0.15, height = 0.02, alpha = 0.6, size = 1.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, colour = Crop.type.clean),
                width = 0.2) +
  geom_point(aes(colour = Crop.type.clean), size = 4, shape = 17) +
  scale_colour_manual(values = crop_colours, name = "Crop type",
                      labels = function(x) {
                        n_lookup <- setNames(summary_evid_crop$n, summary_evid_crop$Crop.type.clean)
                        paste0(x, " (n=", n_lookup[x], ")")
                      }) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = function(x) evidence_labels[as.character(x)]) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(x = NULL, y = "Evidence score (± SE)", title = "Evidence of bat-mediated pest suppression, by Crop Type") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# ----------------------------------------
# Evidence — by World Region
# ----------------------------------------
d_evid_region <- Survey_1_long %>%
  filter(Score.type == "Evidence", !is.na(World.region.clean))

summary_evid_region <- d_evid_region %>%
  group_by(World.region.clean) %>%
  summarise(mean = mean(Score, na.rm = TRUE),
            se = sd(Score, na.rm = TRUE) / sqrt(n_distinct(Respondent.entry.label)),
            n = n_distinct(Respondent.entry.label),
            .groups = "drop") %>%
  mutate(label = paste0(str_wrap(World.region.clean, width = 12), "\n(n=", n, ")"))

d_evid_region <- d_evid_region %>% 
  left_join(select(summary_evid_region, World.region.clean, label), by = "World.region.clean")

# plot coloured by region with legend
ggplot(summary_evid_region, aes(x = fct_reorder(label, mean), y = mean)) +
  geom_jitter(data = d_evid_region, aes(x = label, y = Score, colour = World.region.clean),
              width = 0.15, height = 0.02, alpha = 0.6, size = 1.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, colour = World.region.clean),
                width = 0.2) +
  geom_point(aes(colour = World.region.clean), size = 4, shape = 17, alpha = 1) +
  scale_colour_manual(values = region_colours, name = "World region",
                      labels = function(x) {
                        n_lookup <- setNames(summary_evid_region$n, summary_evid_region$World.region.clean)
                        paste0(x, " (n=", n_lookup[x], ")")
                      }) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = function(x) evidence_labels[as.character(x)]) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(x = NULL, y = "Evidence score (± SE)", title = "Evidence of bat-mediated pest suppression, by World Region") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



# ----------------------------------------
# 10. VISUALISATIONS - composite plot 
# Mean score +/- SE for each step along the knowledge pathway
# ----------------------------------------
#install.packages("ggh4x")
library(ggh4x)
library(patchwork)
library(stringr)
library(cowplot)
library(dplyr)
library(ggplot2)
library(forcats)

# ============================================================
# SHARED SETUP for plots by crop type and region (labels, wrapping, panel + arrow builders)
# ============================================================
evidence_labels <- c(`0` = "No data on bat\nactivity/interaction\nwith pests",
                     `1` = "Bats only known\nto occur in\nthe crop",
                     `2` = "Bats consume pests\nor overlap in\nactivity with pests",
                     `3` = "Direct evidence of\npest suppression\nby bats")

limiting_labels <- c(`0` = "No research or\ninfo on threats\nexists",
                     `1` = "Anecdotal evidence\nof possible\nthreats",
                     `2` = "Some empirical\nstudies identify\nlimiting factors",
                     `3` = "Strong empirical\nevidence of key\nlimiting factors")

intervention_labels <- c(`0` = "No strategies\nhave been\nproposed",
                         `1` = "Untested strategies\nexist, not\nadapted/tested",
                         `2` = "Strategies tried\nwith incomplete\nvalidation",
                         `3` = "Proven, effective,\ntailored strategies\nexist")

monitoring_labels <- c(`0` = "No strategies\nimplemented, so\nnone monitored",
                       `1` = "Irregular/anecdotal\nmonitoring, not\nused to adapt",
                       `2` = "Monitoring in place\nbut limited in\nscope",
                       `3` = "Systematic monitoring\nused to guide\nadaptive management")

implementation_labels <- c(`0` = "No implementation\noccurs anywhere\nin the system",
                           `1` = "Isolated/pilot\nimplementation,\nresearch only",
                           `2` = "Moderate adoption,\nuneven or not\nwidespread",
                           `3` = "Widespread, sustained\nimplementation across\nmost farms")

score_relabel_full <- c(
  "Evidence" = "Evidence for bat-mediated pest suppression",
  "Limiting Factors" = "Understanding of threats to bats",
  "Available Interventions" = "Knowledge of management interventions available",
  "Monitoring Interventions" = "Monitoring of intervention outcomes",
  "Breadth of Implementation\nof Interventions" = "System-wide extent of intervention implementation"
)
score_relabel_wrapped <- setNames(str_wrap(score_relabel_full, width = 29), names(score_relabel_full))


# ----- Panel function: takes level_order + legend_title, n= below name -----
make_panel <- function(score_type, y_labels, colour_values, group_var, data_raw, data_summary, level_order, legend_title) {
  d_sub <- data_raw %>% filter(Score.type.f == score_type)
  s_sub <- data_summary %>% filter(Score.type.f == score_type)
  
  d_sub$label <- factor(d_sub$label, levels = level_order)
  s_sub$label <- factor(s_sub$label, levels = level_order)
  
  # n lookup for this panel's data (used if this panel's legend gets extracted)
  n_lookup <- setNames(s_sub$n, s_sub[[group_var]])
  
  ggplot(s_sub, aes(x = label, y = mean, colour = .data[[group_var]])) +
    geom_jitter(data = d_sub, aes(x = label, y = Score, colour = .data[[group_var]]),
                width = 0.08, height = 0.02, alpha = 0.35, size = 1) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15, linewidth = 0.5) +
    geom_point(size = 2.2, shape = 17) +
    scale_colour_manual(values = colour_values, name = legend_title,
                        labels = function(x) paste0(x, "\n(n=", n_lookup[x], ")")) +
    scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                       labels = function(x) y_labels[as.character(x)]) +
    labs(x = NULL, y = NULL, title = score_relabel_wrapped[[score_type]]) +
    theme_minimal(base_size = 9) +
    theme(axis.text.y = element_text(size = 7, lineheight = 0.75),
          plot.title = element_text(size = 8, face = "bold", lineheight = 0.9,
                                    hjust = 0.5, margin = margin(b = 6)),
          panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 0.7),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          plot.margin = margin(4, 8, 4, 4))
}

make_arrow <- function() {
  ggplot() +
    annotate("segment", x = 0, xend = 1, y = 0.5, yend = 0.5,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             linewidth = 0.8, colour = "grey30") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
}


# ----- Composite builder and creates legend_title -----
build_composite <- function(group_var, colour_values, data_raw, data_summary, plot_title, legend_title) {
  
  overall_order <- data_summary %>%
    group_by(across(all_of(group_var)), label) %>%
    summarise(overall_mean = mean(mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(overall_mean) %>%
    pull(label) %>%
    as.character()
  
  panels <- purrr::map(keep_5, ~ make_panel(.x,
                                            switch(.x,
                                                   "Evidence" = evidence_labels,
                                                   "Limiting Factors" = limiting_labels,
                                                   "Available Interventions" = intervention_labels,
                                                   "Monitoring Interventions" = monitoring_labels,
                                                   "Breadth of Implementation\nof Interventions" = implementation_labels),
                                            colour_values, group_var, data_raw, data_summary,
                                            level_order = overall_order, legend_title = legend_title))
  
  arrow <- make_arrow()
  
  legend_source <- panels[[1]] +
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold")) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3), nrow = 1,
                                 title.position = "left", title.vjust = 0.5))
  shared_legend <- cowplot::get_legend(legend_source)
  
  main_row <- panels[[1]] + arrow + panels[[2]] + arrow + panels[[3]] +
    arrow + panels[[4]] + arrow + panels[[5]] +
    plot_layout(widths = c(6, 1, 6, 1, 6, 1, 6, 1, 6))
  
  main_row / shared_legend +
    plot_layout(heights = c(10, 1)) +
    plot_annotation(title = plot_title)
}


# ============================================================
# CROP TYPE VERSION
# ============================================================
d_all_crop <- Survey_1_long %>%
  filter(Score.type %in% keep_5, !is.na(Crop.type.clean)) %>%
  mutate(Score.type.f = factor(Score.type, levels = keep_5))

summary_all_crop <- d_all_crop %>%
  group_by(Score.type.f, Crop.type.clean) %>%
  summarise(mean = mean(Score, na.rm = TRUE),
            se = sd(Score, na.rm = TRUE) / sqrt(n_distinct(Respondent.entry.label)),
            n = n_distinct(Respondent.entry.label),
            .groups = "drop") %>%
  mutate(label = paste0(str_wrap(Crop.type.clean, width = 15), "\n(n=", n, ")"))

d_all_crop <- d_all_crop %>%
  left_join(distinct(summary_all_crop, Score.type.f, Crop.type.clean, label),
            by = c("Score.type.f", "Crop.type.clean"))

final_plot_crop <- build_composite("Crop.type.clean", crop_colours,
                                   d_all_crop, summary_all_crop,
                                   "Knowledge pathway progress, by Crop Type",
                                   legend_title = "Crop Type")
final_plot_crop

ggsave("figure exports/Draft_composite knowledge pathway plot_By Crop Type.png", final_plot_crop, width = 12.1, height = 2.6, dpi = 300)


# ============================================================
# WORLD REGION VERSION
# ============================================================
d_all_region <- Survey_1_long %>%
  filter(Score.type %in% keep_5, !is.na(World.region.clean)) %>%
  mutate(Score.type.f = factor(Score.type, levels = keep_5))

summary_all_region <- d_all_region %>%
  group_by(Score.type.f, World.region.clean) %>%
  summarise(mean = mean(Score, na.rm = TRUE),
            se = sd(Score, na.rm = TRUE) / sqrt(n_distinct(Respondent.entry.label)),
            n = n_distinct(Respondent.entry.label),
            .groups = "drop") %>%
  mutate(label = paste0(str_wrap(World.region.clean, width = 15), "\n(n=", n, ")"))

d_all_region <- d_all_region %>%
  left_join(distinct(summary_all_region, Score.type.f, World.region.clean, label),
            by = c("Score.type.f", "World.region.clean"))

final_plot_region <- build_composite("World.region.clean", region_colours,
                                     d_all_region, summary_all_region,
                                     "Knowledge pathway progress, by World Region",
                                     legend_title = "World Region")
final_plot_region

ggsave("figure exports/Draft_composite knowledge pathway plot_By World Region.png", final_plot_region, width = 12.1, height = 2.6, dpi = 300)
