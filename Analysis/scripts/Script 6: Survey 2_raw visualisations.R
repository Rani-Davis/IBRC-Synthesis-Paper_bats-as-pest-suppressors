# Code author: Rani Davis
# Last updated:20 July 2026
#
# ----------------------------------------
# 0. Load libraries
# ----------------------------------------
library(tidyverse); library(writexl); library(scales)

# ----------------------------------------
# Import data
# ----------------------------------------
Survey_2_data <- read_csv("Analysis/clean data/Survey 2_scored_near complete_16.6.26.csv") 
head(Survey_2_data)
names(Survey_2_data)
str(Survey_2_data)

# ----------------------------------------
# 1. Pivot to long format and keep just scores for plotting
# ----------------------------------------
Survey_2_long <- Survey_2_data %>%
  select(Respondent.clean, Respondent.ID, Respondent.entry.ID,
         Region.within.country.clean, Country.clean, World.region.clean,
         Crop.clean, Crop.type.clean,
         `3a. Artificial Roost Score`,
         `4a. Agrochemical Reduction Score`,
         `5a. Revegetation Score`,
         `6a. Bat Education Score`,
         `7a. Artificial Water Sources Score`,
         `8a. Acoustic Bat Lure Score`) %>%
  pivot_longer(
    cols = ends_with("Score"),
    names_to = "Score.type",
    values_to = "Score"
  ) %>%
  mutate(Score.type = str_remove(Score.type, "^\\d+a\\. ") %>%
           str_remove(" Score$"))
#View(Survey_2_long)

# ----------------------------------------
# 2. Create unique identifier for each respondent x entry
# ----------------------------------------
Survey_2_long <- Survey_2_long %>%
  mutate(Respondent.entry.label = paste(Respondent.ID, Respondent.entry.ID, sep = "-"))
colnames(Survey_2_long)

# ----------------------------------------
# 3. Calculate total and mean scores across all interventions
# ----------------------------------------
Survey_2_long <- Survey_2_long %>%
  group_by(Respondent.clean, Respondent.entry.ID) %>%
  mutate(TotalScore.allInterventions = sum(Score, na.rm = TRUE),
         MeanScore.allInterventions    = mean(Score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Respondent.clean = fct_reorder(Respondent.clean, TotalScore.allInterventions))

Survey_2_long <- Survey_2_long %>%
  mutate(Respondent.entry.label = fct_reorder(Respondent.entry.label, TotalScore.allInterventions, .desc = TRUE))

colnames(Survey_2_long)
head(Survey_2_long)

#View(Survey_2_long)

# ----------------------------------------
# 4. VISUALISATIONS - Stacked barcharts
# ----------------------------------------
Survey_2_long %>%
  filter(!is.na(Score)) %>%   # remove the incomplete survey responses (where score = NA)
  mutate(Score.label = factor(Score, levels = 0:3,
                              labels = c("Not tried", "Tried, untested", "Tried, testing ongoing", "Tried and tested"))) %>%
  ggplot(aes(x = Score.type, fill = Score.label)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#d73027", "#fee08b", "#1a9850", "darkgreen")) +
  scale_y_continuous(labels = percent) +
  labs(x = "Management intervention", y = "Proportion of systems surveyed", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Other way of visualising where to the left = not tried.
Survey_2_long %>%
  mutate(Score.label = factor(Score, levels = 0:3,
                              labels = c("Not tried", "Tried, untested", "Tried, testing ongoing", "Tried and tested")),
         diverge = Score - 1) %>%
  group_by(Score.type, Score.label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Score.type) %>%
  mutate(pct = n / sum(n) * ifelse(Score.label == "Not tried", -1, 1)) %>%
  ggplot(aes(x = fct_reorder(Score.type, pct), y = pct, fill = Score.label)) +
  geom_col() +
  scale_fill_manual(values = c("#d73027", "#fee08b", "#1a9850", "darkgreen")) +
  scale_y_continuous(labels = ~ paste0(abs(round(. * 100)), "%")) +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "white", linewidth = 1) +
  labs(x = NULL, y = "← Not tried  |  Tried →\nProportion of systems surveyed", fill = NULL) +
  theme_minimal()

# 'Evidence gap' bar chart - just the 'not tried' answers
Survey_2_long %>%
  mutate(tried = ifelse(Score == 0, "Never tried", "Tried (any level)")) %>%
  group_by(Score.type, tried) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Score.type) %>%
  mutate(pct = n / sum(n)) %>%
  filter(tried == "Never tried") %>%
  ggplot(aes(x = fct_reorder(Score.type, pct), y = pct)) +
  geom_col(fill = "#d73027") +
  geom_text(aes(label = paste0(round(pct * 100), "%")),
            hjust = -0.2, size = 3.5) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  coord_flip() +
  labs(x = NULL, y = "% of systems that have never tried this intervention",
       title = "Evidence gaps by intervention") +
  theme_minimal()


# Faceted by region ------
region_n <- Survey_2_long %>%
  filter(!is.na(World.region.clean)) %>%
  distinct(Respondent.ID, World.region.clean) %>%
  count(World.region.clean) %>%
  mutate(label = paste0(World.region.clean, "\n(n=", n, ")"))

region_order <- Survey_2_long %>%
  filter(!is.na(World.region.clean)) %>%
  group_by(World.region.clean) %>%
  summarise(mean_total = mean(Score, na.rm = TRUE)) %>%
  arrange(mean_total) %>%
  pull(World.region.clean)

Survey_2_long %>%
  filter(!is.na(World.region.clean)) %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(
    Score.label = factor(Score, levels = 0:3,
                         labels = c("Not tried", "Tried, untested", "Testing ongoing", "Evaluated")),
    World.region.clean = factor(World.region.clean, levels = region_order),
    label = factor(label, levels = paste0(region_order, "\n(n=", region_n$n[match(region_order, region_n$World.region.clean)], ")"))
  ) %>%
  ggplot(aes(x = Score.type, fill = Score.label)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#d73027", "#fee08b", "#1a9850", "darkgreen"),
                    name = "Evidence level") +
  scale_y_continuous(labels = percent) +
  facet_wrap(~ label) +
  coord_flip() +
  labs(x = NULL, y = "Proportion of systems") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 9, face = "bold"),
        legend.position = "bottom")


# Faceted by Crop type ------
crop_n <- Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  distinct(Respondent.ID, Crop.type.clean) %>%
  count(Crop.type.clean) %>%
  mutate(label = paste0(Crop.type.clean, "\n(n=", n, ")"))

crop_order <- Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  group_by(Crop.type.clean) %>%
  summarise(mean_total = mean(Score, na.rm = TRUE)) %>%
  arrange(mean_total) %>%
  pull(Crop.type.clean)

Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  left_join(crop_n, by = "Crop.type.clean") %>%
  mutate(
    Score.label = factor(Score, levels = 0:3,
                         labels = c("Not tried", "Tried, untested", "Testing ongoing", "Evaluated")),
    Crop.type.clean = factor(Crop.type.clean, levels = crop_order),
    label = factor(label, levels = paste0(crop_order, "\n(n=", crop_n$n[match(crop_order, crop_n$Crop.type.clean)], ")"))
  ) %>%
  ggplot(aes(x = Score.type, fill = Score.label)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#d73027", "#fee08b", "#1a9850", "darkgreen"),
                    name = "Evidence level") +
  scale_y_continuous(labels = percent) +
  facet_wrap(~ label, ncol = 4) +
  coord_flip() +
  labs(x = NULL, y = "Proportion of systems") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 9, face = "bold"),
        legend.position = "bottom")


# ----------------------------------------
# 5. VISUALISATIONS - Mean +/- SE of scores
# ----------------------------------------
mutate(Crop.type.label = paste0(Crop.type.clean, "\n(n = ", n_resp, ")"))

Survey_2_long %>%
  group_by(Score.type) %>%
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    se = sd / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = fct_reorder(Score.type, mean), y = mean)) +
  geom_jitter(data = Survey_2_long,
              aes(x = Score.type, y = Score),
              width = 0.22, height = 0.05, alpha = 0.3, size = 1.5, colour = "grey60") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, colour = "grey50") +
  geom_point(size = 4, colour = "#1a9850") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = c("Not tried", "Tried,\nuntested", "Tried,\ntesting ongoing", "Tried\nand tested")
  )+
  coord_flip() +
  labs(x = "Intervention", y = "Mean intervention testing score (± SE)",
       caption = "Dashed line = 'Tried, untested' threshold") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 9))

# By crop type  ------
crop_n <- Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  group_by(Crop.type.clean) %>%
  summarise(n_resp = n_distinct(Respondent.ID, Respondent.entry.ID), .groups = "drop") %>%
  mutate(Crop.type.label = paste0(Crop.type.clean, "\n(n = ", n_resp, ")"))

summary_by_crop <- Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  left_join(crop_n, by = "Crop.type.clean") %>%
  group_by(Crop.type.clean, Crop.type.label, Score.type) %>%   # add Crop.type.clean here
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    se = sd / sqrt(n()),
    .groups = "drop"
  )

jitter_by_crop <- Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  left_join(crop_n, by = "Crop.type.clean")

ggplot(summary_by_crop, aes(x = fct_reorder(Score.type, mean), y = mean)) +
  geom_jitter(data = jitter_by_crop,
              aes(x = Score.type, y = Score, colour = Crop.type.clean),
              width = 0.22, height = 0.05, alpha = 0.3, size = 1.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, colour = Crop.type.clean),
                width = 0.2) +
  geom_point(aes(colour = Crop.type.clean), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = crop_colours) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = c("Not tried",  "Tried,\nuntested", "Tried,\ntesting ongoing", "Tried\nand tested")
  ) +
  coord_flip() +
  facet_wrap(~ Crop.type.label, nrow = 2, ncol = 4) +
  labs(x = NULL, y = "Mean intervention testing score (± SE)",
       caption = "Dashed line = 'Tried, untested' threshold") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 0.9, size = 8),
        panel.spacing = unit(1.4, "lines")) +
  guides(colour = "none")


# by world region  ------
region_n <- Survey_2_long %>%
  filter(!is.na(World.region.clean)) %>%
  group_by(World.region.clean) %>%
  summarise(n_resp = n_distinct(Respondent.ID, Respondent.entry.ID), .groups = "drop") %>%
  mutate(World.region.label = paste0(World.region.clean, "\n(n = ", n_resp, ")"))

summary_by_region <- Survey_2_long %>%
  filter(!is.na(World.region.clean)) %>%
  left_join(region_n, by = "World.region.clean") %>%
  group_by(World.region.clean, World.region.label, Score.type) %>%   # add World.region.clean here
  summarise(
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    se = sd / sqrt(n()),
    .groups = "drop"
  )


jitter_by_region <- Survey_2_long %>%
  filter(!is.na(World.region.clean)) %>%
  left_join(region_n, by = "World.region.clean")

ggplot(summary_by_region, aes(x = fct_reorder(Score.type, mean), y = mean)) +
  geom_jitter(data = jitter_by_region,
              aes(x = Score.type, y = Score, colour = World.region.clean),
              width = 0.22, height = 0.05, alpha = 0.3, size = 1.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, colour = World.region.clean),
                width = 0.2) +
  geom_point(aes(colour = World.region.clean), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = region_colours) +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = c("Not tried",  "Tried,\nuntested", "Tried,\ntesting ongoing", "Tried\nand tested")
  ) +
  coord_flip() +
  facet_wrap(~ World.region.label, nrow = 2, ncol = 4) +
  labs(x = NULL, y = "Mean intervention testing score (± SE)",
       caption = "Dashed line = 'Tried but untested' threshold") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 0.9, size = 8),
        panel.spacing = unit(1.4, "lines")) +
  guides(colour = "none")

# ----------------------------------------
# 6. VISUALISATIONS - Rigeline plots for scores
# ----------------------------------------
# Ridgeline plot by region
library(ggridges)
# Calculates height at each point on a ridge from a kernel density estimate (KDE), 
# Where scores cluster together (e.g. lots of respondents at Score = 3), the bumps overlap and stack, producing a tall peak. Where scores are sparse, the curve stays low.

Survey_2_long %>%
  ggplot(aes(x = Score, y = Score.type, fill = after_stat(x))) +
  geom_density_ridges_gradient(jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0.05),
                               point_shape = 21, point_size = 1.5, point_alpha = 0.5,
                               scale = 0.8) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 1.5) +
  scale_x_continuous(breaks = 0:3, limits = c(0, 3)) +
  labs(x = "Intervention testing score (0–3)", y = NULL,
       caption = "Each point represents one crop/region system") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, hjust = 0))



# Ridgeline by world region  ------
    # Set your minimum n for a density ridge to be shown
    min_n_for_ridge <- 3
    
    ridge_data <- Survey_2_long %>%
      distinct(Respondent.entry.label, World.region.clean, TotalScore) %>%
      filter(!is.na(World.region.clean)) %>%
      left_join(region_n_ridge, by = "World.region.clean") %>%
      mutate(label = fct_reorder(label, TotalScore))
    
    # Split into two groups based on sample size
    ridge_data_full   <- ridge_data %>% filter(n >= min_n_for_ridge)
    ridge_data_sparse <- ridge_data %>% filter(n <  min_n_for_ridge)
    
    ggplot(ridge_data, aes(x = TotalScore, y = label)) +
      # Density ridges only for regions with enough data
      geom_density_ridges_gradient(data = ridge_data_full,
                                   aes(fill = after_stat(x)),
                                   jittered_points = TRUE,
                                   position = position_points_jitter(width = 0.05, height = 0.05),
                                   point_shape = 21, point_size = 1.5, point_alpha = 0.5,
                                   scale = 0.8) +
      # Points only for low-n regions
      geom_point(data = ridge_data_sparse,
                 position = position_jitter(width = 0.05, height = 0.1),
                 shape = 21, size = 1, alpha = 0.8,
                 aes(fill = TotalScore)) +
      scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                           midpoint = 6) +
      scale_x_continuous(breaks = seq(-1, 18, by = 2), limits = c(-1, 18)) +
      labs(x = "Total intervention testing score (sum of 6 interventions, 0–18)", 
           y = NULL,
           caption = "Each point represents one crop+region system.\nRegions with n < 5 shown as points only (density not estimated).") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.caption = element_text(size = 8, hjust = 0))


# By region and intervention  ------
    # Calculate n per region x Score.type (not just per region)
    region_scoretype_n <- Survey_2_long %>%
      filter(!is.na(World.region.clean), !is.na(Score)) %>%
      distinct(Respondent.entry.label, World.region.clean, Score.type) %>%
      count(World.region.clean, Score.type, name = "n_facet")
    
    region_scoretype_ridge_data <- Survey_2_long %>%
      filter(!is.na(World.region.clean)) %>%
      left_join(region_n_ridge, by = "World.region.clean") %>%
      left_join(region_scoretype_n, by = c("World.region.clean", "Score.type")) %>%
      mutate(label = fct_reorder(label, Score))
    
    region_scoretype_ridge_full   <- region_scoretype_ridge_data %>% filter(n_facet >= min_n_for_ridge)
    region_scoretype_ridge_sparse <- region_scoretype_ridge_data %>% filter(n_facet <  min_n_for_ridge)
    
    ggplot(region_scoretype_ridge_data, aes(x = Score, y = label)) +
      # Density ridges where n is sufficient
      geom_density_ridges_gradient(data = region_scoretype_ridge_full,
                                   aes(fill = after_stat(x)),
                                   jittered_points = TRUE,
                                   position = position_points_jitter(width = 0.05, height = 0.05),
                                   point_shape = 21, point_size = 1, point_alpha = 0.5,
                                   scale = 0.8) +
      # Points only where n is too low for density estimation
      geom_point(data = region_scoretype_ridge_sparse,
                 position = position_jitter(width = 0.05, height = 0.05),
                 shape = 21, size = 1, alpha = 0.5,
                 aes(fill = Score)) +
      scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                           midpoint = 1.5) +
      scale_x_continuous(breaks = 0:3, limits = c(0, 3)) +
      facet_wrap(~ Score.type, ncol = 3) +
      labs(x = "Intervention testing score (0–3)", y = NULL,
           caption = "Each point represents one crop/region system.\nRegions with n < 5 for a given intervention shown as points only (density not estimated).") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.caption = element_text(size = 8, hjust = 0),
            strip.text = element_text(face = "bold"))
    


# Ridgeline plot by crop type  ------
crop_n_ridge <- Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  distinct(Respondent.entry.label,Crop.type.clean) %>%
  count(Crop.type.clean) %>%
  mutate(label = paste0(Crop.type.clean, " (n=", n, ")"))

Survey_2_long %>%
  distinct(Respondent.entry.label, Crop.type.clean, TotalScore) %>%
  filter(!is.na(Crop.type.clean)) %>%
  left_join(crop_n_ridge, by = "Crop.type.clean") %>%
  ggplot(aes(x = TotalScore, y = fct_reorder(label, TotalScore),
             fill = after_stat(x))) +
  geom_density_ridges_gradient(jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0.05),
                               point_shape = 21, point_size = 1.5, point_alpha = 0.5,
                               scale = 0.8) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 6) +
  scale_x_continuous(breaks = seq(0, 18, by = 2), limits = c(0, 18)) +
  labs(x = "Total intervention testing score (sum of 6 interventions, 0–18)", 
       y = NULL,
       caption = "Each point represents one crop+region system") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, hjust = 0))


# By crop type and intervention  ------
Survey_2_long %>%
  filter(!is.na(Crop.type.clean)) %>%
  left_join(crop_n_ridge, by = "Crop.type.clean") %>%
  ggplot(aes(x = Score, y = fct_reorder(label, Score),
             fill = after_stat(x))) +
  geom_density_ridges_gradient(jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0.05),
                               point_shape = 21, point_size = 1.5, point_alpha = 0.5,
                               scale = 0.8) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 1.5) +
  scale_x_continuous(breaks = 0:3, limits = c(0, 3)) +
  facet_wrap(~ Score.type, ncol = 3) +
  labs(x = "Intervention testing score (0–3)", y = NULL,
       caption = "Each point represents one crop/region system") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(size = 8, hjust = 0),
        strip.text = element_text(face = "bold"))


# ----------------------------------------
# 7. VISUALISATIONS - 'Heatmaps'
# ----------------------------------------
# reorder entries by mean score across all
Survey_2_long <- Survey_2_long %>%
  mutate(Score.type = fct_reorder(Score.type, Score, .fun = mean, .desc = FALSE)) # ignore warning, we want to drop NAs

responder_column <- "Respondent.entry.label"

# Simple heatmap (no facet), labelled by crop   ------
ggplot(Survey_2_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_2_long$Crop.type.clean, Survey_2_long$Respondent.entry.label)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  coord_cartesian(clip = "off")


# Simple heatmap (no facet), labelled by country  ------
ggplot(Survey_2_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_2_long$Country.clean, Survey_2_long$Respondent.entry.label)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  coord_cartesian(clip = "off")


# Faceted by world region, labelled by crop type ------
ggplot(Survey_2_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_2_long$Crop.type.clean, Survey_2_long$Respondent.entry.label)) +
  facet_wrap(~ World.region.clean, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  )


# Faceted by crop type, labelled by region  ------
ggplot(Survey_2_long, aes(x = Score.type, y = Respondent.entry.label, fill = Score)) +
  geom_tile(color = "white", height = 0.9) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850", midpoint = 1.5) +
  scale_y_discrete(labels = setNames(Survey_2_long$World.region.clean, Survey_2_long$Respondent.entry.label)) +
  facet_wrap(~ Crop.type.clean, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Score") +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 15))
  )



# ----------------------------------------
# 8. VISUALISATIONS - Radar plot
# ----------------------------------------
# Radar plots are very busy for this data
