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
Survey_1_data <- read_csv("clean data/Survey 1_scored_near complete_16.6.26.csv") 
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

write.csv(Survey_1_long, "clean data/Survey 1_scored_near complete_long_16.6.26.csv", row.names = FALSE)


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

## -- Specify palettes --
crop_colours <- c(
  "Vegetable crops" = "#1B5E20", "Perennial fruit & forestry crops" = "#C62828",
  "Vineyard" = "#7B1FA2", "Pasture & mixed cropping" = "#C8E6C9",
  "Field crops" = "#FDD835", "Grain crops" = "#FF8C00", "Agroforestry cacao" = "#3E2000")
region_colours <- c(
  "Europe" = "#2166AC", "Africa" = "#D6604D", "Latin America / Caribbean" = "#33A02C",
  "Australasia / Pacific" = "#00B4D8", "North America" = "#7B2D8B",
  "Middle East / Western Asia" = "#FF8C00", "South / Southeast Asia" = "#E7298A",
  "East Asia" = "#E6C619")

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
evidence_labels <- c(`0` = "No data", `1` = "Bats present\nonly", 
                     `2` = "Diet/activity\noverlap", `3` = "Direct\nsuppression")
# ----------------------------------------
# Evidence — by Crop Type
# ----------------------------------------
d_evid_crop <- Survey_1_long %>%
  filter(Score.type == "Evidence", !is.na(Crop.type.clean))

summary_evid_crop <- d_evid_crop %>%
  group_by(Crop.type.clean) %>%
  summarise(mean = mean(Score, na.rm = TRUE),
            se = sd(Score, na.rm = TRUE) / sqrt(n_distinct(Respondent.entry.label)),
            n = n_distinct(Respondent.entry.label),
            .groups = "drop") %>%
  mutate(label = paste0(Crop.type.clean, "\n(n=", n, ")"))

d_evid_crop <- d_evid_crop %>% 
  left_join(select(summary_evid_crop, Crop.type.clean, label), by = "Crop.type.clean")

ggplot(summary_evid_crop, aes(x = fct_reorder(label, mean), y = mean)) +
  geom_jitter(data = d_evid_crop, aes(x = label, y = Score),
              width = 0.15, height = 0.05, alpha = 0.3, size = 1.5, colour = "grey60") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, colour = "grey50") +
  geom_point(size = 3, colour = "#1a9850") +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = function(x) evidence_labels[as.character(x)]) +
  coord_flip() +
  labs(x = NULL, y = "Evidence score (± SE)", title = "Evidence of bat-mediated pest suppression, by Crop Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

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
  mutate(label = paste0(World.region.clean, "\n(n=", n, ")"))

d_evid_region <- d_evid_region %>% left_join(select(summary_evid_region, World.region.clean, label), by = "World.region.clean")

ggplot(summary_evid_region, aes(x = fct_reorder(label, mean), y = mean)) +
  geom_jitter(data = d_evid_region, aes(x = label, y = Score),
              width = 0.15, height = 0.05, alpha = 0.3, size = 1.5, colour = "grey60") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, colour = "grey50") +
  geom_point(size = 3, colour = "#1a9850") +
  scale_y_continuous(breaks = 0:3, limits = c(0, 3),
                     labels = function(x) evidence_labels[as.character(x)])+
  coord_flip() +
  labs(x = NULL, y = "Evidence score (± SE)", title = "Evidence of bat-mediated pest suppression, by World Region") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))


