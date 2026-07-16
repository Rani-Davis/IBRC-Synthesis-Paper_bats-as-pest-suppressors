# Code author: Rani Davis
# Last updated: 6 July 2026, Started 24th June 2025


# ----------------------------------------
# 0. Load packages
# ----------------------------------------
library(tidyverse); library(ggplot2);library(stringr);library(dplyr);
library(viridis); library(fmsb); library(scales); library(janitor);
library(writexl)


# =========================
# 1. Load data
# =========================
survey_2 <- read_csv("Analysis/raw data/Survey 2 clean_near complete_16.6.26.csv") %>%
  mutate(RowID = row_number())

# Optional check
glimpse(survey_2)
head(survey_2)
names(survey_2)
#View(survey_2)


# ----------------------------------------
# 0. Adjust south / southeast asia grouping
# ----------------------------------------
# Rename column World.region.clean to World.region.specific.clean
# Create new column with correct groupings and call it World.region.clean
unique(survey_2$Crop.type.clean)
survey_2 %>% count(World.region.clean)

survey_2 <- survey_2 %>%
  rename(World.region.specific.clean = World.region.clean) %>%
  mutate(
    World.region.clean = case_when(
      World.region.specific.clean %in% c("South Asia", "Southeast Asia") ~ "South / Southeast Asia",
      TRUE ~ World.region.specific.clean))%>%
  relocate(World.region.clean, .before = World.region.specific.clean)

# Check the result
unique(survey_2$World.region.clean)
unique(survey_2$World.region.specific.clean) # here is where the original grouping is kept

# ----------------------------------------
# 0. Adjust pine plantations and pasture grouping
# ----------------------------------------
# Rename column World.region.clean to World.region.specific.clean
# Create new column with correct groupings and call it World.region.clean
unique(survey_2$Crop.type.clean)
survey_2 %>% count(Crop.type.clean)

survey_2 <- survey_2 %>%
  rename(Crop.type.specific.clean = Crop.type.clean) %>%
  mutate(
    Crop.type.clean = case_when(
      Crop.type.specific.clean %in% c("Pasture", "Mixed farming system") ~ "Pasture & mixed cropping",
      Crop.type.specific.clean %in% c("Forestry", "Perennial fruit crops") ~ "Perennial fruit & forestry crops",
      TRUE ~ Crop.type.specific.clean))%>%
  relocate(Crop.type.clean, .before = Crop.type.specific.clean)

# Check the result
unique(survey_2$Crop.type.clean)
unique(survey_2$Crop.type.specific.clean) # here is where the original grouping is kept
survey_2 %>% count(Crop.type.clean,Crop.clean)%>%print(n = 70)




# =========================
# Scoring lookup table
# =========================
scores_list_2 <- list(
  Artificial_roost_score = c(
    "Not tried - No artificial roost habitats are known of" = 0,
    "Tried, untested - Some artificial roosts provided, but knowledge of its benefits to bats in this system is anecdotal or theoretical" = 1,
    "Tried, testing ongoing - Some artificial roosts provided, and impacts on bat activity or populations currently being monitored" = 2,
    "Tried and evaluated - There is confidence that artificial roosts have been provided and the effect on bat activity or populations was measured" = 3
  ),
  
  Agrochemical_reduction_score = c(
    "Not tried - No reductions in agrochemical use are known of ." = 0,
    "Tried, untested - Use of agrochemicals has been reduced, but knowledge of its benefits to bats in this system is anecdotal or theoretical" = 1,
    "Tried, testing ongoing - Use of agrochemicals has been reduced, and impacts on bat activity or populations currently being monitored" = 2,
    "Tried and evaluated - There is confidence that agrochemical use has been reduced and the effect on bat activity or populations was measured" = 3
  ),
  
  Revegetation_score = c(
    "Not tried - No revegetation programs for increasing landscape heterogeneity are known of." = 0,
    "Tried, not tested - There are some revegetation programs, but knowledge of its benefits to bats in this system is anecdotal or theoretical" = 1,
    "Tried, testing ongoing - There are some revegetation programs, and impacts on bat activity or populations currently being monitored" = 2,
    "Tried and evaluated - There is confidence that revegetation programs have been used and the effect on bat activity or populations was measured" = 3
  ),
  
  Bat_education_score = c(
    "Not tried - no community outreach programs delivered" = 0,
    "Tried, not tested - Community outreach programs have been conducted, but knowledge of its benefits to bats (e.g. behavioural change and positive bat perceptions) in this system is anecdotal or theoretical" = 1,
    "Tried, testing ongoing - regular, targeted outreach is ongoing where behavioural change (e.g. perception of bats) is being measured pre and post outreach/education activity" = 2,
    "Tried and tested - Regular, targeted outreach where behavioural change (e.g. perception of bats) was measured to evaluate the effectiveness of this management action" = 3
  ),
  
  Artificial_water_score = c(
    "Not tried - No created/artificial habitats are known of" = 0,
    "Tried, not tested - water sources have been created, but knowledge of its benefits to bats in this system is anecdotal or theoretical" = 1,
    "Tried, testing ongoing - water sources have been created, and impacts on bat activity or populations currently being monitored" = 2,
    "Tried and tested - There is confidence that created water sources have been used, and the effect on bat activity or populations was measured" = 3
  ),
  
  Acoustic_lure_score = c(
    "Not tried - No use of acoustic bat lures known of" = 0,
    "Tried, not tested - acoustic bat lures have been used, but knowledge of its benefits to bats in this system is anecdotal or theoretical" = 1,
    "Tried, testing ongoing - acoustic bat lures have been used, and impacts on bat activity or populations currently being monitored" = 2,
    "Tried and tested - acoustic bat lures have been used, and the effect on bat activity or populations was measured" = 3
  )
)

# =========================
# Apply scoring 
# =========================
survey_2.1 <- survey_2 %>%
  rename_with(~ str_remove(., "^x"), everything())
head(survey_2.1)

q3_name <- names(survey_2.1)[str_detect(names(survey_2.1), "^3a\\)")]
q4_name <- names(survey_2.1)[str_detect(names(survey_2.1), "^4a\\)")]
q5_name <- names(survey_2.1)[str_detect(names(survey_2.1), "^5a\\)")]
q6_name <- names(survey_2.1)[str_detect(names(survey_2.1), "^6a\\)")]
q7_name <- names(survey_2.1)[str_detect(names(survey_2.1), "^7a\\)")]
q8_name <- names(survey_2.1)[str_detect(names(survey_2.1), "^8a\\)")]

survey_2.2 <- survey_2.1 %>%
  mutate(
    q3a = .data[[q3_name]],
    q4a = .data[[q4_name]],
    q5a = .data[[q5_name]],
    q6a = .data[[q6_name]],
    q7a = .data[[q7_name]],
    q8a = .data[[q8_name]])

survey_2.2 <- survey_2.2 %>%
  mutate(
    `3a. Artificial Roost Score` =
      unname(scores_list_2$Artificial_roost_score[clean_text(q3a)]),
    `4a. Agrochemical Reduction Score` =
      unname(scores_list_2$Agrochemical_reduction_score[clean_text(q4a)]),
    `5a. Revegetation Score` =
      unname(scores_list_2$Revegetation_score[clean_text(q5a)]),
    `6a. Bat Education Score` =
      unname(scores_list_2$Bat_education_score[clean_text(q6a)]),
    `7a. Artificial Water Sources Score` =
      unname(scores_list_2$Artificial_water_score[clean_text(q7a)]),
    `8a. Acoustic Bat Lure Score` =
      unname(scores_list_2$Acoustic_lure_score[clean_text(q8a)])
  )

colnames(survey_2.2)

survey_2.2 <- survey_2.2 %>%
  select(-q3a, -q4a, -q5a, -q6a, -q7a, -q8a, -RowID)

# =========================
# Quick check
# =========================
#View(survey_2.2)
print(survey_2.2)
colnames(survey_2.2)


# =========================
# Export clean scored datasheets
# =========================
# specify columns to export
survey_2_export <- survey_2.2[, 1:31]
# write files
#write_xlsx(survey_2_export, "Analysis/clean data/Survey 2_scored_near complete_16.6.26.xlsx")
#write.csv(survey_2_export, "Analysis/clean data/Survey 2_scored_near complete_16.6.26.csv", row.names = FALSE)
