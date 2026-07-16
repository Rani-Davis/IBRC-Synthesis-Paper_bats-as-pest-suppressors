# Code author: Rani Davis
# Last updated: 6 July 2026, Started 4th Sep 2025

# ----------------------------------------
# 0. Load packages
# ----------------------------------------
library(tidyverse); library(ggplot2);library(stringr);library(dplyr);
library(viridis); library(fmsb); library(scales); library(janitor);
library(writexl)


# =========================
# 1. Load data
# =========================
survey_1 <- read_csv("Analysis/raw data/Survey 1 clean_near complete_16.6.26.csv") %>%
  mutate(RowID = row_number())

# Optional check
glimpse(survey_1)
head(survey_1)
names(survey_1)
#View(survey_1)


# ----------------------------------------
# 2. Wrangling - Adjust south / southeast asia grouping
# ----------------------------------------
# Rename column World.region.clean to World.region.specific.clean
# Create new column with correct groupings and call it World.region.clean
unique(survey_1$Crop.type.clean)
survey_1 %>% count(World.region.clean)

survey_1 <- survey_1 %>%
  rename(World.region.specific.clean = World.region.clean) %>%
  mutate(
    World.region.clean = case_when(
      World.region.specific.clean %in% c("South Asia", "Southeast Asia") ~ "South / Southeast Asia",
      TRUE ~ World.region.specific.clean)) %>%
  relocate(World.region.clean, .before = World.region.specific.clean)

# Check the result
unique(survey_1$World.region.clean)
unique(survey_1$World.region.specific.clean) # here is where the original grouping is kept

# ----------------------------------------
# 3. Wrangling - Adjust pine plantations and pasture grouping
# ----------------------------------------
# Rename column World.region.clean to World.region.specific.clean
# Create new column with correct groupings and call it World.region.clean
unique(survey_1$Crop.type.clean)
survey_1 %>% count(Crop.type.clean)
survey_1 %>% count(Crop.type.clean,`5) What is/ are your study crop(s) (e.g. vegetables)? If you work in multiple crops grown in different ways (e.g. rotational vegetable crops AND macadamia) please repeat this survey for each crop.`)%>%print(n = 70)

survey_1 <- survey_1 %>%
  rename(Crop.type.specific.clean = Crop.type.clean) %>%
  mutate(
    Crop.type.clean = case_when(
      Crop.type.specific.clean %in% c("Pasture", "Mixed farming system") ~ "Pasture & mixed cropping",
      Crop.type.specific.clean %in% c("Forestry", "Perennial fruit crops") ~ "Perennial fruit & forestry crops",
      TRUE ~ Crop.type.specific.clean)) %>%
  relocate(Crop.type.clean, .before = Crop.type.specific.clean)

# Check the result
unique(survey_1$Crop.type.clean)
unique(survey_1$Crop.type.specific.clean) # here is where the original grouping is kept
survey_1 %>% count(Crop.type.clean,Crop.clean)%>%print(n = 70)


# =========================
# 4. SCORE RESPONSES 

# 4a. Scoring lookup table
# =========================
scores_list <- list(
  Bats_role_score = c(
    "No data on bat presence, activity or interaction with pests in the crop." = 0,
    "Bats known to occur in the crop (e.g. via acoustics or capture), but no direct link to pests." = 1,
    "Bats known to consume pests or overlap in activity with pests (indirect evidence of bat benefits)." = 2,
    "Direct evidence of pest suppression by bats (e.g. exclusion studies, reductions in pest damage where bats are present)." = 3
  ),
  
  Evidence_rep_score = c(
    "From a single site / season - data collected from a single location/ farm or from a short term (<1 year) study. No spatial or temporal replication." = 0,
    "From a few sites or seasons - data collected from more than one farm or site, or during limited crop seasons (1 - 3 years). The representativeness of the findings to the whole system is limited." = 1,
    "Moderately representative - data from multiple sites and seasons (>3 years) but is patchy or limited in some geographic or temporal areas. The findings begin to reflect broader system variability but important gaps remain (e.g. the research could still expand to include important and different farm types)." = 2,
    "Broad and representative - our data comes from many farms and seasons (> 5 years), and covers different landscape types, making findings broadly representative of the system’s spatial and temporal variability." = 3
  ),
  
  Limiting_factors_score = c(
    "No research or information on threats or limiting factors exists in this specific study system." = 0,
    "Anecdotal evidence, informal observations, or studies from this crop elsewhere, suggest some possible threats (e.g., habitat loss), but without systematic investigation or confirmation in this specific system." = 1,
    "Some empirical studies or surveys identify several key limiting factors in this specific system (e.g., roost loss, pesticides), but evidence is incomplete, inconsistent, or geographically limited." = 2,
    "Strong empirical evidence identifies key limiting factors in this system with clear relevance to bat conservation or pest suppression." = 3
  ),
  
  Knowledge_management_score = c(
    "No strategies have been proposed for bat conservation or pest suppression in this cropping system." = 0,
    "Some ideas or untested strategies (e.g., installing artificial roosts) exist but have not been adapted to this system or tested rigorously." = 1,
    "Several management strategies have been tried or tested in limited contexts in this system, showing promise but with incomplete validation (short-term trials, lack of control groups) or limited geographic application (local trials without broad replicates) or unclear relationship between the management strategies tested and the mechanisms." = 2,
    "Proven, effective strategies exist that are tailored to this system, with strong evidence of local relevance and documented success in improving bat conservation or pest suppression by bats." = 3
  ),
  
  Testing_management_score = c(
    "No strategies have been implemented, so none have been monitored." = 0,
    "Irregular monitoring or anecdotal evidence to support their effectiveness, but data are not consistently or formally used to adapt or improve management strategies." = 1,
    "Monitoring is in place but limited in spatial or temporal scale, and data are not consistently or formally used to adapt or improve management strategies." = 2,
    "Monitoring is robust, systematic, and clearly informs management practices." = 3
  ),
  
  Implementation_score = c(
    "No implementation of bat-supportive strategies occurs anywhere in the system." = 0,
    "Isolated or pilot implementations exist on a limited scale, often within research projects only." = 1,
    "Moderate adoption exists across some farms or stakeholder groups, but implementation is uneven or not widespread." = 2,
    "Widespread and sustained implementation of bat-supportive management practices occurs across the majority of relevant farms in the region." = 3
  )
)

clean_text <- function(x) {
  x %>%
    trimws() %>%          # remove leading/trailing spaces
    stringr::str_squish()  # collapse any internal double/multiple spaces to one
}

# =========================
# 4.b. Apply scoring 
# =========================
survey_1.1 <- survey_1

q10_name <- names(survey_1.1)[str_detect(names(survey_1.1), "^10a\\)")]
q11_name <- names(survey_1.1)[str_detect(names(survey_1.1), "^11a\\)")]
q12_name <- names(survey_1.1)[str_detect(names(survey_1.1), "^12a\\)")]
q13_name <- names(survey_1.1)[str_detect(names(survey_1.1), "^13a\\)")]
q14_name <- names(survey_1.1)[str_detect(names(survey_1.1), "^14a\\)")]
q15_name <- names(survey_1.1)[str_detect(names(survey_1.1), "^15a\\)")]

survey_1.2 <- survey_1.1 %>%
  mutate(
    q10a = .data[[q10_name]],
    q11a = .data[[q11_name]],
    q12a = .data[[q12_name]],
    q13a = .data[[q13_name]],
    q14a = .data[[q14_name]],
    q15a = .data[[q15_name]])

survey_1.2 <- survey_1.2 %>%
  mutate(
    `10a. Evidence Score` =
      unname(scores_list$Bats_role_score[clean_text(q10a)]),
    `11a. Representativeness Score` =
      unname(scores_list$Evidence_rep_score[clean_text(q11a)]),
    `12a. Limiting Factors Score` =
      unname(scores_list$Limiting_factors_score[clean_text(q12a)]),
    `13a. Available Strategies Score` =
      unname(scores_list$Knowledge_management_score[clean_text(q13a)]),
    `14a. Monitoring Strategies Score` =
      unname(scores_list$Testing_management_score[clean_text(q14a)]),
    `15a. Implementing Strategies Score` =
      unname(scores_list$Implementation_score[clean_text(q15a)])
  )


# =========================
# 5. Add unique responder.ID
# =========================
# Sort unique names alphabetically, then assign letters
respondent_ids <- tibble(
  Respondent.clean = unique(survey_1.2$Respondent.clean)) %>%
  mutate(Respondent.ID = paste0("R", row_number()))
# Join to main dataframe
survey_1.2 <- survey_1.2 %>%
  left_join(respondent_ids, by = "Respondent.clean") %>%
  relocate(Respondent.ID, .after = Respondent.clean)

# =========================
# Quick check
# =========================
#View(survey_1.2)
print(survey_1.2)
names(survey_1.2)


# =========================
# 6. Export clean scored datasheets
# =========================
# specify columns to export
survey_1_export <- survey_1.2[, 1:48]
# write files
#write_xlsx(survey_1_export, "Analysis/clean data/Survey 1_scored_near complete_16.6.26.xlsx")
#write.csv(survey_1_export, "Analysis/clean data/Survey 1_scored_near complete_16.6.26.csv", row.names = FALSE)
