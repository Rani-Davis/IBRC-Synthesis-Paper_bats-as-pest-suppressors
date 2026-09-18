# Code author: Rani Davis and Nayelli Rivera Villanueva  
# Last updated: 18 Sep 2026

# ----------------------------------------
# Packages
# ----------------------------------------

# ! Before running, ensure your DHARMa package is version 0.5.0

library(patchwork); library(visreg); library(glmmTMB); library(DHARMa)
library(tidyverse)          # includes dplyr, ggplot2, tidyr, tibble, purrr, readr
library(broom.mixed); library(ggrepel); library(AICcmodavg)
library(plotly); library(car); library(openxlsx); library(corrplot)
library(performance)


# data 
#Survey_1_long_indices_wLatLong_wRichness <- read_csv("Analysis/clean data/Survey 1_scored_with indices_w Lat Long_w Richness.csv") %>% mutate(RowID = row_number())


# ========================================
# Notes on covariates to include in all models, and error family:
# ========================================
# COUNTRY:-------------------------------------------------------------------
# Need to include 'Country.clean' as a random effect (1|Country):
# This accounts for multiple respondents per country sharing the same GDP/Env Performance etc value

# PARTICIPANT:-----------------------------------------------------------------
# Need to include 'Respondent.clean' as a random effect (1|Respondent):
# This accounts for multiple responses per person

# NUMBER OF RESPONDENTS / COUNTRY:---------------------------------------------
# Because countries with more responses should have more stable estimates?

# COUNTRY-LEVEL SOCIO-ECONOMIC INDICES to explore -----------------------------
#   - GDP_per_capita (log10)               - national resourcing / access to extension services
#   - AgForestryFish_ValueAdded_percentGDP - how economically central agriculture is nationally
#   - AgResearchAndDev_PPP2017 (log10)     - agriculture-specific R&D investment
#   - EnviroPerformance_score              - national environmental policy/priority

# Potential PREDICTOR VARIABLES to include:----------------------------------
#   -   Number of bat species. More diversity could = a more complex study system, therefore harder to gain knowledge?
#   -   Crop type, some crops may be more studied due to higher $$$. Can we get values for yield/or value per crop type? e.g. https://ourworldindata.org/crop-yields#explore-data-on-crop-yields
          # Although it will be hard to extract crop-specific data for all countries represented, and we can only include 2 predictors (as stated below)

# ERROR FAMILY for modelling: ------------------------------------------------
#   Cannot be linear/gaussian because it assumes the value/scores can be anything
#   But really our mean scores are always between 0 and 3
#   Beta is used when outcomes/scores are bounded between two known limits (0 and 3 in our case)
# Need to rescale scores to be between 0 and 1 for beta:

# -------------------------------------------------------------------
# What is our PREDICTOR BUDGET (aka how many predictors can we have)?
# Since our socio-economic variables vary at the country level"
Survey_1_long_indices_wLatLong_wRichness %>% distinct(Country.clean) %>% arrange(Country.clean) %>% print(n = 50)
# We have 23 unique country level clusters (considering Peru and Peru & Columbia as one...)
# Following the 10 observations per predictor rule, that means we can have 2 predictors / fixed effects
# We can have additional random effects 
# Prioritising our 2 predictors:
#  -  a: socio-economic predictor
#  -  b: bat species richness


            
# ==============================================================================
# STEP 1: Collapse long-format data to one row per respondent entry (de-duplicate before modelling)
# ==============================================================================
# 1a. Set the covariates -------------------------------------------------------
covariate_vars <- c("GDP_per_capita", 
                    # "AllResearchAndDev_percentGPD", # <- remove this predictor as our dataset is incomplete
                    "AgForestryFish_ValueAdded_percentGDP",
                    "AgResearchAndDev_PPP2017", "EnviroPerformance_score", "BatRichness")

# Summary stats (national covariates de-duplicated by country; latitude is respondent-level)
national_vars <- setdiff(covariate_vars, "Exact_latitude")# Confirm national covariates are constant within country (should return 0 rows)


# 1b. Collapse dataframe, filter out NAs for our variables etc ------------------
Survey_1_long_indices_wLatLong_wRichness %>%
  filter(!is.na(Country.clean)) %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  count(Country.clean) %>%
  filter(n > 1)

respondent_level_vars <- c(
  "Respondent.clean", "Respondent.ID", "Respondent.entry.ID", "Respondent.entry.label",
  "Region.within.country.clean", "Country.clean", "World.region.clean",
  "Crop.clean", "Crop.type.clean",
  "TotalScore.allSteps", "MeanScore.allSteps", "GeoMeanScore.allSteps",
  "Country.WB", "GDP_per_capita", "AgForestryFish_ValueAdded_percentGDP",
  # "AllResearchAndDev_percentGPD", <- no longer including this variable in model selection  as there is no data availavel for some countries ("Eswatini"   "Madagascar" "Ghana"      "Cameroon"   "Fiji" )
  "EnviroPerformance_score", "AgResearchAndDev_PPP2017",
  "Taiwan_imputed_from_China",
  "Exact_latitude", "Exact_longitude",
  "Coarse_latitude", "Coarse_longitude", "BatRichness")

# Check the CORRECT key: Respondent.entry.label
# Should return 0 rows
Survey_1_long_indices_wLatLong_wRichness %>%
  group_by(Respondent.entry.label) %>%
  summarise(across(all_of(setdiff(respondent_level_vars, c("Respondent.entry.label", "Respondent.entry.ID"))),
                   ~ n_distinct(.)), .groups = "drop") %>%
  pivot_longer(-Respondent.entry.label, names_to = "variable", values_to = "n_distinct") %>%
  filter(n_distinct > 1)

# Now collapse to one row per respondent entry
Survey_1_respondent <- Survey_1_long_indices_wLatLong_wRichness %>%
  select(all_of(respondent_level_vars)) %>%
  distinct(Respondent.entry.label, .keep_all = TRUE)

nrow(Survey_1_respondent) == n_distinct(Survey_1_long_indices_wLatLong_wRichness$Respondent.entry.label)  # should be TRUE




# ==============================================================================
# STEP 1: Data checks — spread, distributions, country-covariate consistency
# ==============================================================================
# 1a. data spread -------------------------------------------------------
# explore spread of data for each index to determine whether they need to be log transformed:
Survey_1_long_indices_wLatLong_wRichness %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  pivot_longer(cols = all_of(national_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(n = sum(!is.na(value)), min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE), median = median(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            ratio_max_min = max / min, skewness = (mean - median) / sd, .groups = "drop") %>%
  arrange(desc(abs(skewness)))
# AgResearchAndDev_PPP2017 is the predictor with most spread of all (1332x ratio between the max and min)
# GDP also has a 161x ratio between min and max
# Bat Richness has a 116x ratio between min and max
# Thus GDP and Ag R&D and BatRichness get a log10() transform because they're wildly skewed

# 1b. Histograms -------------------------------------------------------
# ---- 1. Histogram of the raw MeanScore.allSteps (0-3 scale) ----
p1 <- ggplot(Survey_1_respondent, aes(x = MeanScore.allSteps)) +
  geom_histogram(binwidth = 0.2, boundary = 0, fill = "steelblue", colour = "white") +
  labs(title = "MeanScore.allSteps (0-3)", x = "Mean score", y = "Count") + theme_minimal()

# ---- 2. Histogram of TotalScore.allSteps (0-15 scale) ----
p2 <- ggplot(Survey_1_respondent, aes(x = TotalScore.allSteps)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "darkorange", colour = "white") +
  labs(title = "TotalScore.allSteps (0-15)", x = "Total score", y = "Count") + theme_minimal()

# ---- 3. Proportion of respondents scoring the FLOOR (0) and CEILING (max) ----
Survey_1_respondent %>%
  summarise(
    prop_zero  = mean(MeanScore.allSteps == 0, na.rm = TRUE),
    prop_max   = mean(MeanScore.allSteps == 3, na.rm = TRUE),
    n = n()) 
# prop_zero prop_max- 0.0267  
# No excess of zeros, only 2.7% of respondents are exactly zero..

p1 / p2

# ---- 4. Distribution of EACH individual domain score (before aggregating) ----
levels(Survey_1_long_indices_wLatLong_wRichness$Score.type)

p3 <- Survey_1_long_indices_wLatLong_wRichness %>%
  ggplot(aes(x = Score)) +
  geom_bar(fill = "seagreen") +
  facet_wrap(~ Score.type, scales = "free_y") +
  labs(title = "Distribution of each 0-3 domain score", x = "Score", y = "Count") +
  theme_minimal()

par(mfrow = c(2, 3))
p3
# we don't really care about scores for Representativeness? Should those scores be included in the Total Score?

# ---- 5. Distribution of Bat Richness data  ----
p4 <- ggplot(Survey_1_respondent, aes(x = BatRichness)) +
  geom_histogram(binwidth = 10, boundary = 0, fill = "purple", colour = "white") +
  labs(title = "Bat Richness per country", x = "Bat richness", y = "Count") + theme_minimal()
p4

# 1c. Explore raw relationship of predictors with mean score -------------------
# ---- 1.CropType ~ Mean score
S1_p_croptype_box <- ggplot(Survey_1_long_indices_wLatLong_wRichness, aes(x = Crop.type.clean, y = MeanScore.allSteps,
                                                                          text = paste0("Respondent: ", Respondent.clean,"<br>Country: ", Country.clean,
                                                                                        "<br>Crop type: ", Crop.type.clean,"<br>Score: ", round(MeanScore.allSteps, 2)))) +
  geom_boxplot(aes(fill = Crop.type.clean), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1, colour = "grey20") +
  labs(x = "Crop type", y = "Mean Knowledge Score (0-3)") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_fill_viridis_d(option = "turbo") +theme_minimal() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(S1_p_croptype_box, tooltip = "text")
# Check: Is crop type nested within country?
table(Survey_1_long_indices_wLatLong_wRichness$Crop.clean, Survey_1_long_indices_wLatLong_wRichness$Country.clean)
# Crossed, not nested (most crops span >1 country)
# BUT very sparse: many crops occur in only ONE country (e.g. Wheat, Corn, Potato = Israel only), 
# so crop and country effects cannot be separated in those rows. also ~16/29 crop categories are Israel-only.
# What about at the crop.type level (broader)?
table(Survey_1_long_indices_wLatLong_wRichness$Crop.type.clean, Survey_1_long_indices_wLatLong_wRichness$Country.clean)
# Collapsed to 7 broader categories - now every category spans multiple countries,
# Caveat: "Vegetable crops" is still 78% Israel.

# ---- 2. BatRichness ~ mean score -  doesn't look very convincing
S1_Richness_byScore_plot <- ggplot(Survey_1_long_indices_wLatLong_wRichness, aes(x = BatRichness, y = MeanScore.allSteps,
                                                                                 text = paste0("Respondent: ", Respondent.clean,
                                                                                               "<br>Country: ", Country.clean,
                                                                                               "<br>Bat richness: ", BatRichness,
                                                                                               "<br>Score: ", round(MeanScore.allSteps, 2)))) +
  geom_point(alpha = 0.7, size = 2, colour = "purple") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.8) +
  labs(x = "Bat species richness (country-level)", y = "Mean Knowledge Score (0-3)") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal()
S1_Richness_byScore_plot

# ---- 3. GDP per capita ~ mean score ----
S1_GDP_byScore_plot <- ggplot(Survey_1_long_indices_wLatLong_wRichness, aes(x = GDP_per_capita, y = MeanScore.allSteps,
                                                                            text = paste0("Respondent: ", Respondent.clean,
                                                                                          "<br>Country: ", Country.clean,
                                                                                          "<br>GDP per capita: ", round(GDP_per_capita, 2),
                                                                                          "<br>Score: ", round(MeanScore.allSteps, 2)))) +
  geom_point(alpha = 0.7, size = 2, colour = "purple") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.8) +
  labs(x = "GDP per capita", y = "Mean Knowledge Score (0-3)") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal()

# ---- 4. Ag/Forestry/Fishing value added (% GDP) ~ mean score ----
S1_AgValueAdded_byScore_plot <- ggplot(Survey_1_long_indices_wLatLong_wRichness, aes(x = AgForestryFish_ValueAdded_percentGDP, y = MeanScore.allSteps,
                                                                                     text = paste0("Respondent: ", Respondent.clean,
                                                                                                   "<br>Country: ", Country.clean,
                                                                                                   "<br>Ag/Forestry/Fish value added (% GDP): ", round(AgForestryFish_ValueAdded_percentGDP, 2),
                                                                                                   "<br>Score: ", round(MeanScore.allSteps, 2)))) +
  geom_point(alpha = 0.7, size = 2, colour = "purple") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.8) +
  labs(x = "Agriculture/Forestry/Fishing\nValue Added (% GDP)", y = "Mean Knowledge Score (0-3)") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal()

# ---- 5. Ag R&D spending (PPP 2017) ~ mean score ----
S1_AgRD_byScore_plot <- ggplot(Survey_1_long_indices_wLatLong_wRichness, aes(x = AgResearchAndDev_PPP2017, y = MeanScore.allSteps,
                                                                             text = paste0("Respondent: ", Respondent.clean,
                                                                                           "<br>Country: ", Country.clean,
                                                                                           "<br>Ag R&D (PPP 2017): ", round(AgResearchAndDev_PPP2017, 2),
                                                                                           "<br>Score: ", round(MeanScore.allSteps, 2)))) +
  geom_point(alpha = 0.7, size = 2, colour = "purple") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.8) +
  labs(x = "Ag R&D Spending (PPP 2017)", y = "Mean Knowledge Score (0-3)") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal()

# ---- 6. Environmental Performance score ~ mean score ----
S1_EPS_byScore_plot <- ggplot(Survey_1_long_indices_wLatLong_wRichness, aes(x = EnviroPerformance_score, y = MeanScore.allSteps,
                                                                            text = paste0("Respondent: ", Respondent.clean,
                                                                                          "<br>Country: ", Country.clean,
                                                                                          "<br>EnviroPerformance: ", round(EnviroPerformance_score, 2),
                                                                                          "<br>Score: ", round(MeanScore.allSteps, 2)))) +
  geom_point(alpha = 0.7, size = 2, colour = "purple") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.8) +
  labs(x = "Environmental Performance Score", y = "Mean Knowledge Score (0-3)") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal()

# ---- Combine into one 2x2 layout ----
S1_indices_combined_plot <- (S1_GDP_byScore_plot + S1_AgValueAdded_byScore_plot) /
  (S1_AgRD_byScore_plot + S1_EPS_byScore_plot) +
  plot_annotation(title = "Mean Knowledge Score by Country-Level Index (Survey 1)")

S1_indices_combined_plot


# ==============================================================================
# STEP 3: Correlation matrix — check collinearity between indices BEFORE including multiple in the same model
# ==============================================================================
cor_matrix <- Survey_1_long_indices_wLatLong_wRichness %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  mutate(log10_GDP = log10(GDP_per_capita),
         #log10_AllRD = log10(AllResearchAndDev_percentGPD), # <- no longer including in models
         log10_AgGDP = log10(AgForestryFish_ValueAdded_percentGDP),
         log10_AgRD = log10(AgResearchAndDev_PPP2017),
         log10_BatRichness = log10(BatRichness)) %>%
  select(EnviroPerformance_score, log10_GDP,
         #log10_AllRD,
         log10_AgGDP, log10_AgRD, log10_BatRichness) %>%
  cor(use = "complete.obs")

round(cor_matrix, 2)

par(mfrow = c(2, 1))
corrplot::corrplot(cor_matrix, method = "number", type = "upper", diag = FALSE)
corrplot::corrplot(cor_matrix, method = "ellipse", type = "upper", diag = FALSE)
# Only combine two predictors in one model if |cor| < 0.6

# Conclusion: The only socio-economic indices that be included together in a model are:
# log10_AgGDP & log10AgRD
# log10_GDP & log10AgRD
# EnviroPerformance_score & log10AgRD
# But we only have a predictor budget of 2, and we should prioritise bat richness?



# ==============================================================================
# STEP 4: Set up the complete data set for model selection
# ==============================================================================
# 4a. check which variables we set as covariate_vars, then filter out NA's----------
covariate_vars

Survey_1_complete <- Survey_1_respondent %>%             
  filter(if_all(all_of(covariate_vars), ~ !is.na(.)))

# 4b. Check the data remaining after filtering out NA's ----------------------------
nrow(Survey_1_respondent) # originally was 80, for both complete surveys
nrow(Survey_1_complete)   # but the model dataset drops to 75 due to incomplete responses

countries_full <- Survey_1_respondent %>% distinct(Country.clean) %>% pull(Country.clean)
countries_complete <- Survey_1_complete %>% distinct(Country.clean) %>% pull(Country.clean)
length(countries_full) # 25 countries in the full dataset
length(countries_complete) # 24 countries in the model dataset (due to incomplete responses)
countries_full
countries_complete # only lost the NA's (incomplete responses)

setdiff(countries_full, countries_complete) # Only NA! Good

# 4c. Rescale the mean score data --------------------------------------------------
# Needs to be between 0 and 1 for beta regression models:
Survey_1_complete <- Survey_1_complete %>%
  mutate(MeanScore.allSteps.prop = MeanScore.allSteps / 3,   # rescaled to 0-1 for beta regression
         n_obs = n(),
         Survey_1_mean_rescaled_beta = (MeanScore.allSteps.prop * (n_obs - 1) + 0.5) / n_obs)  # boundary-adjusted, beta-ready



# ==============================================================================
# STEP 4: Set up the complete data set for model selection
# ==============================================================================
# 4a. check which variables we set as covariate_vars, then filter out NA's----------
covariate_vars

Survey_1_complete <- Survey_1_respondent %>%             
  filter(if_all(all_of(covariate_vars), ~ !is.na(.)))

# 4b. Check the data remaining after filtering out NA's ----------------------------
nrow(Survey_1_respondent) # originally was 80, for both complete surveys
nrow(Survey_1_complete)   # but the model dataset drops to 75 due to incomplete responses

countries_full <- Survey_1_respondent %>% distinct(Country.clean) %>% pull(Country.clean)
countries_complete <- Survey_1_complete %>% distinct(Country.clean) %>% pull(Country.clean)
length(countries_full) # 25 countries in the full dataset
length(countries_complete) # 24 countries in the model dataset (due to incomplete responses)
countries_full
countries_complete # only lost the NA's (incomplete responses)

setdiff(countries_full, countries_complete) # Only NA! Good

# 4c. Rescale the mean score data --------------------------------------------------
# Needs to be between 0 and 1 for beta regression models:
Survey_1_complete <- Survey_1_complete %>%
  mutate(MeanScore.allSteps.prop = MeanScore.allSteps / 3,   # rescaled to 0-1 for beta regression
         n_obs = n(),
         Survey_1_mean_rescaled_beta = (MeanScore.allSteps.prop * (n_obs - 1) + 0.5) / n_obs)  # boundary-adjusted, beta-ready



# ==============================================================================
# STEP 5: MODEL SELECTION for Survey 1 — Mean Knowledge pathway score
# ==============================================================================
# --- 5A. Null and single-covariate models (without BatRichness/CropType) ----
S1_null_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ 1
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Lat_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ abs(Exact_latitude)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Ag_GDP_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Ag_RD_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_EP_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5B. BatRichness alone, and paired with each single covariate -----------
# NOTE: run the extended collinearity check (BatRichness vs each socio-economic index/latitude) before trusting these pairs - drop any pair with |cor| > 0.6
S1_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Lat_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ abs(Exact_latitude) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Ag_GDP_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Ag_RD_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_EP_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5C. Crop.type.clean alone, and paired with each single covariate ------
# NOTE: Crop.type.clean = 6 df (entry-level, not country-level), so pairing it with a country-level covariate spends 7 fixed-effect parameters from n=75 / 
# ~24 country clusters — well beyond the 10-obs-per-parameter guideline used to set the original 2-predictor budget. Treat these as exploratory.
S1_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Lat_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ abs(Exact_latitude) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Ag_GDP_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_Ag_RD_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_EP_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(BatRichness) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5D. Non-collinear socio-economic pairs (without BatRichness/CropType) --
# remember can only include those with |cor| < 0.6
# log10_AgGDP & log10AgRD
# log10_GDP & log10AgRD
# EnviroPerformance_score & log10AgRD
S1_EP_AgRD_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_AgRD_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_AgGDP_AgRD_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5E. Same non-collinear pairs, WITH BatRichness added (3-predictor models) ----
# NOTE: with only ~24 country clusters, a 3-fixed-effect model may exceed budget
S1_EP_AgRD_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_AgRD_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_AgGDP_AgRD_BatRichness_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5F. Same non-collinear pairs, WITH Crop.type.clean added (3-predictor + 6df factor) ----
# NOTE: these spend ~8-9 fixed-effect parameters from n=75 / ~24 clusters.
# High risk of unstable estimates or convergence warnings — check carefully.
S1_EP_AgRD_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_AgRD_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_AgGDP_AgRD_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5G. Non-collinear pairs, WITH BatRichness AND Crop.type.clean (4 predictors) ----
# NOTE: ~9-10 fixed-effect parameters from n=75 / ~24 clusters. Almost certainly over-specified for this sample size —
# included here only for completeness of testing alternative models, but expect these to be flagged/dropped on predictor budget grounds.
S1_EP_AgRD_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_GDP_AgRD_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)

S1_AgGDP_AgRD_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete)


# --- 5H. Full AICc comparison table ----------------------------------------
S1_candidates <- list(
  null                              = S1_null_glmmTMB,
  Lat                               = S1_Lat_glmmTMB,
  GDP                               = S1_GDP_glmmTMB,
  Ag_GDP                            = S1_Ag_GDP_glmmTMB,
  Ag_RD                             = S1_Ag_RD_glmmTMB,
  EP                                = S1_EP_glmmTMB,
  BatRichness                       = S1_BatRichness_glmmTMB,
  Lat_BatRichness                   = S1_Lat_BatRichness_glmmTMB,
  GDP_BatRichness                   = S1_GDP_BatRichness_glmmTMB,
  Ag_GDP_BatRichness                = S1_Ag_GDP_BatRichness_glmmTMB,
  Ag_RD_BatRichness                 = S1_Ag_RD_BatRichness_glmmTMB,
  EP_BatRichness                    = S1_EP_BatRichness_glmmTMB,
  CropType                          = S1_CropType_glmmTMB,
  Lat_CropType                      = S1_Lat_CropType_glmmTMB,
  GDP_CropType                      = S1_GDP_CropType_glmmTMB,
  Ag_GDP_CropType                   = S1_Ag_GDP_CropType_glmmTMB,
  Ag_RD_CropType                    = S1_Ag_RD_CropType_glmmTMB,
  EP_CropType                       = S1_EP_CropType_glmmTMB,
  BatRichness_CropType              = S1_BatRichness_CropType_glmmTMB,
  EP_AgRD                           = S1_EP_AgRD_glmmTMB,
  GDP_AgRD                          = S1_GDP_AgRD_glmmTMB,
  AgGDP_AgRD                        = S1_AgGDP_AgRD_glmmTMB,
  EP_AgRD_BatRichness               = S1_EP_AgRD_BatRichness_glmmTMB,
  GDP_AgRD_BatRichness              = S1_GDP_AgRD_BatRichness_glmmTMB,
  AgGDP_AgRD_BatRichness            = S1_AgGDP_AgRD_BatRichness_glmmTMB,
  EP_AgRD_CropType                  = S1_EP_AgRD_CropType_glmmTMB,
  GDP_AgRD_CropType                 = S1_GDP_AgRD_CropType_glmmTMB,
  AgGDP_AgRD_CropType               = S1_AgGDP_AgRD_CropType_glmmTMB,
  EP_AgRD_BatRichness_CropType      = S1_EP_AgRD_BatRichness_CropType_glmmTMB,
  GDP_AgRD_BatRichness_CropType     = S1_GDP_AgRD_BatRichness_CropType_glmmTMB,
  AgGDP_AgRD_BatRichness_CropType   = S1_AgGDP_AgRD_BatRichness_CropType_glmmTMB
)

S1_AICc_full <- purrr::map_dfr(
  S1_candidates,
  function(mod) {
    data.frame(
      formula   = gsub("\\s+", " ", paste(deparse(formula(mod)), collapse = " ")),
      k         = length(fixef(mod)$cond) + 1,
      logLik    = as.numeric(logLik(mod)),
      AIC       = AIC(mod),
      AICc      = AICc(mod)
    )
  },
  .id = "model"
) %>%
  arrange(AICc) %>%
  mutate(
    delta_AICc = AICc - min(AICc),
    AICc_weight = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)),
    meaningful_support = delta_AICc < 2
  ) %>%
  select(model, formula, k, logLik, AIC, AICc, delta_AICc, AICc_weight, meaningful_support)

S1_AICc_full

write.xlsx(S1_AICc_full, "Analysis/model selection/Survey 1_model comparison table.xlsx")

# Model with Enviro Performance is best


# --- 5F. Compare the top models within 2 AICcs of the best --------------------
# Compare model structure for EP & EP_BatRichness
formula(S1_EP_glmmTMB)
formula(S1_EP_BatRichness_glmmTMB) # The only difference is the addition of log10(BatRichness)

# Full summaries of both models
summary(S1_EP_glmmTMB)
summary(S1_EP_BatRichness_glmmTMB) # BatRichness is not significant

# Visualise effects for both models
par(mfrow = c(1, 1))
visreg(S1_EP_glmmTMB, "EnviroPerformance_score", scale = "response", partial = TRUE)
par(mfrow = c(1, 2))
visreg(S1_EP_BatRichness_glmmTMB, "EnviroPerformance_score", scale = "response", partial = TRUE)
visreg(S1_EP_BatRichness_glmmTMB, "BatRichness", scale = "response", partial = TRUE)

# Compare R^2 - does adding richness meaningfully increase the variance explained?
S1_R2_EP <- r2(S1_EP_glmmTMB)
S1_R2_EP_BatRichness <- r2(S1_EP_BatRichness_glmmTMB)
S1_R2_comparison <- data.frame(
  model = c("EP", "EP_BatRichness"),
  R2_marginal    = c(S1_R2_EP$R2_marginal, S1_R2_EP_BatRichness$R2_marginal),
  R2_conditional = c(S1_R2_EP$R2_conditional, S1_R2_EP_BatRichness$R2_conditional))
S1_R2_comparison  # Increased from 39.8% to 46.5% when we add richness, but getting a Singular fit error
# Check singular fit error
performance::check_singularity(S1_EP_glmmTMB) # TRUE - explore in 5G
performance::check_singularity(S1_EP_BatRichness_glmmTMB) # TRUE
# If you look at the model summaries, Respondent.clean explains a VERY small amount of variation, likely culprit of singular fit

# Likelihood ratio test — does adding BatRichness improve fit?
S1_LRT_EP_vs_BatRichness <- anova(S1_EP_glmmTMB, S1_EP_BatRichness_glmmTMB)
S1_LRT_EP_vs_BatRichness  # no significant improvement in model fit when we add richness

# Compare coefficients and p values:
S1_EP_coefs <- broom.mixed::tidy(S1_EP_glmmTMB, effects = "fixed", conf.int = TRUE) %>% mutate(model = "EP")
S1_EP_BatRichness_coefs <- broom.mixed::tidy(S1_EP_BatRichness_glmmTMB, effects = "fixed", conf.int = TRUE) %>% mutate(model = "EP_BatRichness")
S1_coef_comparison <- bind_rows(S1_EP_coefs, S1_EP_BatRichness_coefs) %>%
  select(model, term, estimate, std.error, conf.low, conf.high, statistic, p.value)
S1_coef_comparison

# Decision - continue with the more parsimonious (simpler) model of S1_EP_glmmTMB because:
# BatRichness was not a significant predictor of mean knowledge scores in the more complex model
# Inclduing BatRichness did not sig improve the model fit


# --- 5G. Sensitivity check: explore singular fit - is the EnvPerformance effect driven by one respondent? -------
# Step 5F found Respondent.clean variance ~0 (singular fit) in both EP and EP_BatRichness. 
# Check whether it reflects genuine absence of respondent-level variation, or is a structural artifact of imbalance in the respondent/country data
respondent_entry_counts <- Survey_1_complete %>%
  count(Respondent.clean, name = "n_entries") %>%
  mutate(pct_of_total = round(100 * n_entries / sum(n_entries), 1)) %>%
  arrange(desc(n_entries))

respondent_entry_counts # CK contributed 21/75 entries (28% of the dataset) — flag for follow-up

Survey_1_complete %>% filter(Respondent.clean == "CK") %>% distinct(Country.clean)
Survey_1_complete %>% filter(Country.clean == "Israel") %>% distinct(Respondent.clean)
# CK is Israel's ONLY respondent -> Country.clean and Respondent.clean are perfectly confounded for this entry
# likely explains the singular fit: the model can't separate "Israel's effect" from "CK's effect".

Survey_1_complete_noCK <- Survey_1_complete %>%
  filter(Respondent.clean != "CK")

nrow(Survey_1_complete)       # 75
nrow(Survey_1_complete_noCK)  # 54

# Sensitivity check: Re-run model without CK data to see if the results change
S1_EP_noCK_glmmTMB <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_complete_noCK)

summary(S1_EP_noCK_glmmTMB)
performance::check_singularity(S1_EP_noCK_glmmTMB) # no singular fit
# Respondent.clean variance reappears once CK/Israel's confound is removed
# EnviroPerformance_score effect holds (direction, magnitude, and significance all consistent/strengthened)
# the reported effect is not an artifact of CK's disproportionate contribution.
# See Step 6 for an independent confirmation via residual diagnostics.

S1_sensitivity_comparison <- bind_rows(
  broom.mixed::tidy(S1_EP_glmmTMB, effects = c("fixed","ran_pars"), conf.int = TRUE) %>%
    mutate(model = "Full data (n=74)"),
  broom.mixed::tidy(S1_EP_noCK_glmmTMB, effects = c("fixed","ran_pars"), conf.int = TRUE) %>%
    mutate(model = "Excluding CK (n=53)")
) %>%
  select(model, term, estimate, std.error, conf.low, conf.high, statistic, p.value)

S1_sensitivity_comparison # no sig change in model predictions, magnitude or sig.. continue with S1_EP_glmmTMB



# ==============================================================================
# STEP 6. MODEL DIAGNOSTICS: S1_EP_glmmTMB_fulldata
# ==============================================================================
# This is our best model
formula(S1_EP_glmmTMB)

## But we need to refit it on the full dataset:
Survey_1_respondent_rescaled_beta <- Survey_1_respondent %>%
  mutate(MeanScore.allSteps.prop = MeanScore.allSteps / 3,   # rescaled to 0-1 for beta regression
         n_obs = n(),
         Survey_1_mean_rescaled_beta = (MeanScore.allSteps.prop * (n_obs - 1) + 0.5) / n_obs)  # boundary-adjusted, beta-ready

S1_EP_glmmTMB_fulldata <- glmmTMB(
  Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + 
    + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data = Survey_1_respondent_rescaled_beta )

summary(S1_EP_glmmTMB_fulldata)

## ---- Stage 1: Simulate residuals ----
set.seed(42)
simres_S1_EP_glmmTMB <- simulateResiduals(S1_EP_glmmTMB_fulldata, plot = FALSE)

## ---- Stage 2: Global fit checks ----
plot(simres_S1_EP_glmmTMB)

par(mfrow = c(2, 1))
testDispersion(simres_S1_EP_glmmTMB)  # over/underdispersion -> n.s.
testOutliers(simres_S1_EP_glmmTMB)    # excess extreme residuals -> 2/75 flagged, n.s.
# Result: uniformity, dispersion, and outlier tests all n.s. 
# No zero-inflation test needed as the response is a continuous proportion bounded strictly within (0,1) via rescaling, so "zero-inflation" (a count-data concept) isn't applicable

## ---- Stage 3: Residuals by predictor and grouping factor ----
# glmmTMB drops incomplete cases (NAs) when fitting, so DHARMa's residual vector
# is shorter than the full dataset — build the matching complete-case dataframe:
S1_EP_model_data_noNAs <- Survey_1_respondent_rescaled_beta %>%
  filter(!is.na(EnviroPerformance_score),
         !is.na(Survey_1_mean_rescaled_beta),
         !is.na(Country.clean),
         !is.na(Respondent.clean))

par(mfrow = c(1, 1))
plotResiduals(simres_S1_EP_glmmTMB, form = S1_EP_model_data_noNAs$EnviroPerformance_score)

testCategorical(simres_S1_EP_glmmTMB, catPred = S1_EP_model_data_noNAs$Country.clean)

leveneTest(simres_S1_EP_glmmTMB$scaledResiduals ~ S1_EP_model_data_noNAs$Country.clean)
fligner.test(simres_S1_EP_glmmTMB$scaledResiduals ~ S1_EP_model_data_noNAs$Country.clean)
# All three -> n.s., though power is limited given ~24 countries averaging ~2-3 respondents each.

## ---- Stage 4: Investigate apparent nonlinearity ----
p_raw_EP <- ggplot(Survey_1_complete, aes(x = EnviroPerformance_score, y = Survey_1_mean_rescaled_beta,
                                          text = paste("Country:", Country.clean,
                                                       "<br>Respondent:", Respondent.clean,
                                                       "<br>EnviroPerformance:", round(EnviroPerformance_score, 2),
                                                       "<br>Score:", round(Survey_1_mean_rescaled_beta, 3)))) +
  geom_point(color = "black", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1) +
  labs(x = "EnviroPerformance_score", y = "Rescaled score", title = "Raw relationship") +
  theme_minimal()

ggplotly(p_raw_EP, tooltip = "text")

Survey_1_complete %>% filter(Country.clean == "Israel") %>% pull(EnviroPerformance_score) %>% table()
# -> All 21 Israeli respondents share EnviroPerformance_score = 51.69 (expected, since it's a country-level covariate)
# but 21/75 respondents (28%) at one x-value gives that point strong leverage over the loess curve.

ggplot(Survey_1_complete, aes(x = EnviroPerformance_score, y = Survey_1_mean_rescaled_beta)) +
  geom_point(aes(color = Country.clean == "Israel"), alpha = 0.7) +
  scale_color_manual(values = c("grey60", "red"), labels = c("Other", "Israel")) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(color = "") +
  theme_minimal()

Survey_1_no_Israel <- Survey_1_complete %>% filter(Country.clean != "Israel")

ggplot(Survey_1_no_Israel, aes(x = EnviroPerformance_score, y = Survey_1_mean_rescaled_beta)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal()
# -> Yes: loess flattens with Israel excluded, confirming the apparent nonlinearity is a leverage artifact from one overrepresented country's respondent count,
# not a genuine cross-national dose-response pattern.

## ---- Conclusion ----
# No global diagnostic (uniformity, dispersion, outliers, country variance) flagged a significant problem. 
# The visual bend in residuals/raw data appears to trace to Israel's disproportionate respondent count at a single EnviroPerformance_score value, 
# A quadratic term was considered but rejected: it would spend a df fitting sampling imbalance rather than a genuine effect, and is hard to justify given only ~24 independent country clusters. 
# Linear specification retained.
# Note: this confirms, from an independent angle (residual diagnostics rather than model comparison), the same CK/Israel confounding between country * respondent flagged in Step 5G.



# ==============================================================================
# Step 7. VISUALISE: Effect of EnviroPerformance_score on intervention score (Survey 1)
# ==============================================================================
visreg(S1_EP_glmmTMB_fulldata, "EnviroPerformance_score", scale = "response", partial = TRUE)
par(cex.axis = 0.6)  # shrinks x-axis category labels and y-axis tick labels for next plot
visreg(S1_EP_glmmTMB_fulldata, "Country.clean", scale = "response")
par(cex.axis = 1)  # reset back to default
visreg(S1_EP_glmmTMB_fulldata, "Respondent.clean", scale = "response")

S1_visreg <- visreg(S1_EP_glmmTMB_fulldata, "EnviroPerformance_score", scale = "response", plot = FALSE)

# ---- Rebuild the exact complete-case data used by the model ----
# visreg's $res only contains rows retained after glmmTMB's internal NA-omission, 
# so grouping columns (Country.clean, World.region.clean) must come from the same complete-case subset, in the same row order 
# i.e. NOT from Survey_1_long_indices_wLatLong_wRichness (wrong dataset: that's long-format, one row per domain score, different structure).
# Use S1_EP_model_data_noNAs (built in Step 6):

S1_fit_data <- S1_visreg$fit
S1_fit_data$visregFit <- S1_fit_data$visregFit * 3
S1_fit_data$visregLwr <- S1_fit_data$visregLwr * 3
S1_fit_data$visregUpr <- S1_fit_data$visregUpr * 3

S1_res_data <- S1_visreg$res
S1_res_data$visregRes <- S1_res_data$visregRes * 3
S1_res_data$Country.clean <- S1_EP_model_data_noNAs$Country.clean
S1_res_data$World.region.clean <- S1_EP_model_data_noNAs$World.region.clean
S1_res_data$Respondent.clean <- S1_EP_model_data_noNAs$Respondent.clean
S1_res_data$Crop.type.clean <- S1_EP_model_data_noNAs$Crop.type.clean

# ---- Base plot: fitted line + CI ribbon + partial residuals coloured by country ----
ggplot(S1_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_jitter(data = S1_res_data,
              aes(x = EnviroPerformance_score, y = visregRes, colour = Country.clean),
              width = 0.5, height = 0.05, alpha = 0.7, size = 2) +
  labs(x = "Environmental Performance Score", y = "Mean Knowledge Score (0-3)",
       colour = "Country") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal() +
  theme(legend.text = element_text(size = 8))

# ---- Add n per country to legend labels ----
S1_country_n <- S1_EP_model_data_noNAs %>%
  count(Country.clean, name = "n")

S1_res_data <- S1_res_data %>%
  select(-any_of(c("n", "Country.label"))) %>%
  left_join(S1_country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

ggplot(S1_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(data = S1_res_data,
             aes(x = EnviroPerformance_score, y = visregRes, colour = Country.label),
             alpha = 0.7, size = 2,
             position = position_jitter(width = 0.5, height = 0.05, seed = 1)) +
  geom_text_repel(data = S1_res_data,
                  aes(x = EnviroPerformance_score, y = visregRes, label = Country.clean,
                      colour = Country.label),
                  size = 2.5, show.legend = FALSE,
                  position = position_jitter(width = 0.5, height = 0.05, seed = 1),
                  max.overlaps = 20, segment.size = 0.2) +
  labs(x = "Environmental Performance Score", y = "Mean Knowledge Score (0-3)",
       colour = "Country") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_viridis_d(option = "turbo") +
  theme_minimal() +
  theme(legend.text = element_text(size = 8))

# ---- Same plot, coloured by world region instead of country ----
S1_region_n <- S1_EP_model_data_noNAs %>%
  count(World.region.clean, name = "n_region")

S1_res_data <- S1_res_data %>%
  select(-any_of(c("n_region", "Region.label"))) %>%
  left_join(S1_region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

# Map your custom region_colours onto the "Region (n=X)" labels
S1_region_label_lookup <- S1_res_data %>%
  distinct(World.region.clean, Region.label) %>%
  mutate(colour = region_colours[World.region.clean])

S1_region_colours_labelled <- setNames(S1_region_label_lookup$colour, S1_region_label_lookup$Region.label)

ggplot(S1_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(data = S1_res_data,
             aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
             alpha = 0.75, size = 2.2,
             position = position_jitter(width = 0.5, height = 0.05, seed = 1)) +
  geom_text_repel(data = S1_res_data,
                  aes(x = EnviroPerformance_score, y = visregRes, label = Country.clean,
                      colour = Region.label),
                  size = 2.4, show.legend = FALSE,
                  position = position_jitter(width = 0.5, height = 0.05, seed = 1),
                  max.overlaps = 20, segment.size = 0.2) +
  labs(x = "Environmental Performance Score", y = "Mean Knowledge Score (0-3)",
       colour = "World Region") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_manual(values = S1_region_colours_labelled) +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10))


# ---- Same plot but interactive to see respondent, crop type etc when hovering over points
S1_res_data <- S1_res_data %>%
  mutate(hover_text = paste0("Respondent: ", Respondent.clean,
                             "<br>Country: ", Country.clean,
                             "<br>Region: ", World.region.clean,
                             "<br>Crop type: ", Crop.type.clean,
                             "<br>EnviroPerformance: ", round(EnviroPerformance_score, 2),
                             "<br>Score: ", round(visregRes, 2)))

S1_p_region <- ggplot(S1_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(data = S1_res_data,
             aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label, text = hover_text),
             alpha = 0.75, size = 2.2,
             position = position_jitter(width = 0.5, height = 0.05, seed = 1)) +
  labs(x = "Environmental Performance Score", y = "Mean Knowledge Score (0-3)",
       colour = "World Region") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_manual(values = S1_region_colours_labelled) +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10))

ggplotly(S1_p_region, tooltip = "text")



