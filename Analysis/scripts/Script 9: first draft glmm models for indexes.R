# Code author: Rani Davis
# Last updated: 22 July 2026
#
# ----------------------------------------
# Packages
# ----------------------------------------
library(lme4)
library(visreg)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(emmeans)

# ========================================
# Notes on covariates to include in all models:
# ========================================
# COUNTRY:
    # Need to include 'Country.clean' as a random effect (1|Country):
    # This accounts for multiple respondents per country sharing the same GDP/Env Performance etc value

# NUMBER OF RESPONDENTS / COUNTRY:
    # Because countries with more responses should have more stable estimates?

# Potential confounding variables to include:
#   -   Latitude affects biodiversity, more diversity could = a complex study system and therefore it could be harder to gain knowledge?
#   -   Bat species diversity, could be an alternative and more direct measure than latitude. (But pest diversity is also an important consideration)
#   -   Crop type, some crops may be more studied due to higher $$$. Can we get values for yield/or value per crop type? e.g. https://ourworldindata.org/crop-yields#explore-data-on-crop-yields

# Candidate covariates being explored one at a time in this first pass:
#   - GDP_per_capita (log10)               - national resourcing / access to extension services
#   - AllResearchAndDev_percentGPD         - overall national R&D investment
#   - AgForestryFish_ValueAdded_percentGDP - how economically central agriculture is nationally
#   - AgResearchAndDev_PPP2017 (log10)     - agriculture-specific R&D investment
#   - EnviroPerformance_score              - national environmental policy/priority

Survey_1_long_indices %>% distinct(Country.clean) %>% arrange(Country.clean) %>% print(n = 50)
# ========================================
# Simple lmer model? No covariates except for random country effect
# ========================================
# e.g. for GDP
S1_GDP_lmer <- lmer(MeanScore.allSteps ~ log10(GDP_per_capita) + 
                      (1|Country.clean),
                    data = Survey_1_long_indices)
summary(S1_GDP_lmer)

S1_RD.all_lmer <- lmer(MeanScore.allSteps ~ AllResearchAndDev_percentGPD + (1|Country.clean),
           data = Survey_1_long_indices)

S1_AG.pct.GDP_lmer <- lmer(MeanScore.allSteps ~ AgForestryFish_ValueAdded_percentGDP + (1|Country.clean),
  data = Survey_1_long_indices)

S1_AG.RD_lmer <- lmer(MeanScore.allSteps ~ log10(AgResearchAndDev_PPP2017) + (1|Country.clean),
                   data = Survey_1_long_indices)

S1_EP_lmer <- lmer(MeanScore.allSteps ~ EnviroPerformance_score + (1|Country.clean),
                data = Survey_1_long_indices)



# ==============================================================================
# STEP 1: Data checks — spread, distributions, country-covariate consistency
# ==============================================================================
covariate_vars <- c("GDP_per_capita", "AllResearchAndDev_percentGPD",
                    "AgForestryFish_ValueAdded_percentGDP",
                    "AgResearchAndDev_PPP2017", "EnviroPerformance_score",
                    "Exact_latitude")

# Summary stats (national covariates de-duplicated by country; latitude is respondent-level)
national_vars <- setdiff(covariate_vars, "Exact_latitude")

Survey_1_long_indices %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  pivot_longer(cols = all_of(national_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(n = sum(!is.na(value)), min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE), median = median(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            ratio_max_min = max / min, skewness = (mean - median) / sd, .groups = "drop") %>%
  arrange(desc(abs(skewness)))

# Histograms
Survey_1_long_indices %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  pivot_longer(cols = all_of(national_vars), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "#2166AC", colour = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Confirm national covariates are constant within country (should return 0 rows)
Survey_1_long_indices %>%
  filter(!is.na(Country.clean)) %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  count(Country.clean) %>%
  filter(n > 1)


# ==============================================================================
# STEP 2: Correlation matrix — check collinearity BEFORE combining covariates
# ==============================================================================
cor_matrix <- Survey_1_long_indices %>%
  select(Country.clean, all_of(national_vars)) %>%
  distinct() %>%
  mutate(log10_GDP = log10(GDP_per_capita),
         log10_AllRD = log10(AllResearchAndDev_percentGPD),
         log10_AgGDP = log10(AgForestryFish_ValueAdded_percentGDP),
         log10_AgRD = log10(AgResearchAndDev_PPP2017)) %>%
  select(EnviroPerformance_score, log10_GDP, log10_AllRD, log10_AgGDP, log10_AgRD) %>%
  cor(use = "complete.obs")

round(cor_matrix, 2)

# Visualise it
corrplot::corrplot(cor_matrix, method = "number", type = "upper", diag = FALSE)
corrplot::corrplot(cor_matrix, method = "ellipse", type = "upper", diag = FALSE)


# ==============================================================================
# STEP 3: Survey 1 — Knowledge pathway score
# ==============================================================================
covariate_vars_crop <- c(covariate_vars, "Crop.type.clean")

Survey_1_complete <- Survey_1_long_indices %>%
  filter(if_all(all_of(covariate_vars), ~ !is.na(.)))

Survey_1_complete_crop <- Survey_1_long_indices %>%
  filter(if_all(all_of(covariate_vars_crop), ~ !is.na(.)))

# --- A. Null and single-covariate models (on Survey_1_complete) ---
S1_null_glmmTMB <- glmmTMB(MeanScore.allSteps ~ 1 + (1|Country.clean),
                           family = gaussian(), data = Survey_1_complete)

S1_Lat_glmmTMB <- glmmTMB(MeanScore.allSteps ~ abs(Exact_latitude) + (1|Country.clean),
                          family = gaussian(), data = Survey_1_complete)

S1_GDP_glmmTMB <- glmmTMB(MeanScore.allSteps ~ log10(GDP_per_capita) + abs(Exact_latitude) +
                            (1|Country.clean),
                          family = gaussian(), data = Survey_1_complete)

S1_RD_all_glmmTMB <- glmmTMB(MeanScore.allSteps ~ log10(AllResearchAndDev_percentGPD) + abs(Exact_latitude) +
                               (1|Country.clean),
                             family = gaussian(), data = Survey_1_complete)

S1_Ag_GDP_glmmTMB <- glmmTMB(MeanScore.allSteps ~ log10(AgForestryFish_ValueAdded_percentGDP) + abs(Exact_latitude) +
                               (1|Country.clean),
                             family = gaussian(), data = Survey_1_complete)

S1_Ag_RD_glmmTMB <- glmmTMB(MeanScore.allSteps ~ log10(AgResearchAndDev_PPP2017) + abs(Exact_latitude) +
                              (1|Country.clean),
                            family = gaussian(), data = Survey_1_complete)

S1_EP_glmmTMB <- glmmTMB(MeanScore.allSteps ~ EnviroPerformance_score + abs(Exact_latitude) +
                           (1|Country.clean),
                         family = gaussian(), data = Survey_1_complete) # This is the best one so far

# --- B. Non-collinear paired combinations (cor < 0.6 per correlation matrix) ---
# EP vs AgRD: cor = -0.18 (safe)  , GDP vs AgRD: cor = 0.05 (safe)
# NOT combining: EP+GDP (0.83), EP+AgGDP (-0.81), GDP+AgGDP (-0.82), GDP+AllRD (0.70)
S1_EP_AgRD_glmmTMB <- glmmTMB(MeanScore.allSteps ~ EnviroPerformance_score +
                                log10(AgResearchAndDev_PPP2017) + abs(Exact_latitude) +
                                (1|Country.clean),
                              family = gaussian(), data = Survey_1_complete)

S1_GDP_AgRD_glmmTMB <- glmmTMB(MeanScore.allSteps ~ log10(GDP_per_capita) +
                                 log10(AgResearchAndDev_PPP2017) + abs(Exact_latitude) +
                                 (1|Country.clean),
                               family = gaussian(), data = Survey_1_complete)

# --- C. Crop type variations (on Survey_1_complete_crop) ---
S1_null_crop_glmmTMB <- glmmTMB(MeanScore.allSteps ~ 1 + (1|Country.clean),
                                family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + (1|Country.clean),
                           family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_Lat_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + abs(Exact_latitude) +
                                 (1|Country.clean),
                               family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_GDP_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + log10(GDP_per_capita) +
                                 abs(Exact_latitude) + (1|Country.clean),
                               family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_RD_all_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + log10(AllResearchAndDev_percentGPD) +
                                    abs(Exact_latitude) + (1|Country.clean),
                                  family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_Ag_GDP_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + log10(AgForestryFish_ValueAdded_percentGDP) +
                                    abs(Exact_latitude) + (1|Country.clean),
                                  family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_Ag_RD_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + log10(AgResearchAndDev_PPP2017) +
                                   abs(Exact_latitude) + (1|Country.clean),
                                 family = gaussian(), data = Survey_1_complete_crop)

S1_Crop_EP_glmmTMB <- glmmTMB(MeanScore.allSteps ~ Crop.type.clean + EnviroPerformance_score +
                                abs(Exact_latitude) + (1|Country.clean),
                              family = gaussian(), data = Survey_1_complete_crop)

# --- Full AIC comparison table (Survey 1) ---
S1_AIC_full <- AIC(S1_null_glmmTMB, S1_Lat_glmmTMB, S1_GDP_glmmTMB, S1_RD_all_glmmTMB,
                   S1_Ag_GDP_glmmTMB, S1_Ag_RD_glmmTMB, S1_EP_glmmTMB,
                   S1_EP_AgRD_glmmTMB, S1_GDP_AgRD_glmmTMB) %>%
  rownames_to_column("model") %>%
  arrange(AIC) %>%
  mutate(delta_AIC = AIC - min(AIC))
S1_AIC_full # best model in this set = S1_EP_glmmTMB, AIC is 490

S1_AIC_crop_full <- AIC(S1_null_crop_glmmTMB, S1_Crop_glmmTMB, S1_Crop_Lat_glmmTMB,
                        S1_Crop_GDP_glmmTMB, S1_Crop_RD_all_glmmTMB, S1_Crop_Ag_GDP_glmmTMB,
                        S1_Crop_Ag_RD_glmmTMB, S1_Crop_EP_glmmTMB) %>%
  rownames_to_column("model") %>%
  arrange(AIC) %>%
  mutate(delta_AIC = AIC - min(AIC))
S1_AIC_crop_full # Then when we add crop type, this is the best = S1_Crop_EP_glmmTMB AIC is 461

# ==============================================================================
# STEP 4: Get summary and visualise best model for Survey 1 - Knowledge scores 
# ==============================================================================
summary(S1_Crop_EP_glmmTMB)
# Interpretation:
#   -   despite crop type improving AIC by a lot when we added it over the null, not a single crop category is significant
#   -   AIC can improve from the joint contribution of the whole set of crop types even without single crops being sig
# Lets do a joint test of whether Crop.type.clean matters at all
#   -   Car:Anova() gives one p-value per term instead of one per crop type with summary
car::Anova(S1_Crop_EP_glmmTMB, type = "III")

visreg(S1_Crop_EP_glmmTMB,
       "EnviroPerformance_score",
       scale = "response",
       gg = TRUE, rug = TRUE)

visreg(S1_Crop_EP_glmmTMB,
       "Exact_latitude",
       scale = "response",
       gg = TRUE, rug = TRUE)

visreg(S1_Crop_EP_glmmTMB,
       "Crop.type.clean",
       scale = "response",
       gg = TRUE)

# Check diagnostics
simulateResiduals(S1_Crop_EP_glmmTMB, plot = TRUE) # residuals arent great...
performance::check_model(S1_Crop_EP_glmmTMB)


# ==============================================================================
# STEP 5: Survey 2 — Intervention score — FULL MODEL SET (same structure)
# ==============================================================================
Survey_2_complete <- Survey_2_long_indices %>%
  filter(if_all(all_of(covariate_vars), ~ !is.na(.)))

Survey_2_complete_crop <- Survey_2_long_indices %>%
  filter(if_all(all_of(covariate_vars_crop), ~ !is.na(.)))

# --- A. Null and single-covariate models ---
S2_null_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ 1 + (1|Country.clean),
                           family = gaussian(), data = Survey_2_complete)

S2_Lat_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ abs(Exact_latitude) + (1|Country.clean),
                          family = gaussian(), data = Survey_2_complete)

S2_GDP_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ log10(GDP_per_capita) + abs(Exact_latitude) +
                            (1|Country.clean),
                          family = gaussian(), data = Survey_2_complete)

S2_RD_all_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ log10(AllResearchAndDev_percentGPD) + abs(Exact_latitude) +
                               (1|Country.clean),
                             family = gaussian(), data = Survey_2_complete)

S2_Ag_GDP_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ log10(AgForestryFish_ValueAdded_percentGDP) + abs(Exact_latitude) +
                               (1|Country.clean),
                             family = gaussian(), data = Survey_2_complete)

S2_Ag_RD_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ log10(AgResearchAndDev_PPP2017) + abs(Exact_latitude) +
                              (1|Country.clean),
                            family = gaussian(), data = Survey_2_complete)

S2_EP_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ EnviroPerformance_score + abs(Exact_latitude) +
                           (1|Country.clean),
                         family = gaussian(), data = Survey_2_complete)

# --- B. Non-collinear paired combinations ---
S2_EP_AgRD_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ EnviroPerformance_score +
                                log10(AgResearchAndDev_PPP2017) + abs(Exact_latitude) +
                                (1|Country.clean),
                              family = gaussian(), data = Survey_2_complete)

S2_GDP_AgRD_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ log10(GDP_per_capita) +
                                 log10(AgResearchAndDev_PPP2017) + abs(Exact_latitude) +
                                 (1|Country.clean),
                               family = gaussian(), data = Survey_2_complete)

# --- C. Crop type variations ---
S2_null_crop_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ 1 + (1|Country.clean),
                                family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + (1|Country.clean),
                           family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_Lat_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + abs(Exact_latitude) +
                                 (1|Country.clean),
                               family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_GDP_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + log10(GDP_per_capita) +
                                 abs(Exact_latitude) + (1|Country.clean),
                               family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_RD_all_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + log10(AllResearchAndDev_percentGPD) +
                                    abs(Exact_latitude) + (1|Country.clean),
                                  family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_Ag_GDP_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + log10(AgForestryFish_ValueAdded_percentGDP) +
                                    abs(Exact_latitude) + (1|Country.clean),
                                  family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_Ag_RD_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + log10(AgResearchAndDev_PPP2017) +
                                   abs(Exact_latitude) + (1|Country.clean),
                                 family = gaussian(), data = Survey_2_complete_crop)

S2_Crop_EP_glmmTMB <- glmmTMB(MeanScore.allInterventions ~ Crop.type.clean + EnviroPerformance_score +
                                abs(Exact_latitude) + (1|Country.clean),
                              family = gaussian(), data = Survey_2_complete_crop)

# --- Full AIC comparison table (Survey 2) ---
S2_AIC_full <- AIC(S2_null_glmmTMB, S2_Lat_glmmTMB, S2_GDP_glmmTMB, S2_RD_all_glmmTMB,
                   S2_Ag_GDP_glmmTMB, S2_Ag_RD_glmmTMB, S2_EP_glmmTMB,
                   S2_EP_AgRD_glmmTMB, S2_GDP_AgRD_glmmTMB) %>%
  rownames_to_column("model") %>%
  arrange(AIC) %>%
  mutate(delta_AIC = AIC - min(AIC))
S2_AIC_full # This is the best model of those without crop type = S2_EP_glmmTMB, AIC = 199

S2_AIC_crop_full <- AIC(S2_null_crop_glmmTMB, S2_Crop_glmmTMB, S2_Crop_Lat_glmmTMB,
                        S2_Crop_GDP_glmmTMB, S2_Crop_RD_all_glmmTMB, S2_Crop_Ag_GDP_glmmTMB,
                        S2_Crop_Ag_RD_glmmTMB, S2_Crop_EP_glmmTMB) %>%
  rownames_to_column("model") %>%
  arrange(AIC) %>%
  mutate(delta_AIC = AIC - min(AIC))
S2_AIC_crop_full # This is the best model with crop type included for survey 2 = S2_Crop_EP_glmmTMB, AIC = 180


# ==============================================================================
# STEP 5: Category size check for crop models (before trusting them)
# ==============================================================================
n_distinct(Survey_1_complete_crop$Crop.type.clean)
Survey_1_complete_crop %>% count(Crop.type.clean) %>% arrange(n)

n_distinct(Survey_2_complete_crop$Crop.type.clean)
Survey_2_complete_crop %>% count(Crop.type.clean) %>% arrange(n)




# ==============================================================================
# STEP 6: Get summary and visualise best model for Survey 2 - Intervention scores 
# ==============================================================================
summary(S2_Crop_EP_glmmTMB)
# Interpretation:
#   -   each crop type seems to be having a significant effect on the mean score of intervention testing
car::Anova(S2_Crop_EP_glmmTMB, type = "III") # and an overall effect

visreg(S2_Crop_EP_glmmTMB,
       "EnviroPerformance_score",
       scale = "response",
       gg = TRUE, rug = TRUE)

visreg(S2_Crop_EP_glmmTMB,
       "Exact_latitude",
       scale = "response",
       gg = TRUE, rug = TRUE)

visreg(S2_Crop_EP_glmmTMB,
       "Crop.type.clean",
       scale = "response",
       gg = TRUE)

# Check diagnostics
simulateResiduals(S2_Crop_EP_glmmTMB, plot = TRUE) # residuals arent great... QQ ok, but quantile deviations detected
performance::check_model(S2_Crop_EP_glmmTMB)


# ==============================================================================
# 7. VISUALISE RAW DATA AND MODEL PREDICTIONS
# ==============================================================================

score_caption_S1 <- "Knowledge pathway scores = mean across all steps in our conceptual knowledge pathway, scored from 0 to 3 at each step.\nThe best GLMM structure so far is:\nMeanScore.allSteps ~ Crop.type.clean + EnviroPerformance_score + abs(Exact_latitude) + (1 | Country.clean)"
score_caption_S2 <- "Intervention score = mean across all interventions, scored 0 (not tried) to 3 (tried and tested).\nThe best GLMM structure so far is:\nMeanScore.allInterventions ~ Crop.type.clean + EnviroPerformance_score + abs(Exact_latitude) + (1 | Country.clean)"

#---------------------------------
# A) EnviroPerformance_score — predicted effect + raw data, separate plots
#---------------------------------
S1_EP_pred <- ggpredict(S1_Crop_EP_glmmTMB, terms = "EnviroPerformance_score [all]")
S2_EP_pred <- ggpredict(S2_Crop_EP_glmmTMB, terms = "EnviroPerformance_score [all]")

S1_raw <- Survey_1_complete_crop %>%
  select(x = EnviroPerformance_score, predicted = MeanScore.allSteps)
S2_raw <- Survey_2_complete_crop %>%
  select(x = EnviroPerformance_score, predicted = MeanScore.allInterventions)

ggplot(as.data.frame(S1_EP_pred), aes(x = x, y = predicted)) +
  geom_point(data = S1_raw, aes(x = x, y = predicted),
             alpha = 0.25, size = 1.5, colour = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  labs(x = "Environmental Performance score",
       y = "Predicted Knowledge Pathway score (± 95% CI)",
       title = "Model-predicted effect of Environmental Performance on Knowledge Pathway Scores (Survey 1)",
       caption = score_caption_S1) +
  theme_minimal()

ggplot(as.data.frame(S2_EP_pred), aes(x = x, y = predicted)) +
  geom_point(data = S2_raw, aes(x = x, y = predicted),
             alpha = 0.25, size = 1.5, colour = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  labs(x = "Environmental Performance score",
       y = "Predicted Intervention score (± 95% CI)",
       title = "Model-predicted effect of Environmental Performance on Intervention Scores (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal()


#---------------------------------
# B) abs(Exact_latitude) — predicted effect + raw data, separate plots
#---------------------------------
S1_Lat_pred <- ggpredict(S1_Crop_EP_glmmTMB, terms = "Exact_latitude [all]")
S2_Lat_pred <- ggpredict(S2_Crop_EP_glmmTMB, terms = "Exact_latitude [all]")

S1_raw_lat <- Survey_1_complete_crop %>%
  select(x = Exact_latitude, predicted = MeanScore.allSteps) %>%
  mutate(x = abs(x))
S2_raw_lat <- Survey_2_complete_crop %>%
  select(x = Exact_latitude, predicted = MeanScore.allInterventions) %>%
  mutate(x = abs(x))

ggplot(as.data.frame(S1_Lat_pred), aes(x = abs(x), y = predicted)) +
  geom_point(data = S1_raw_lat, aes(x = x, y = predicted),
             alpha = 0.25, size = 1.5, colour = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2166AC") +
  geom_line(colour = "#2166AC", linewidth = 1) +
  labs(x = "Absolute latitude (°)",
       y = "Predicted Knowledge Pathway score (± 95% CI)",
       title = "Model-predicted effect of Latitude on Knowledge Pathway score (Survey 1)",
       caption = score_caption_S1) +
  theme_minimal()

ggplot(as.data.frame(S2_Lat_pred), aes(x = abs(x), y = predicted)) +
  geom_point(data = S2_raw_lat, aes(x = x, y = predicted),
             alpha = 0.25, size = 1.5, colour = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2166AC") +
  geom_line(colour = "#2166AC", linewidth = 1) +
  labs(x = "Absolute latitude (°)",
       y = "Predicted Intervention score (± 95% CI)",
       title = "Model-predicted effect of Latitude on Intervention Score (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal()

# ==============================================================================
# E) Pairwise crop-type comparisons with significance brackets, separate plots
# ==============================================================================
S1_Crop_pairs <- emmeans(S1_Crop_EP_glmmTMB, pairwise ~ Crop.type.clean, adjust = "tukey")
S2_Crop_pairs <- emmeans(S2_Crop_EP_glmmTMB, pairwise ~ Crop.type.clean, adjust = "tukey")

S1_sig_pairs <- as.data.frame(S1_Crop_pairs$contrasts) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

S2_sig_pairs <- as.data.frame(S2_Crop_pairs$contrasts) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

S1_sig_pairs
S2_sig_pairs

# Helper: build bracket annotation data from a sig_pairs table + emmeans data
build_brackets <- function(sig_pairs, emm_data, y_start_offset = 0.15, y_step = 0.12) {
  sig_pairs <- sig_pairs %>%
    separate(contrast, into = c("group1", "group2"), sep = " - ", remove = FALSE) %>%
    mutate(group1 = str_trim(group1), group2 = str_trim(group2))
  
  crop_order <- levels(fct_reorder(emm_data$Crop.type.clean, emm_data$emmean))
  
  sig_pairs %>%
    mutate(
      x1 = match(group1, crop_order),
      x2 = match(group2, crop_order),
      y = max(emm_data$asymp.UCL) + y_start_offset + (row_number() - 1) * y_step,
      label = case_when(
        p.value < 0.001 ~ "p < 0.001",
        TRUE ~ paste0("p = ", round(p.value, 3))
      )
    )
}

S1_emm_df <- as.data.frame(S1_Crop_pairs$emmeans)
S2_emm_df <- as.data.frame(S2_Crop_pairs$emmeans)

S1_brackets <- build_brackets(S1_sig_pairs, S1_emm_df)
S2_brackets <- build_brackets(S2_sig_pairs, S2_emm_df)

# Survey 1
ggplot(S1_Crop_emm,
       aes(x = fct_reorder(Crop.type.clean, emmean),
           y = emmean,
           colour = Crop.type.clean)) +
  
  # Raw data
  geom_jitter(data = S1_raw_crop,
              aes(x = Crop.type.clean, y = y, colour = Crop.type.clean),
              width = 0.20, height = 0.05,
              alpha = 0.35, size = 1.2) +
  
  # 95% CI
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.18, colour = "grey40") +
  
  # EMMs
  geom_point(size = 4, shape = 17) +
  
  # Significance brackets
  {
    if (nrow(S1_brackets) > 0)
      list(
        # horizontal bar
        geom_segment(data = S1_brackets,
                     aes(x = x1, xend = x2, y = y, yend = y),
                     inherit.aes = FALSE,
                     colour = "grey20"),
        
        # left tick
        geom_segment(data = S1_brackets,
                     aes(x = x1, xend = x1,
                         y = y, yend = y - 0.03),
                     inherit.aes = FALSE,
                     colour = "grey20"),
        
        # right tick
        geom_segment(data = S1_brackets,
                     aes(x = x2, xend = x2,
                         y = y, yend = y - 0.03),
                     inherit.aes = FALSE,
                     colour = "grey20"),
        
        geom_text(data = S1_brackets,
                  aes(x = (x1 + x2)/2,
                      y = y + 0.03,
                      label = label),
                  inherit.aes = FALSE,
                  size = 3,
                  angle = 270)
      )
  } +
  
  scale_colour_manual(values = crop_colours) +
  
  coord_flip() +
  
  labs(
    x = NULL,
    y = "Model-predicted mean Knowledge Pathway score (±95% CI) (EMM)",
    title = "Model-predicted effect of crop type on Knowledge Pathway Scores (Survey 1)",
    caption = score_caption_S1
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


# Survey 2
ggplot(S2_Crop_emm,
       aes(x = fct_reorder(Crop.type.clean, emmean),
           y = emmean,
           colour = Crop.type.clean)) +
  
  geom_jitter(data = S2_raw_crop,
              aes(x = Crop.type.clean, y = y, colour = Crop.type.clean),
              width = 0.20, height = 0.05,
              alpha = 0.35, size = 1.5) +
  
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.18, colour = "grey40") +
  
  geom_point(size = 4, shape = 17) +
  
  {
    if (nrow(S2_brackets) > 0)
      list(
        geom_segment(data = S2_brackets,
                     aes(x = x1, xend = x2, y = y, yend = y),
                     inherit.aes = FALSE,
                     colour = "grey20"),
        
        geom_segment(data = S2_brackets,
                     aes(x = x1, xend = x1,
                         y = y, yend = y - 0.03),
                     inherit.aes = FALSE,
                     colour = "grey20"),
        
        geom_segment(data = S2_brackets,
                     aes(x = x2, xend = x2,
                         y = y, yend = y - 0.03),
                     inherit.aes = FALSE,
                     colour = "grey20"),
        
        geom_text(data = S2_brackets,
                  aes(x = (x1 + x2)/2,
                      y = y + 0.03,
                      label = label),
                  inherit.aes = FALSE,
                  size = 3,
                  angle = 270)
      )
  } +
  
  scale_colour_manual(values = crop_colours) +
  
  coord_flip() +
  
  labs(
    x = NULL,
    y = "Model-predicted mean Intervention score (±95% CI) (EMM)",
    title = "Model-predicted effect of crop type on Intervention Scores (Survey 2)",
    caption = score_caption_S2
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
