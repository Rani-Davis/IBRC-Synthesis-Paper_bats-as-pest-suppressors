# Code author: Rani Davis
# Last updated: 18 Sep 2026
#

# ----------------------------------------
# Packages
# ----------------------------------------

# !! Before running, ensure your DHARMa package is version 0.5.0

library(lme4);library(patchwork);library(visreg);library(glmmTMB)
library(DHARMa);library(ggeffects);library(emmeans);library(tidyr)
library(dplyr);library(ggplot2);library(tibble);library(tidyverse)
library(broom.mixed);library(ggrepel);library(readr);library(lmerTest)

# data 
#Survey_2_long_indices_wLatLong_wRichness <- read_csv("Analysis/clean data/Survey 2_scored_with indices_w Lat Long_w Richness.csv") %>% mutate(RowID = row_number())
names(Survey_2_long_indices_wLatLong_wRichness)

# ==============================================================================
# STEP 1: Collapse long-format data to one row per respondent entry (de-duplicate before modelling)
# ==============================================================================
# REPEAT FOR SURVEY 2 DAtaset:
respondent_level_vars2 <- c(
   "Respondent.clean", "Respondent.ID", "Respondent.entry.ID", "Respondent.entry.label",
   "Region.within.country.clean", "Country.clean", "World.region.clean",
   "Crop.clean", "Crop.type.clean",
   "TotalScore.allInterventions", "MeanScore.allInterventions",
   "Country.WB", "GDP_per_capita", "AgForestryFish_ValueAdded_percentGDP",
   #"AllResearchAndDev_percentGPD", <- no longer included in model selection as there is no data availavel for some countries
   "EnviroPerformance_score", "AgResearchAndDev_PPP2017",
   "Taiwan_imputed_from_China", "Exact_latitude", "Exact_longitude",
   "Coarse_latitude", "Coarse_longitude", "BatRichness"
 )
# Should return 0 rows now that R3 is fixed
Survey_2_long_indices_wLatLong_wRichness %>%
   group_by(Respondent.entry.label) %>%
   summarise(across(all_of(setdiff(respondent_level_vars2, c("Respondent.entry.label", "Respondent.entry.ID"))),
                    ~ n_distinct(.)), .groups = "drop") %>%
   pivot_longer(-Respondent.entry.label, names_to = "variable", values_to = "n_distinct") %>%
   filter(n_distinct > 1)

# Now collapse to one row per repondent entry
 Survey_2_respondent <- Survey_2_long_indices_wLatLong_wRichness %>%
   select(all_of(respondent_level_vars2)) %>%
   distinct(Respondent.entry.label, .keep_all = TRUE)

 nrow(Survey_2_respondent) == n_distinct(Survey_2_long_indices_wLatLong_wRichness$Respondent.entry.label)  # should be TRUE


 
# ==============================================================================
# Step 2) SURVEY 2 — MEAN INTERVENTION SCORE -
# ==============================================================================
# The mean intervention score (across all interventions) is modelled first to identify whicH national factors 
 # are associated with respondents' overall score for testing of bat-supportive interventions
# We then model each intervention separately to determine whether patterns in
 # the overall score are consistent across intervention types, or whether particular 
 # interventions are driving the overall result.
# ==============================================================================
 # Ensure all models run on the same data:
 # Check which variables we set as covariate_vars, then filter out NA's----------
 covariate_vars
 
 Survey_2_complete <- Survey_2_respondent %>%             
   filter(if_all(all_of(covariate_vars), ~ !is.na(.)))


# Rescale the 0–3 mean score to 0–1 for beta regression
 Survey_2_complete<-  Survey_2_complete%>%
  mutate(
    MeanScore.allInterventions.prop = MeanScore.allInterventions / 3,
    n_obs = n(),
    Survey_2_mean_rescaled_beta =
      (MeanScore.allInterventions.prop * (n_obs - 1) + 0.5) / n_obs
  )

# Check whether respondents are repeated in Survey 2
 Survey_2_complete%>%
  count(Respondent.clean) %>%
  filter(n > 1) %>%
  nrow()

# ------------------------------------------------------------------------------
# STEP 2) Candidate models for - Mean INTERVENTION SCORE across all interventions -
# ------------------------------------------------------------------------------
# Survey 2 models test predictors of the mean intervention-support score.
# All models include Country.clean and Respondent.clean as random intercepts to account for non-independence among respondents within countries and repeated observations from respondents.
#
# The candidate model structure follows the Survey 1 analysis:
#   2A. Null and single-covariate models
#   2B. BatRichness alone and paired with each single covariate
#   2C. Crop.type.clean alone and paired with each single covariate
#   2D. Non-collinear socio-economic pairs
#   2E. Non-collinear pairs with BatRichness added
#   2F. Non-collinear pairs with Crop.type.clean added
#   2G. Non-collinear pairs with both BatRichness and Crop.type.clean added

# --- 2A. Null and single-covariate models (without BatRichness/CropType) ----
# These models test the association between Survey 2 scores and each
# country-level predictor individually.
S2_null_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ 1
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Lat_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ abs(Exact_latitude)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Ag_GDP_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Ag_RD_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_EP_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2B. BatRichness alone, and paired with each single covariate -----------
# NOTE: run the extended collinearity check (BatRichness vs each socio-economic index/latitude) before trusting these paired models.
# Drop any pair with |cor| > 0.6.

S2_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Lat_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ abs(Exact_latitude) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Ag_GDP_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(AgForestryFish_ValueAdded_percentGDP) +
    log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Ag_RD_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(AgResearchAndDev_PPP2017) +
    log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_EP_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2C. Crop.type.clean alone, and paired with each single covariate ------
# These models test whether Survey 2 scores differ among crop types, both alone
# and after accounting for each individual country-level predictor.
#
# NOTE: Crop.type.clean = 6 df (entry-level, not country-level), 
# so pairing it with a country-level covariate spends 7 fixed-effect parameters. 
# These models  should therefore be treated as exploratory
S2_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Lat_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ abs(Exact_latitude) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Ag_GDP_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~
    log10(AgForestryFish_ValueAdded_percentGDP) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_Ag_RD_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~
    log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_EP_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(BatRichness) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2D. Non-collinear socio-economic pairs (without BatRichness/CropType) --
# These models combine pairs of country-level predictors that are sufficiently weakly correlated to be included together (|cor| < 0.6).
#
# Candidate pairs:
#   - Environmental Performance + total R&D
#   - Environmental Performance + agricultural R&D
#   - GDP per capita + agricultural R&D
S2_EP_AgRD_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score +
    log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_AgRD_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita) +
    log10(AgResearchAndDev_PPP2017)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2E. Same non-collinear pairs, WITH BatRichness added (3-predictor models) ----
# These models extend the non-collinear two-predictor models by adding log10(BatRichness).
#
# NOTE: with only ~24 country clusters, a 3-fixed-effect model may exceed the
# original predictor budget. Treat these as exploratory and check model
# convergence and parameter stability carefully.
S2_EP_AgRD_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score +
    log10(AgResearchAndDev_PPP2017) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_AgRD_BatRichness_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita) +
    log10(AgResearchAndDev_PPP2017) + log10(BatRichness)
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2F. Same non-collinear pairs, WITH Crop.type.clean added ---------------
# These models add Crop.type.clean to the non-collinear country-level
# predictor pairs.
#
# NOTE: Crop.type.clean = 6 df, so these models use approximately 8-9 fixed-effect parameters. 
# With ~24 country clusters, they are relatively complex and should be treated as exploratory.
# Check convergence and parameter stability carefully.
S2_EP_AgRD_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score +
    log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_AgRD_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita) +
    log10(AgResearchAndDev_PPP2017) + Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2G. Non-collinear pairs, WITH BatRichness AND Crop.type.clean -----------
# These models include both BatRichness and Crop.type.clean alongside the
# two non-collinear country-level predictors.
#
# NOTE: these models contain approximately 9-10 fixed-effect parameters once Crop.type.clean is expanded to its 6 df factor. 
# they are likely to be over-specified. 
# They are included for completeness of the candidate set but should be considered exploratory
S2_EP_AgRD_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ EnviroPerformance_score +
    log10(AgResearchAndDev_PPP2017) + log10(BatRichness) +
    Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)

S2_GDP_AgRD_BatRichness_CropType_glmmTMB <- glmmTMB(
  Survey_2_mean_rescaled_beta ~ log10(GDP_per_capita) +
    log10(AgResearchAndDev_PPP2017) + log10(BatRichness) +
    Crop.type.clean
  + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(), data =  Survey_2_complete)


# --- 2H. Full AICc comparison table ------------------------------------------
# Compare all candidate models using AICc. 
# Models with BatRichness and/or Crop.type.clean can then be assessed alongside the corresponding simpler models to determine whether these additional predictors improve model fit sufficiently to justify their additional complexity.
S2_candidates <- list(
  null                              = S2_null_glmmTMB,
  Lat                               = S2_Lat_glmmTMB,
  GDP                               = S2_GDP_glmmTMB,
  Ag_GDP                            = S2_Ag_GDP_glmmTMB,
  Ag_RD                             = S2_Ag_RD_glmmTMB,
  EP                                = S2_EP_glmmTMB,
  BatRichness                       = S2_BatRichness_glmmTMB,
  Lat_BatRichness                   = S2_Lat_BatRichness_glmmTMB,
  GDP_BatRichness                   = S2_GDP_BatRichness_glmmTMB,
  Ag_GDP_BatRichness                = S2_Ag_GDP_BatRichness_glmmTMB,
  Ag_RD_BatRichness                 = S2_Ag_RD_BatRichness_glmmTMB,
  EP_BatRichness                    = S2_EP_BatRichness_glmmTMB,
  CropType                          = S2_CropType_glmmTMB,
  Lat_CropType                      = S2_Lat_CropType_glmmTMB,
  GDP_CropType                      = S2_GDP_CropType_glmmTMB,
  Ag_GDP_CropType                   = S2_Ag_GDP_CropType_glmmTMB,
  Ag_RD_CropType                    = S2_Ag_RD_CropType_glmmTMB,
  EP_CropType                       = S2_EP_CropType_glmmTMB,
  BatRichness_CropType              = S2_BatRichness_CropType_glmmTMB,
  
  EP_AgRD                           = S2_EP_AgRD_glmmTMB,
  GDP_AgRD                          = S2_GDP_AgRD_glmmTMB,
  
  EP_AgRD_BatRichness               = S2_EP_AgRD_BatRichness_glmmTMB,
  GDP_AgRD_BatRichness              = S2_GDP_AgRD_BatRichness_glmmTMB,
  
  EP_AgRD_CropType                  = S2_EP_AgRD_CropType_glmmTMB,
  GDP_AgRD_CropType                 = S2_GDP_AgRD_CropType_glmmTMB,
  
  EP_AgRD_BatRichness_CropType      = S2_EP_AgRD_BatRichness_CropType_glmmTMB,
  GDP_AgRD_BatRichness_CropType     = S2_GDP_AgRD_BatRichness_CropType_glmmTMB
)

S2_AICc_full <- purrr::map_dfr(
  S2_candidates,
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

S2_AICc_full
write.xlsx( S2_AICc_full, "Analysis/model selection/Survey 2_model comparison table.xlsx")



# ==============================================================================
# STEP 3) DIAGNOSTICS FOR THE BEST OVERALL SURVEY 2 MODEL 
# ==============================================================================
S2_EP_glmmTMB

# This is our best model
formula(S2_EP_glmmTMB)

## But we need to refit it on the full dataset:---------------------------
# Rescale full dataseT:
Survey_2_respondent_rescaled_beta <- Survey_2_respondent %>%
  mutate(
    MeanScore.allInterventions.prop = MeanScore.allInterventions / 3,
    n_obs = n(),
    Survey_2_mean_rescaled_beta =
      (MeanScore.allInterventions.prop * (n_obs - 1) + 0.5) / n_obs
  )

## Re-run model:
S2_EP_glmmTMB_fulldata <- glmmTMB(
  Survey_2_mean_rescaled_beta ~
    EnviroPerformance_score +
    (1 | Country.clean) +
    (1 | Respondent.clean),
  family = beta_family(),
  data = Survey_2_respondent_rescaled_beta
)

summary(S2_EP_glmmTMB_fulldata)

## ---- Stage 1: Simulate residuals ----
set.seed(42)
simres_S2_EP_glmmTMB <- simulateResiduals(S2_EP_glmmTMB_fulldata, plot = FALSE)

## ---- Stage 2: Global fit checks ----
plot(simres_S2_EP_glmmTMB)

par(mfrow = c(2, 1))
testDispersion(simres_S2_EP_glmmTMB)  # over/underdispersion -> n.s.
testOutliers(simres_S2_EP_glmmTMB)    # excess extreme residuals -> 1/58 flagged, n.s.
# Result: uniformity, dispersion, and outlier tests all n.s. 
# No zero-inflation test needed as the response is a continuous proportion bounded strictly within (0,1) via rescaling, so "zero-inflation" (a count-data concept) isn't applicable

## ---- Stage 3: Residuals by predictor and grouping factor ----
# glmmTMB drops incomplete cases (NAs) when fitting, so DHARMa's residual vector
# is shorter than the full dataset — build the matching complete-case dataframe:
S2_EP_model_data_noNAs <- Survey_2_respondent_rescaled_beta %>%
  filter(!is.na(EnviroPerformance_score),
         !is.na(Survey_2_mean_rescaled_beta),
         !is.na(Country.clean),
         !is.na(Respondent.clean))

par(mfrow = c(1, 1))
plotResiduals(simres_S2_EP_glmmTMB, form = S2_EP_model_data_noNAs$EnviroPerformance_score)

testCategorical(simres_S2_EP_glmmTMB, catPred = S2_EP_model_data_noNAs$Country.clean)

leveneTest(simres_S2_EP_glmmTMB$scaledResiduals ~ S2_EP_model_data_noNAs$Country.clean)
fligner.test(simres_S2_EP_glmmTMB$scaledResiduals ~ S2_EP_model_data_noNAs$Country.clean)
# all ns

## ---- Stage 4: Investigate apparent nonlinearity ----
p_raw_EP_S2 <- ggplot(Survey_2_complete, aes(x = EnviroPerformance_score, y = Survey_2_mean_rescaled_beta,
                                          text = paste("Country:", Country.clean,
                                                       "<br>Respondent:", Respondent.clean,
                                                       "<br>EnviroPerformance:", round(EnviroPerformance_score, 2),
                                                       "<br>Score:", round(Survey_2_mean_rescaled_beta, 3)))) +
  geom_point(color = "black", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1) +
  labs(x = "EnviroPerformance_score", y = "Rescaled score", title = "Raw relationship") +
  theme_minimal()

ggplotly(p_raw_EP_S2, tooltip = "text")

Survey_2_complete %>% filter(Country.clean == "Israel") %>% pull(EnviroPerformance_score) %>% table()
# -> All 21 Israeli respondents share EnviroPerformance_score = 51.69 (expected, since it's a country-level covariate)
# but 21/75 respondents (28%) at one x-value gives that point strong leverage over the loess curve.

ggplot(Survey_2_complete, aes(x = EnviroPerformance_score, y = Survey_2_mean_rescaled_beta)) +
  geom_point(aes(color = Country.clean == "Israel"), alpha = 0.7) +
  scale_color_manual(values = c("grey60", "red"), labels = c("Other", "Israel")) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(color = "") +
  theme_minimal()

Survey_2_no_Israel <- Survey_2_complete %>% filter(Country.clean != "Israel")

ggplot(Survey_2_no_Israel, aes(x = EnviroPerformance_score, y = Survey_2_mean_rescaled_beta)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal()
# -> Didn't really change it



# ==============================================================================
# Step 7. VISUALISE: Effect of EnviroPerformance_score on intervention score (Survey 1)
# ==============================================================================
visreg(S2_EP_glmmTMB_fulldata, "EnviroPerformance_score", scale = "response", partial = TRUE)

S2_visreg <- visreg(S2_EP_glmmTMB_fulldata, "EnviroPerformance_score", scale = "response", plot = FALSE)

# ---- Rebuild the exact complete-case data used by the model ----
# visreg's $res only contains rows retained after glmmTMB's internal NA-omission, 
# so grouping columns (Country.clean, World.region.clean) must come from the same complete-case subset, in the same row order 
# i.e. NOT from Survey_1_long_indices_wLatLong_wRichness (wrong dataset: that's long-format, one row per domain score, different structure).
# Use S2_EP_model_data_noNAs (built in Step 6):

S2_fit_data <- S2_visreg$fit
S2_fit_data$visregFit <- S2_fit_data$visregFit * 3
S2_fit_data$visregLwr <- S2_fit_data$visregLwr * 3
S2_fit_data$visregUpr <- S2_fit_data$visregUpr * 3

S2_res_data <- S2_visreg$res
S2_res_data$visregRes <- S2_res_data$visregRes * 3
S2_res_data$Country.clean <- S2_EP_model_data_noNAs$Country.clean
S2_res_data$World.region.clean <- S2_EP_model_data_noNAs$World.region.clean
S2_res_data$Respondent.clean <- S2_EP_model_data_noNAs$Respondent.clean
S2_res_data$Crop.type.clean <- S2_EP_model_data_noNAs$Crop.type.clean

# ---- Base plot: fitted line + CI ribbon + partial residuals coloured by country ----
ggplot(S2_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_jitter(data = S2_res_data,
              aes(x = EnviroPerformance_score, y = visregRes, colour = Country.clean),
              width = 0.5, height = 0.05, alpha = 0.7, size = 2) +
  labs(x = "Environmental Performance Score", y = " (0-3)",
       colour = "Country") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  theme_minimal() +
  theme(legend.text = element_text(size = 8))

# ---- Add n per country to legend labels ----
S2_country_n <- S2_EP_model_data_noNAs %>%
  count(Country.clean, name = "n")

S2_res_data <- S2_res_data %>%
  select(-any_of(c("n", "Country.label"))) %>%
  left_join(S2_country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

ggplot(S2_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point(data = S2_res_data,
             aes(x = EnviroPerformance_score, y = visregRes, colour = Country.label),
             alpha = 0.7, size = 2,
             position = position_jitter(width = 0.5, height = 0.05, seed = 1)) +
  geom_text_repel(data = S2_res_data,
                  aes(x = EnviroPerformance_score, y = visregRes, label = Country.clean,
                      colour = Country.label),
                  size = 2.5, show.legend = FALSE,
                  position = position_jitter(width = 0.5, height = 0.05, seed = 1),
                  max.overlaps = 20, segment.size = 0.2) +
  labs(x = "Environmental Performance Score", y = "",
       colour = "Country") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_viridis_d(option = "turbo") +
  theme_minimal() +
  theme(legend.text = element_text(size = 8))

# ---- Same plot, coloured by world region instead of country ----
S2_region_n <- S2_EP_model_data_noNAs %>%
  count(World.region.clean, name = "n_region")

S2_res_data <- S2_res_data %>%
  select(-any_of(c("n_region", "Region.label"))) %>%
  left_join(S2_region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

# Map your custom region_colours onto the "Region (n=X)" labels
S2_region_label_lookup <- S2_res_data %>%
  distinct(World.region.clean, Region.label) %>%
  mutate(colour = region_colours[World.region.clean])

S2_region_colours_labelled <- setNames(S2_region_label_lookup$colour, S2_region_label_lookup$Region.label)

ggplot(S2_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(data = S2_res_data,
             aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
             alpha = 0.75, size = 2.2,
             position = position_jitter(width = 0.5, height = 0.05, seed = 1)) +
  geom_text_repel(data = S2_res_data,
                  aes(x = EnviroPerformance_score, y = visregRes, label = Country.clean,
                      colour = Region.label),
                  size = 2.4, show.legend = FALSE,
                  position = position_jitter(width = 0.5, height = 0.05, seed = 1),
                  max.overlaps = 20, segment.size = 0.2) +
  labs(x = "Environmental Performance Score", y = "Mean Intervention Score (0-3)",
       colour = "World Region") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_manual(values = S2_region_colours_labelled) +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10))


# ---- Same plot but interactive to see respondent, crop type etc when hovering over points
S2_res_data <- S2_res_data %>%
  mutate(hover_text = paste0("Respondent: ", Respondent.clean,
                             "<br>Country: ", Country.clean,
                             "<br>Region: ", World.region.clean,
                             "<br>Crop type: ", Crop.type.clean,
                             "<br>EnviroPerformance: ", round(EnviroPerformance_score, 2),
                             "<br>Score: ", round(visregRes, 2)))

S2_p_region <- ggplot(S2_fit_data, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(data = S2_res_data,
             aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label, text = hover_text),
             alpha = 0.75, size = 2.2,
             position = position_jitter(width = 0.5, height = 0.05, seed = 1)) +
  labs(x = "Environmental Performance Score", y = "Mean Intervention Score (0-3)",
       colour = "World Region") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_manual(values = S2_region_colours_labelled) +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 10))

ggplotly(S2_p_region, tooltip = "text")




# ==============================================================================
# STEP 8: Survey 2 — Individual intervention scores
# ==============================================================================
# The overall score provides a general measure of the level of testing of all bat-supportive interventions. 
# Now repeat  model-selection approach for each intervention separately to determine whether the relationships identified above (Env Performance being important) are consistent across intervention types.

# Each intervention is analysed independently using the same candidate predictor structure and Country as a random effect. 
# AICc comparisons are made within intervention type only.
# ==============================================================================
# ==============================================================================
# 8A. Prepare intervention-specific data
# ==============================================================================
# Use complete cases across all candidate predictors so that models within each
# intervention are fitted to the same observations and can be compared using AICc.
d_roost <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Artificial Roost") %>%
  filter(
    !is.na(Score),
    !is.na(EnviroPerformance_score),
    !is.na(Exact_latitude),
    !is.na(GDP_per_capita),
    !is.na(AllResearchAndDev_percentGPD),
    !is.na(AgForestryFish_ValueAdded_percentGDP),
    !is.na(AgResearchAndDev_PPP2017),
    !is.na(Country.clean))

d_water <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Artificial Water Sources") %>%
  filter(
    !is.na(Score),
    !is.na(EnviroPerformance_score),
    !is.na(Exact_latitude),
    !is.na(GDP_per_capita),
    !is.na(AllResearchAndDev_percentGPD),
    !is.na(AgForestryFish_ValueAdded_percentGDP),
    !is.na(AgResearchAndDev_PPP2017),
    !is.na(Country.clean))

d_lure <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Acoustic Bat Lure") %>%
  filter(
    !is.na(Score),
    !is.na(EnviroPerformance_score),
    !is.na(Exact_latitude),
    !is.na(GDP_per_capita),
    !is.na(AllResearchAndDev_percentGPD),
    !is.na(AgForestryFish_ValueAdded_percentGDP),
    !is.na(AgResearchAndDev_PPP2017),
    !is.na(Country.clean))

d_education <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Bat Education") %>%
  filter(
    !is.na(Score),
    !is.na(EnviroPerformance_score),
    !is.na(Exact_latitude),
    !is.na(GDP_per_capita),
    !is.na(AllResearchAndDev_percentGPD),
    !is.na(AgForestryFish_ValueAdded_percentGDP),
    !is.na(AgResearchAndDev_PPP2017),
    !is.na(Country.clean))

d_agchem <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Agrochemical Reduction") %>%
  filter(
    !is.na(Score),
    !is.na(EnviroPerformance_score),
    !is.na(Exact_latitude),
    !is.na(GDP_per_capita),
    !is.na(AllResearchAndDev_percentGPD),
    !is.na(AgForestryFish_ValueAdded_percentGDP),
    !is.na(AgResearchAndDev_PPP2017),
    !is.na(Country.clean))

d_reveg <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Revegetation") %>%
  filter(
    !is.na(Score),
    !is.na(EnviroPerformance_score),
    !is.na(Exact_latitude),
    !is.na(GDP_per_capita),
    !is.na(AllResearchAndDev_percentGPD),
    !is.na(AgForestryFish_ValueAdded_percentGDP),
    !is.na(AgResearchAndDev_PPP2017),
    !is.na(Country.clean))


# ==============================================================================
# 2B. Rescale individual intervention scores for beta regression
# ==============================================================================
d_roost <- d_roost %>%
  mutate(Score.prop = Score / 3, n_obs = n(),
         Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)

d_water <- d_water %>%
  mutate(Score.prop = Score / 3, n_obs = n(),
         Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)

d_lure <- d_lure %>%
  mutate(Score.prop = Score / 3, n_obs = n(),
         Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)

d_education <- d_education %>%
  mutate(Score.prop = Score / 3, n_obs = n(),
         Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)

d_agchem <- d_agchem %>%
  mutate(Score.prop = Score / 3, n_obs = n(),
         Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)

d_reveg <- d_reveg %>%
  mutate(Score.prop = Score / 3, n_obs = n(),
         Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)


# ==============================================================================
# 8c. FIT EACH INTERVENTION MODEL INDIVIDUALLY
# But base the model structure on the best performing model for the mean intervention score across all interventions
# ==============================================================================
# --- Artificial Roost ---
S2_roost <- glmmTMB(
  Score.beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(),
  data = d_roost)
# --- Artificial Water Sources ---
S2_water <- glmmTMB(
  Score.beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(),
  data = d_water)
# --- Acoustic Bat Lure ---
S2_lure <- glmmTMB(
  Score.beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(),
  data = d_lure)
# --- Bat Education ---
S2_education <- glmmTMB(
  Score.beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(),
  data = d_education)
# --- Agrochemical Reduction ---
S2_agchem <- glmmTMB(
  Score.beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(),
  data = d_agchem)
# --- Revegetation ---
S2_revegetation <- glmmTMB(
  Score.beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean),
  family = beta_family(),
  data = d_reveg)

# ------------------------------
# 2d. QUICK SUMMARY OF ENVIRONMENTAL PERFORMANCE EFFECT
# ------------------------------
data.frame(
  Intervention = c(
    "Artificial Roost",
    "Artificial Water Sources",
    "Acoustic Bat Lure",
    "Bat Education",
    "Agrochemical Reduction",
    "Revegetation"
  ),
  Estimate = c(
    fixef(S2_roost)$cond["EnviroPerformance_score"],
    fixef(S2_water)$cond["EnviroPerformance_score"],
    fixef(S2_lure)$cond["EnviroPerformance_score"],
    fixef(S2_education)$cond["EnviroPerformance_score"],
    fixef(S2_agchem)$cond["EnviroPerformance_score"],
    fixef(S2_revegetation)$cond["EnviroPerformance_score"]
  ),
  SE = c(
    summary(S2_roost)$coefficients$cond["EnviroPerformance_score", "Std. Error"],
    summary(S2_water)$coefficients$cond["EnviroPerformance_score", "Std. Error"],
    summary(S2_lure)$coefficients$cond["EnviroPerformance_score", "Std. Error"],
    summary(S2_education)$coefficients$cond["EnviroPerformance_score", "Std. Error"],
    summary(S2_agchem)$coefficients$cond["EnviroPerformance_score", "Std. Error"],
    summary(S2_revegetation)$coefficients$cond["EnviroPerformance_score", "Std. Error"]
  ),
  P = c(
    summary(S2_roost)$coefficients$cond["EnviroPerformance_score", "Pr(>|z|)"],
    summary(S2_water)$coefficients$cond["EnviroPerformance_score", "Pr(>|z|)"],
    summary(S2_lure)$coefficients$cond["EnviroPerformance_score", "Pr(>|z|)"],
    summary(S2_education)$coefficients$cond["EnviroPerformance_score", "Pr(>|z|)"],
    summary(S2_agchem)$coefficients$cond["EnviroPerformance_score", "Pr(>|z|)"],
    summary(S2_revegetation)$coefficients$cond["EnviroPerformance_score", "Pr(>|z|)"]
  )
)


# ------------------------------
# 2e. VISREG of each model
#     — ARTIFICIAL ROOST
# ------------------------------
v_roost <- visreg(S2_roost,
  "EnviroPerformance_score",
  data = d_roost,
  scale = "response",
  plot = FALSE)

fit_roost <- v_roost$fit %>%
  mutate(
    visregFit = visregFit * 3,
    visregLwr = visregLwr * 3,
    visregUpr = visregUpr * 3)

res_roost <- v_roost$res %>%
  mutate(
    visregRes = visregRes * 3,
    Country.clean = d_roost$Country.clean,
    World.region.clean = d_roost$World.region.clean)

country_n <- d_roost %>%
  count(Country.clean, name = "n")

res_roost <- res_roost %>%
  left_join(country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

region_n <- d_roost %>%
  count(World.region.clean, name = "n_region")

res_roost <- res_roost %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

ggplot(fit_roost, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(
    data = res_roost,
    aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
    alpha = 0.75,
    size = 2.2,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1)
  ) +
  geom_text_repel(
    data = res_roost,
    aes(
      x = EnviroPerformance_score,
      y = visregRes,
      label = Country.clean,
      colour = Region.label
    ),
    size = 2.4,
    show.legend = FALSE,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1),
    max.overlaps = 20,
    segment.size = 0.2
  ) +
  labs(
    x = "Environmental Performance Score",
    y = "Artificial Roost Score (0-3)",
    colour = "World Region") +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10))

# ------------------------------
# VISREG — ARTIFICIAL WATER SOURCES
# ------------------------------
v_water <- visreg(
  S2_water,
  "EnviroPerformance_score",
  data = d_water,
  scale = "response",
  plot = FALSE)

fit_water <- v_water$fit %>%
  mutate(
    visregFit = visregFit * 3,
    visregLwr = visregLwr * 3,
    visregUpr = visregUpr * 3
)

res_water <- v_water$res %>%
  mutate(
    visregRes = visregRes * 3,
    Country.clean = d_water$Country.clean,
    World.region.clean = d_water$World.region.clean
  )

country_n <- d_water %>%
  count(Country.clean, name = "n")

res_water <- res_water %>%
  left_join(country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

region_n <- d_water %>%
  count(World.region.clean, name = "n_region")

res_water <- res_water %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

ggplot(fit_water, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(
    data = res_water,
    aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
    alpha = 0.75,
    size = 2.2,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1)
  ) +
  geom_text_repel(
    data = res_water,
    aes(
      x = EnviroPerformance_score,
      y = visregRes,
      label = Country.clean,
      colour = Region.label
    ),
    size = 2.4,
    show.legend = FALSE,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1),
    max.overlaps = 20,
    segment.size = 0.2
  ) +
  labs(
    x = "Environmental Performance Score",
    y = "Artificial Water Sources Score (0-3)",
    colour = "World Region"
  ) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10) )

# ------------------------------
# VISREG — ACOUSTIC BAT LURE
# ------------------------------
v_lure <- visreg(
  S2_lure,
  "EnviroPerformance_score",
  data = d_lure,
  scale = "response",
  plot = FALSE
)

fit_lure <- v_lure$fit %>%
  mutate(
    visregFit = visregFit * 3,
    visregLwr = visregLwr * 3,
    visregUpr = visregUpr * 3)

res_lure <- v_lure$res %>%
  mutate(
    visregRes = visregRes * 3,
    Country.clean = d_lure$Country.clean,
    World.region.clean = d_lure$World.region.clean)

country_n <- d_lure %>%
  count(Country.clean, name = "n")

res_lure <- res_lure %>%
  left_join(country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

region_n <- d_lure %>%
  count(World.region.clean, name = "n_region")

res_lure <- res_lure %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

ggplot(fit_lure, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(
    data = res_lure,
    aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
    alpha = 0.75,
    size = 2.2,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1)
  ) +
  geom_text_repel(
    data = res_lure,
    aes(
      x = EnviroPerformance_score,
      y = visregRes,
      label = Country.clean,
      colour = Region.label
    ),
    size = 2.4,
    show.legend = FALSE,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1),
    max.overlaps = 20,
    segment.size = 0.2
  ) +
  labs(
    x = "Environmental Performance Score",
    y = "Acoustic Bat Lure Score (0-3)",
    colour = "World Region"
  ) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10)
  )

# ------------------------------
# VISREG — BAT EDUCATION
# ------------------------------
v_education <- visreg(
  S2_education,
  "EnviroPerformance_score",
  data = d_education,
  scale = "response",
  plot = FALSE
)

fit_education <- v_education$fit %>%
  mutate(
    visregFit = visregFit * 3,
    visregLwr = visregLwr * 3,
    visregUpr = visregUpr * 3)

res_education <- v_education$res %>%
  mutate(
    visregRes = visregRes * 3,
    Country.clean = d_education$Country.clean,
    World.region.clean = d_education$World.region.clean)

country_n <- d_education %>%
  count(Country.clean, name = "n")

res_education <- res_education %>%
  left_join(country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

region_n <- d_education %>%
  count(World.region.clean, name = "n_region")

res_education <- res_education %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

ggplot(fit_education, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(
    data = res_education,
    aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
    alpha = 0.75,
    size = 2.2,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1)
  ) +
  geom_text_repel(
    data = res_education,
    aes(
      x = EnviroPerformance_score,
      y = visregRes,
      label = Country.clean,
      colour = Region.label
    ),
    size = 2.4,
    show.legend = FALSE,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1),
    max.overlaps = 20,
    segment.size = 0.2
  ) +
  labs(
    x = "Environmental Performance Score",
    y = "Bat Education Score (0-3)",
    colour = "World Region"
  ) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10)
  )

# ------------------------------
# VISREG — AGROCHEMICAL REDUCTION
# ------------------------------
v_agchem <- visreg(
  S2_agchem,
  "EnviroPerformance_score",
  data = d_agchem,
  scale = "response",
  plot = FALSE)

fit_agchem <- v_agchem$fit %>%
  mutate(
    visregFit = visregFit * 3,
    visregLwr = visregLwr * 3,
    visregUpr = visregUpr * 3)

res_agchem <- v_agchem$res %>%
  mutate(
    visregRes = visregRes * 3,
    Country.clean = d_agchem$Country.clean,
    World.region.clean = d_agchem$World.region.clean
  )

country_n <- d_agchem %>%
  count(Country.clean, name = "n")

res_agchem <- res_agchem %>%
  left_join(country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

region_n <- d_agchem %>%
  count(World.region.clean, name = "n_region")

res_agchem <- res_agchem %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

ggplot(fit_agchem, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(
    data = res_agchem,
    aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
    alpha = 0.75,
    size = 2.2,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1)
  ) +
  geom_text_repel(
    data = res_agchem,
    aes(
      x = EnviroPerformance_score,
      y = visregRes,
      label = Country.clean,
      colour = Region.label
    ),
    size = 2.4,
    show.legend = FALSE,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1),
    max.overlaps = 20,
    segment.size = 0.2
  ) +
  labs(
    x = "Environmental Performance Score",
    y = "Agrochemical Reduction Score (0-3)",
    colour = "World Region"
  ) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10))


# ------------------------------
# VISREG — REVEGETATION
# ------------------------------
v_reveg <- visreg(
  S2_revegetation,
  "EnviroPerformance_score",
  data = d_reveg,
  scale = "response",
  plot = FALSE
)

fit_reveg <- v_reveg$fit %>%
  mutate(
    visregFit = visregFit * 3,
    visregLwr = visregLwr * 3,
    visregUpr = visregUpr * 3
  )

res_reveg <- v_reveg$res %>%
  mutate(
    visregRes = visregRes * 3,
    Country.clean = d_reveg$Country.clean,
    World.region.clean = d_reveg$World.region.clean
  )

country_n <- d_reveg %>%
  count(Country.clean, name = "n")

res_reveg <- res_reveg %>%
  left_join(country_n, by = "Country.clean") %>%
  mutate(Country.label = paste0(Country.clean, " (n=", n, ")"))

region_n <- d_reveg %>%
  count(World.region.clean, name = "n_region")

res_reveg <- res_reveg %>%
  left_join(region_n, by = "World.region.clean") %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))

ggplot(fit_reveg, aes(x = EnviroPerformance_score, y = visregFit)) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(
    data = res_reveg,
    aes(x = EnviroPerformance_score, y = visregRes, colour = Region.label),
    alpha = 0.75,
    size = 2.2,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1)
  ) +
  geom_text_repel(
    data = res_reveg,
    aes(
      x = EnviroPerformance_score,
      y = visregRes,
      label = Country.clean,
      colour = Region.label
    ),
    size = 2.4,
    show.legend = FALSE,
    position = position_jitter(width = 0.5, height = 0.05, seed = 1),
    max.overlaps = 20,
    segment.size = 0.2
  ) +
  labs(
    x = "Environmental Performance Score",
    y = "Revegetation Score (0-3)",
    colour = "World Region"
  ) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10)
  )


#===============================================================================
# Part 9) SURVEY 2 — INTERVENTION-SPECIFIC CANDIDATE MODELS
# But test all national-level socio-economic predictors, not just Env Performance
#===============================================================================
# ------------------------------
# 9a. DEFINE CANDIDATE PREDICTORS
# ------------------------------
# Candidate predictors used in the overall Survey 2 analysis:
#
# EnviroPerformance_score
# Exact_latitude
# GDP_per_capita
# AgForestryFish_ValueAdded_percentGDP
# AgResearchAndDev_PPP2017
#
# BatRichness is left out below unless it is available in the dataset.

# ------------------------------
# 9b. CREATE INTERVENTION-SPECIFIC COMPLETE-CASE DATASETS
# ------------------------------
# These contain the same predictor variables for every candidate model,
# ensuring that AICc comparisons within each intervention use the same rows.

d_roost <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Artificial Roost",
         !is.na(Score), !is.na(EnviroPerformance_score), !is.na(Exact_latitude),
         !is.na(GDP_per_capita), !is.na(AgForestryFish_ValueAdded_percentGDP),
         !is.na(AgResearchAndDev_PPP2017), !is.na(Country.clean), !is.na(BatRichness))

d_water <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Artificial Water Sources",
         !is.na(Score), !is.na(EnviroPerformance_score), !is.na(Exact_latitude),
         !is.na(GDP_per_capita), !is.na(AgForestryFish_ValueAdded_percentGDP),
         !is.na(AgResearchAndDev_PPP2017), !is.na(Country.clean), !is.na(BatRichness))

d_lure <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Acoustic Bat Lure",
         !is.na(Score), !is.na(EnviroPerformance_score), !is.na(Exact_latitude),
         !is.na(GDP_per_capita), !is.na(AgForestryFish_ValueAdded_percentGDP),
         !is.na(AgResearchAndDev_PPP2017), !is.na(Country.clean), !is.na(BatRichness))

d_education <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Bat Education",
         !is.na(Score), !is.na(EnviroPerformance_score), !is.na(Exact_latitude),
         !is.na(GDP_per_capita), !is.na(AgForestryFish_ValueAdded_percentGDP),
         !is.na(AgResearchAndDev_PPP2017), !is.na(Country.clean), !is.na(BatRichness))

d_agchem <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Agrochemical Reduction",
         !is.na(Score), !is.na(EnviroPerformance_score), !is.na(Exact_latitude),
         !is.na(GDP_per_capita), !is.na(AgForestryFish_ValueAdded_percentGDP),
         !is.na(AgResearchAndDev_PPP2017), !is.na(Country.clean), !is.na(BatRichness))

d_revegetation <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(Score.type == "Revegetation",
         !is.na(Score), !is.na(EnviroPerformance_score), !is.na(Exact_latitude),
         !is.na(GDP_per_capita), !is.na(AgForestryFish_ValueAdded_percentGDP),
         !is.na(AgResearchAndDev_PPP2017), !is.na(Country.clean), !is.na(BatRichness))

# ------------------------------
# 9c. BETA-RESCALE EACH INTERVENTION
# ------------------------------
d_roost <- d_roost %>% mutate(Score.prop = Score / 3, n_obs = n(), Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)
d_water <- d_water %>% mutate(Score.prop = Score / 3, n_obs = n(), Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)
d_lure <- d_lure %>% mutate(Score.prop = Score / 3, n_obs = n(), Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)
d_education <- d_education %>% mutate(Score.prop = Score / 3, n_obs = n(), Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)
d_agchem <- d_agchem %>% mutate(Score.prop = Score / 3, n_obs = n(), Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)
d_revegetation <- d_revegetation %>% mutate(Score.prop = Score / 3, n_obs = n(), Score.beta = (Score.prop * (n_obs - 1) + 0.5) / n_obs)

# ------------------------------
# 9d. ARTIFICIAL ROOST — CANDIDATE MODELS
# -----------------------------
S2_roost_null <- glmmTMB(Score.beta ~ 1 + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_null_Rich <- glmmTMB(Score.beta ~ log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_Lat <- glmmTMB(Score.beta ~ abs(Exact_latitude) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_Lat_Rich <- glmmTMB(Score.beta ~ abs(Exact_latitude) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_GDP <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_GDP_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_Ag_GDP <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_Ag_GDP_Rich <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_Ag_RD <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_Ag_RD_Rich <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_EP <- glmmTMB(Score.beta ~ EnviroPerformance_score + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_EP_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_EP_AgRD <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_EP_AgRD_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_GDP_AgRD <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)
S2_roost_GDP_AgRD_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_roost)

# ------------------------------
# 9e. ARTIFICIAL WATER SOURCES — CANDIDATE MODELS
# ------------------------------
S2_water_null <- glmmTMB(Score.beta ~ 1 + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_null_Rich <- glmmTMB(Score.beta ~ log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_Lat <- glmmTMB(Score.beta ~ abs(Exact_latitude) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_Lat_Rich <- glmmTMB(Score.beta ~ abs(Exact_latitude) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_GDP <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_GDP_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_Ag_GDP <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_Ag_GDP_Rich <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_Ag_RD <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_Ag_RD_Rich <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_EP <- glmmTMB(Score.beta ~ EnviroPerformance_score + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_EP_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_EP_AgRD <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_EP_AgRD_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_GDP_AgRD <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)
S2_water_GDP_AgRD_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_water)

# ------------------------------
# 9f. ACOUSTIC BAT LURE — CANDIDATE MODELS
# -----------------------------
S2_lure_null <- glmmTMB(Score.beta ~ 1 + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_null_Rich <- glmmTMB(Score.beta ~ log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_Lat <- glmmTMB(Score.beta ~ abs(Exact_latitude) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_Lat_Rich <- glmmTMB(Score.beta ~ abs(Exact_latitude) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_GDP <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_GDP_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_Ag_GDP <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_Ag_GDP_Rich <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_Ag_RD <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_Ag_RD_Rich <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_EP <- glmmTMB(Score.beta ~ EnviroPerformance_score + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_EP_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_EP_AgRD <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_EP_AgRD_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_GDP_AgRD <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)
S2_lure_GDP_AgRD_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_lure)

# ------------------------------
# 9g. BAT EDUCATION — CANDIDATE MODELS
# -----------------------------
S2_education_null <- glmmTMB(Score.beta ~ 1 + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_null_Rich <- glmmTMB(Score.beta ~ log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_Lat <- glmmTMB(Score.beta ~ abs(Exact_latitude) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_Lat_Rich <- glmmTMB(Score.beta ~ abs(Exact_latitude) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_GDP <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_GDP_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_Ag_GDP <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_Ag_GDP_Rich <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_Ag_RD <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_Ag_RD_Rich <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_EP <- glmmTMB(Score.beta ~ EnviroPerformance_score + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_EP_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_EP_AgRD <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_EP_AgRD_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_GDP_AgRD <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)
S2_education_GDP_AgRD_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_education)

# ------------------------------
# 9h. AGROCHEMICAL REDUCTION — CANDIDATE MODELS
# ------------------------------
S2_agchem_null <- glmmTMB(Score.beta ~ 1 + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_null_Rich <- glmmTMB(Score.beta ~ log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_Lat <- glmmTMB(Score.beta ~ abs(Exact_latitude) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_Lat_Rich <- glmmTMB(Score.beta ~ abs(Exact_latitude) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_GDP <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_GDP_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_Ag_GDP <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_Ag_GDP_Rich <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_Ag_RD <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_Ag_RD_Rich <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_EP <- glmmTMB(Score.beta ~ EnviroPerformance_score + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_EP_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_EP_AgRD <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_EP_AgRD_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_GDP_AgRD <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)
S2_agchem_GDP_AgRD_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_agchem)

# ------------------------------
# 9i. REVEGETATION — CANDIDATE MODELS
# ------------------------------
S2_revegetation_null <- glmmTMB(Score.beta ~ 1 + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_null_Rich <- glmmTMB(Score.beta ~ log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_Lat <- glmmTMB(Score.beta ~ abs(Exact_latitude) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_Lat_Rich <- glmmTMB(Score.beta ~ abs(Exact_latitude) + log10(BatRichness) + (1 | Country.clean) +(1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_GDP <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_GDP_Rich <- glmmTMB(Score.beta ~log10(GDP_per_capita) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_Ag_GDP <- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_Ag_GDP_Rich<- glmmTMB(Score.beta ~ log10(AgForestryFish_ValueAdded_percentGDP) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_Ag_RD <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_Ag_RD_Rich <- glmmTMB(Score.beta ~ log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_EP <- glmmTMB(Score.beta ~ EnviroPerformance_score + (1 |Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_EP_Rich <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_EP_AgRD <- glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + (1| Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_EP_AgRD_Rich <-glmmTMB(Score.beta ~ EnviroPerformance_score + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_GDP_AgRD <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)
S2_revegetation_GDP_AgRD_Rich <- glmmTMB(Score.beta ~ log10(GDP_per_capita) + log10(AgResearchAndDev_PPP2017) + log10(BatRichness) + (1 | Country.clean) + (1 | Respondent.clean), family = beta_family(), data = d_revegetation)

# ------------------------------
# AICc — ARTIFICIAL ROOST
# ------------------------------
S2_AICc_roost <- data.frame(
  Model =c("Null", "Null + Bat richness","Latitude", "Latitude + Bat richness", "GDP", "GDP + Bat richness", "Agricultural GDP", "Agricultural GDP + Bat richness", "Agricultural R&D", "Agricultural R&D + Bat richness", "Environmental Performance", "Environmental Performance + Bat richness", "EP + Agricultural R&D", "EP + Agricultural R&D + Bat richness", "GDP + Agricultural R&D", "GDP + Agricultural R&D + Bat richness"),
  AICc = c(AICc(S2_roost_null), AICc(S2_roost_null_Rich), AICc(S2_roost_Lat), AICc(S2_roost_Lat_Rich), AICc(S2_roost_GDP), AICc(S2_roost_GDP_Rich), AICc(S2_roost_Ag_GDP), AICc(S2_roost_Ag_GDP_Rich), AICc(S2_roost_Ag_RD), AICc(S2_roost_Ag_RD_Rich), AICc(S2_roost_EP), AICc(S2_roost_EP_Rich), AICc(S2_roost_EP_AgRD), AICc(S2_roost_EP_AgRD_Rich), AICc(S2_roost_GDP_AgRD), AICc(S2_roost_GDP_AgRD_Rich))
) %>% arrange(AICc) %>% mutate(delta_AICc = AICc - min(AICc), AICc_weight = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)), supported = delta_AICc < 2)

# ------------------------------
# AICc — ARTIFICIAL WATER SOURCES
# ------------------------------
S2_AICc_water <- data.frame(
  Model = c("Null", "Null + Bat richness", "Latitude", "Latitude + Bat richness", "GDP", "GDP + Bat richness","Agricultural GDP", "Agricultural GDP + Bat richness", "Agricultural R&D", "Agricultural R&D + Bat richness", "Environmental Performance", "Environmental Performance + Bat richness", "EP + Agricultural R&D", "EP + Agricultural R&D + Bat richness", "GDP + Agricultural R&D", "GDP + Agricultural R&D + Bat richness"),
  AICc = c(AICc(S2_water_null), AICc(S2_water_null_Rich), AICc(S2_water_Lat), AICc(S2_water_Lat_Rich), AICc(S2_water_GDP), AICc(S2_water_GDP_Rich), AICc(S2_water_Ag_GDP), AICc(S2_water_Ag_GDP_Rich), AICc(S2_water_Ag_RD), AICc(S2_water_Ag_RD_Rich), AICc(S2_water_EP), AICc(S2_water_EP_Rich), AICc(S2_water_EP_AgRD), AICc(S2_water_EP_AgRD_Rich), AICc(S2_water_GDP_AgRD), AICc(S2_water_GDP_AgRD_Rich))
) %>% arrange(AICc) %>% mutate(delta_AICc = AICc - min(AICc), AICc_weight = exp(-0.5 * delta_AICc) /sum(exp(-0.5 * delta_AICc)), supported = delta_AICc < 2)

# ------------------------------
# AICc — ACOUSTIC BAT LURE
# ------------------------------
S2_AICc_lure <- data.frame(
  Model = c("Null", "Null + Bat richness", "Latitude", "Latitude + Bat richness", "GDP", "GDP + Bat richness", "Agricultural GDP", "Agricultural GDP + Bat richness", "Agricultural R&D", "Agricultural R&D + Bat richness", "Environmental Performance", "Environmental Performance + Bat richness", "EP + Agricultural R&D", "EP + Agricultural R&D + Bat richness", "GDP + Agricultural R&D", "GDP + Agricultural R&D + Bat richness"),
  AICc = c(AICc(S2_lure_null), AICc(S2_lure_null_Rich), AICc(S2_lure_Lat), AICc(S2_lure_Lat_Rich), AICc(S2_lure_GDP), AICc(S2_lure_GDP_Rich), AICc(S2_lure_Ag_GDP), AICc(S2_lure_Ag_GDP_Rich), AICc(S2_lure_Ag_RD), AICc(S2_lure_Ag_RD_Rich), AICc(S2_lure_EP), AICc(S2_lure_EP_Rich), AICc(S2_lure_EP_AgRD), AICc(S2_lure_EP_AgRD_Rich), AICc(S2_lure_GDP_AgRD), AICc(S2_lure_GDP_AgRD_Rich))
) %>% arrange(AICc) %>% mutate(delta_AICc = AICc - min(AICc), AICc_weight = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)), supported = delta_AICc < 2)

# ------------------------------
# AICc — BAT EDUCATION
# ------------------------------
S2_AICc_education <- data.frame(
  Model = c("Null", "Null + Bat richness", "Latitude", "Latitude + Bat richness", "GDP", "GDP + Bat richness", "Agricultural GDP", "Agricultural GDP + Bat richness", "Agricultural R&D", "Agricultural R&D + Bat richness", "Environmental Performance", "Environmental Performance + Bat richness", "EP + Agricultural R&D", "EP + Agricultural R&D + Bat richness", "GDP + Agricultural R&D", "GDP + Agricultural R&D + Bat richness"),
  AICc = c(AICc(S2_education_null), AICc(S2_education_null_Rich), AICc(S2_education_Lat),AICc(S2_education_Lat_Rich), AICc(S2_education_GDP), AICc(S2_education_GDP_Rich), AICc(S2_education_Ag_GDP), AICc(S2_education_Ag_GDP_Rich), AICc(S2_education_Ag_RD), AICc(S2_education_Ag_RD_Rich), AICc(S2_education_EP), AICc(S2_education_EP_Rich), AICc(S2_education_EP_AgRD), AICc(S2_education_EP_AgRD_Rich), AICc(S2_education_GDP_AgRD), AICc(S2_education_GDP_AgRD_Rich))
) %>% arrange(AICc) %>% mutate(delta_AICc = AICc - min(AICc), AICc_weight = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)), supported = delta_AICc < 2)

# ------------------------------
# AICc — AGROCHEMICAL REDUCTION
# ------------------------------
S2_AICc_agchem <- data.frame(
  Model = c("Null", "Null + Bat richness", "Latitude", "Latitude + Bat richness", "GDP", "GDP + Bat richness", "Agricultural GDP", "Agricultural GDP + Bat richness", "Agricultural R&D", "Agricultural R&D + Bat richness", "Environmental Performance", "Environmental Performance + Bat richness", "EP + Agricultural R&D", "EP + Agricultural R&D + Bat richness", "GDP + Agricultural R&D", "GDP + Agricultural R&D + Bat richness"),
  AICc = c(AICc(S2_agchem_null), AICc(S2_agchem_null_Rich), AICc(S2_agchem_Lat), AICc(S2_agchem_Lat_Rich), AICc(S2_agchem_GDP), AICc(S2_agchem_GDP_Rich), AICc(S2_agchem_Ag_GDP), AICc(S2_agchem_Ag_GDP_Rich), AICc(S2_agchem_Ag_RD), AICc(S2_agchem_Ag_RD_Rich), AICc(S2_agchem_EP), AICc(S2_agchem_EP_Rich), AICc(S2_agchem_EP_AgRD), AICc(S2_agchem_EP_AgRD_Rich), AICc(S2_agchem_GDP_AgRD), AICc(S2_agchem_GDP_AgRD_Rich))
) %>% arrange(AICc) %>% mutate(delta_AICc = AICc - min(AICc), AICc_weight = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)), supported = delta_AICc < 2)

# ------------------------------
# AICc — REVEGETATION
# ------------------------------
S2_AICc_revegetation <- data.frame(
  Model = c("Null", "Null + Bat richness", "Latitude", "Latitude + Bat richness", "GDP", "GDP + Bat richness", "Agricultural GDP", "Agricultural GDP + Bat richness", "Agricultural R&D", "Agricultural R&D + Bat richness", "Environmental Performance", "Environmental Performance + Bat richness", "EP + Agricultural R&D", "EP + Agricultural R&D + Bat richness", "GDP + Agricultural R&D", "GDP + Agricultural R&D + Bat richness"),
  AICc = c(AICc(S2_revegetation_null), AICc(S2_revegetation_null_Rich), AICc(S2_revegetation_Lat), AICc(S2_revegetation_Lat_Rich), AICc(S2_revegetation_GDP), AICc(S2_revegetation_GDP_Rich), AICc(S2_revegetation_Ag_GDP), AICc(S2_revegetation_Ag_GDP_Rich), AICc(S2_revegetation_Ag_RD), AICc(S2_revegetation_Ag_RD_Rich), AICc(S2_revegetation_EP), AICc(S2_revegetation_EP_Rich), AICc(S2_revegetation_EP_AgRD), AICc(S2_revegetation_EP_AgRD_Rich), AICc(S2_revegetation_GDP_AgRD), AICc(S2_revegetation_GDP_AgRD_Rich))
) %>% arrange(AICc) %>% mutate(delta_AICc = AICc - min(AICc), AICc_weight = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)), supported = delta_AICc < 2)

# ------------------------------
# VIEW ALL AICc TABLES
# ------------------------------
S2_AICc_roost
S2_AICc_water
S2_AICc_lure
S2_AICc_education
S2_AICc_agchem
S2_AICc_revegetation

# ------------------------------
# BEST-SUPPORTED MODEL FROM EACH
# ------------------------------
S2_AICc_roost %>% slice(1) # Null is best
S2_AICc_water %>% slice(1) # Latitude - but is comparable to NULL
S2_AICc_lure %>% slice(1) # Null is best
S2_AICc_education %>% slice(1) # Latitude - but is comparable to NULL
S2_AICc_agchem %>% slice(1)  # EnvPerformance - but is comparable to NULL
S2_AICc_revegetation %>% slice(1)  # EnvPerformance - but is comparable to NULL

# Overall interpretation:
# Substantial model uncertainty across interventions; the null remained supported (ΔAICc < 2) for all six.
# Non-null models had greater support for water, education, agrochemical reduction and revegetation,
# while the null ranked highest for roost and lure.
# Overall, national-level predictors provided variable but limited support for intervention scores.

#--------------------------------------------------------------
# Explore the direction of latitude and EnvPerformance effects 
# for interventions where these were the best model (though comparable to null)
#--------------------------------------------------------------
# Artificial water model:
visreg(S2_water_Lat, "Exact_latitude", scale = "response", partial = TRUE)
summary(S2_water_Lat) #non significant

# Education model:
visreg(S2_education_Lat, "Exact_latitude", scale = "response", partial = TRUE)
summary(S2_education_Lat) # actually significant.... P = 0.0361 *
anova(S2_education_null, S2_education_Lat) # Actually a significant improvement to model fit too..
AIC(S2_education_null, S2_education_Lat)
AICc(S2_education_null, S2_education_Lat) # 
AICc(S2_education_null) - AICc(S2_education_Lat) # Only an AICc improvement by 1.7, within the 2 rule

# agrichemical model:
visreg(S2_agchem_EP, "EnviroPerformance_score", scale = "response", partial = TRUE)
summary(S2_agchem_EP) # marginally insignificant

# Revegetation model:
visreg(S2_revegetation_EP, "EnviroPerformance_score", scale = "response", partial = TRUE)
summary(S2_revegetation_EP) # marginally insignificant




