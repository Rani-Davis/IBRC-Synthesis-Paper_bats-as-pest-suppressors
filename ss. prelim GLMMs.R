

# ==============================================================================
# 7. VISUALISE RAW DATA AND MODEL PREDICTIONS
# ==============================================================================
score_caption_S1 <- "Knowledge pathway scores = Each point represents one study system's (crop x region) mean score across all steps in the knowledge pathway\n (each step contributes 0-3 points)."
score_caption_S2 <- "Intervention score = Each point represents one study system's (crop x region) mean score across all 6 interventions\n(each intervention contibutes 0-3 points) (0 = not tried; 1 = tried, untested; 2 = tried, testing ongoing; 3 = tried and tested)."

#---------------------------------
# Survey 1) Random country effect in survey 1 model S1_null_glmmTMB
#---------------------------------
summary(S1_null_glmmTMB)

# Visualise random effect of country
S1_ranef <- tidy(S1_null_glmmTMB, effects = "ran_vals", conf.int = TRUE) %>%
  filter(group == "Country.clean") %>%
  arrange(estimate) %>%
  mutate(level = factor(level, levels = level))

ggplot(S1_ranef, aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, colour = "grey40") +
  geom_point(size = 2, colour = "#2166AC") +
  labs(
    x = "Country-level deviation from grand mean Knowledge Pathway score",
    y = NULL,
    title = "Model-predicted random effect of Country on Knowledge scores (Survey 1)",
    caption = paste0(
      "Each point shows a country's estimated deviation from the grand mean Knowledge Pathway score\n",
      "(random intercept ± 95% CI, from a mixed model with (1 | Country.clean) and no fixed effects).\n",
      "Countries whose interval crosses the dashed zero line do not differ significantly from the grand mean.\n",
      "Intraclass Correlation Coefficient (ICC) = 0.39: ~39% of total variation in Knowledge Pathway scores is\n",
      "attributable to country, but this is not explained by any of the national covariates tested (Step 3)."
    )
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

#---------------------------------
# Survey 2a) EnviroPerformance_score (for survey 2 interventions) — predicted effect + raw data, separate plots
#---------------------------------
summary(S2_EP_glmmTMB)
S2_EP_pred <- ggpredict(S2_EP_glmmTMB, terms = "EnviroPerformance_score [all]")

S2_raw <- Survey_2_complete %>%
  select(x = EnviroPerformance_score, predicted = MeanScore.allInterventions,
         Country.clean, World.region.clean)

ggplot(as.data.frame(S2_EP_pred), aes(x = x, y = predicted)) +
  geom_jitter(data = S2_raw, aes(x = x, y = predicted),
              alpha = 0.25, size = 1.5, colour = "grey50",width = 0.20, height = 0.05) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  labs(x = "Environmental Performance score",
       y = "Predicted Intervention score (± 95% CI)",
       title = "Model-predicted effect of Environmental Performance on Intervention Scores (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# with country labels:
set.seed(1)  # for reproducibility
S2_raw_jit <- S2_raw %>%
  mutate(x_jit = jitter(x, amount = 0.20),
         y_jit = jitter(predicted, amount = 0.05))

ggplot(as.data.frame(S2_EP_pred), aes(x = x, y = predicted)) +
  geom_point(data = S2_raw_jit, aes(x = x_jit, y = y_jit),
             alpha = 0.25, size = 1.5, colour = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  geom_text_repel(data = S2_raw_jit,
                  aes(x = x_jit, y = y_jit, label = Country.clean),
                  size = 2.5, colour = "grey30",
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  segment.colour = "grey70") +
  labs(x = "Environmental Performance score",
       y = "Predicted Intervention score (± 95% CI)",
       title = "Model-predicted effect of Environmental Performance on Intervention Scores (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# colour points by country:
library(ggrepel)
ggplot(as.data.frame(S2_EP_pred), aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  geom_point(data = S2_raw_jit,
             aes(x = x_jit, y = y_jit, colour = Country.clean),
             alpha = 0.7, size = 2) +
  geom_text_repel(data = S2_raw_jit,
                  aes(x = x_jit, y = y_jit, label = Country.clean, colour = Country.clean),
                  size = 2.5,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  show.legend = FALSE) +
  scale_colour_viridis_d(option = "magma") +
  labs(x = "Environmental Performance score",
       y = "Predicted Intervention score (± 95% CI)",
       title = "Model-predicted effect of Environmental Performance on Intervention Scores (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0))

# colour points by region
ggplot(as.data.frame(S2_EP_pred), aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  geom_point(data = S2_raw_jit,
             aes(x = x_jit, y = y_jit, colour = World.region.clean),
             alpha = 0.7, size = 2) +
  geom_text_repel(data = S2_raw_jit,
                  aes(x = x_jit, y = y_jit, label = Country.clean, colour = World.region.clean),
                  size = 2.5,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  show.legend = FALSE) +
  scale_colour_manual(values = region_colours) +
  labs(x = "Environmental Performance score",
       y = "Predicted Intervention score (± 95% CI)",
       colour = "World region",
       title = "Model-predicted effect of Environmental Performance on Intervention Scores (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(legend.position = "right")+
  theme(plot.caption = element_text(hjust = 0))

# Pull the EP coefficient and p-value from the model summary
S2_EP_coef <- summary(S2_EP_glmmTMB)$coefficients$cond["EnviroPerformance_score", ]
S2_EP_coef
#   Estimate   Std. Error      z value     Pr(>|z|)
#   0.016510     0.007108     2.323015     0.020166   (your actual values)

# Build a label string, rounding for display
S2_EP_label <- sprintf("β = %.4f (SE = %.4f)\np = %.3f",
                       S2_EP_coef["Estimate"],
                       S2_EP_coef["Std. Error"],
                       S2_EP_coef["Pr(>|z|)"])

ggplot(as.data.frame(S2_EP_pred), aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#1a9850") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  geom_point(data = S2_raw_jit,
             aes(x = x_jit, y = y_jit, colour = World.region.clean),
             alpha = 0.7, size = 2) +
  geom_text_repel(data = S2_raw_jit,
                  aes(x = x_jit, y = y_jit, label = Country.clean, colour = World.region.clean),
                  size = 2.5,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  show.legend = FALSE) +
  annotate("text",
           x = -Inf, y = Inf,
           label = S2_EP_label,
           hjust = -0.1, vjust = 1.5,
           size = 3.2, colour = "grey20") +
  scale_colour_manual(values = region_colours) +
  labs(x = "Environmental Performance score",
       y = "Predicted Intervention score (± 95% CI)",
       colour = "World region",
       title = "Model-predicted effect of Environmental Performance on Intervention Scores (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(legend.position = "right")+
  theme(plot.caption = element_text(hjust = 0))


#---------------------------------
# Survey 2B) abs(Exact_latitude) — predicted effect + raw data, separate plots
#---------------------------------
# Latitude is retained in the best model for Survey 2 but is not itself significant
S2_Lat_pred <- ggpredict(S2_EP_glmmTMB, terms = "Exact_latitude [all]")

S2_raw_lat <- Survey_2_complete %>%
  select(x = Exact_latitude, predicted = MeanScore.allInterventions, World.region.clean, Country.clean) %>%
  mutate(x = abs(x))

set.seed(1)
S2_raw_lat_jit <- S2_raw_lat %>%
  mutate(x_jit = jitter(x, amount = 0.20),
         y_jit = jitter(predicted, amount = 0.05))

# Pull the latitude coefficient for annotation
S2_Lat_coef <- summary(S2_EP_glmmTMB)$coefficients$cond["abs(Exact_latitude)", ]
S2_Lat_label <- sprintf("β = %.4f (SE = %.4f)\np = %.3f",
                        S2_Lat_coef["Estimate"],
                        S2_Lat_coef["Std. Error"],
                        S2_Lat_coef["Pr(>|z|)"])

ggplot(as.data.frame(S2_Lat_pred), aes(x = abs(x), y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2166AC") +
  geom_line(colour = "#2166AC", linewidth = 1) +
  geom_point(data = S2_raw_lat_jit,
             aes(x = x_jit, y = y_jit, colour = World.region.clean),
             alpha = 0.7, size = 2) +
  annotate("text",
           x = -Inf, y = Inf,
           label = S2_Lat_label,
           hjust = -0.1, vjust = 1.5,
           size = 3.2, colour = "grey20") +
  scale_colour_manual(values = region_colours) +
  labs(x = "Absolute latitude (°)",
       y = "Predicted Intervention score (± 95% CI)",
       colour = "World region",
       title = "Model-predicted effect of Latitude on Intervention Score (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(legend.position = "right")+
  theme(plot.caption = element_text(hjust = 0))


ggplot(as.data.frame(S2_Lat_pred), aes(x = abs(x), y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2166AC") +
  geom_line(colour = "#2166AC", linewidth = 1) +
  geom_point(data = S2_raw_lat_jit,
             aes(x = x_jit, y = y_jit, colour = Country.clean),
             alpha = 0.7, size = 2) +
  geom_text_repel(data = S2_raw_lat_jit,
                  aes(x = x_jit, y = y_jit, label = Country.clean, colour = Country.clean),
                  size = 2.5,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  show.legend = FALSE) +
  annotate("text",
           x = -Inf, y = Inf,
           label = S2_Lat_label,
           hjust = -0.1, vjust = 1.5,
           size = 3.2, colour = "grey20") +
  scale_colour_viridis_d(option = "plasma") +
  labs(x = "Absolute latitude (°)",
       y = "Predicted Intervention score (± 95% CI)",
       colour = "World region",
       title = "Model-predicted effect of Latitude on Intervention Score (Survey 2)",
       caption = score_caption_S2) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(plot.caption = element_text(hjust = 0))


#---------------------------------
# Visualise random effect of country for Survey 2 Intervention scores
#---------------------------------
# Using S2_null_glmmTMB (not S2_EP_glmmTMB) so the ICC reflects total
# between-country variation unconfounded by fixed-effect covariates
S2_ranef <- tidy(S2_null_glmmTMB, effects = "ran_vals", conf.int = TRUE) %>%
  filter(group == "Country.clean") %>%
  arrange(estimate) %>%
  mutate(level = factor(level, levels = level))

# Calculate ICC from S2_null_glmmTMB variance components (Intraclass Correlation Coefficient)
S2_null_vc <- VarCorr(S2_null_glmmTMB)
S2_country_var <- as.numeric(S2_null_vc$cond$Country.clean)
S2_resid_var <- sigma(S2_null_glmmTMB)^2
S2_ICC <- S2_country_var / (S2_country_var + S2_resid_var)
S2_ICC   # check this value before hardcoding it into the caption below

ggplot(S2_ranef, aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, colour = "grey40") +
  geom_point(size = 2, colour = "#2166AC") +
  labs(
    x = "Country-level deviation from grand mean Intervention score",
    y = NULL,
    title = "Between-country variation in Intervention scores (Survey 2)",   # fixed: was "Survey 1"
    caption = paste0("Random intercepts (BLUPs) ± 95% CI from null model with (1 | Country.clean).\n",
                     "ICC ≈ ", round(S2_ICC, 2), ": between-country variation before covariates are added.")
  ) +
  theme_minimal()



# ==============================================================================
# STEP 8: Model averaging across top candidate models (∆AICc < 2)
# ==============================================================================
# Rationale: when multiple models sit within ~2 AICc units of each other, they are statistically indistinguishable 
# Rather than reporting the single top-ranked model as if it were confirmed, we can average coefficients across the full top-model set, weighted by each model's Akaike weight
library(MuMIn)

# ------------------------------------------------------------------------------
# Survey 1 — Knowledge Pathway scores
# ------------------------------------------------------------------------------
S1_model_list <- list(
  S1_null_glmmTMB, S1_Lat_glmmTMB, S1_GDP_glmmTMB, S1_RD_all_glmmTMB,
  S1_Ag_GDP_glmmTMB, S1_Ag_RD_glmmTMB, S1_EP_glmmTMB,
  S1_EP_AgRD_glmmTMB, S1_GDP_AgRD_glmmTMB)

names(S1_model_list) <- c(
  "null", "Lat", "GDP", "RD_all", "Ag_GDP", "Ag_RD", "EP", "EP_AgRD", "GDP_AgRD")

# model.sel() ranks all candidate models by AICc (small-sample-corrected AIC).
# AICc is preferred over plain AIC
S1_msel <- model.sel(S1_model_list)
S1_msel   # confirm delta values match the earlier plain-AIC comparison table

# Four models fall within delta AICc < 2 of the top model:
#   EP (weight 0.36) — MeanScore.allSteps ~ EnviroPerformance_score + Lat
#   null (weight 0.32) — MeanScore.allSteps ~ 1
#   Lat (weight 0.18) — MeanScore.allSteps ~ Lat
#   Ag_GDP (weight 0.14) — MeanScore.allSteps ~ log10(AgGDP) + Lat
# The near-tie between the top model (EP) and the null model (weights 0.36 vs 0.32) signals substantial uncertainty about whether ANY covariate genuinely belongs in the model

S1_avg <- model.avg(S1_msel, subset = delta < 2)

summary(S1_avg)
# Two coefficient tables are returned:

#   (full average): treats a term's effect as ZERO in any model that doesn't include it, before averaging across all 4 models. 
# This is the correct table to report, because it reflects the null model's near-equal weight (0.32) 

#   (conditional average) — only averages over models that DO include the term (e.g. for EnviroPerformance_score, this is just the single EP model). This inflates
# Not useful for reporting here.

# Interpretation: Full-average results:
# EnviroPerformance_score: p = 0.53  -> not significant
# abs(Exact_latitude): p = 0.86  -> not significant
# log10(AgGDP):  p = 0.75 -> not significant
#
# despite EP topping the AICc ranking, none of the tested covariates show a robust, non-zero effect once model-selection uncertainty is properly accounted for. This is consistent with the raw AIC comparison

confint(S1_avg)
# 95% CIs for all three covariates cross or nearly cross zero
# EnviroPerformance_score:-0.0002 to 0.0385
# abs(Exact_latitude):-0.0180 to 0.0223
# log10(AgGDP): -1.3424 to 0.2488
# This further confirms none of these effects can be distinguished from zero.


# ------------------------------------------------------------------------------
# Survey 2 — Intervention scores
# ------------------------------------------------------------------------------
S2_model_list <- list(
  S2_null_glmmTMB, S2_Lat_glmmTMB, S2_GDP_glmmTMB, S2_RD_all_glmmTMB,
  S2_Ag_GDP_glmmTMB, S2_Ag_RD_glmmTMB, S2_EP_glmmTMB,
  S2_EP_AgRD_glmmTMB, S2_GDP_AgRD_glmmTMB
)
names(S2_model_list) <- c(
  "null", "Lat", "GDP", "RD_all", "Ag_GDP", "Ag_RD", "EP", "EP_AgRD", "GDP_AgRD"
)

S2_msel <- model.sel(S2_model_list)
S2_msel
# Only ONE model (EP) falls within delta AICc < 2 — the next-closest model (EP_AgRD) sits at delta = 2.40, just outside the cutoff. EP's Akaike weight

# Because only one model qualifies, model averaging is not applicable here, just use the best model

summary(S2_EP_glmmTMB)
# MeanScore.allInterventions ~ EnviroPerformance_score + abs(Exact_latitude) + (1 | Country.clean)
# EnviroPerformance_score:p = 0.020  -> significant
# abs(Exact_latitude):p = 0.58 -> not significant

confint(S2_EP_glmmTMB)
# Report this alongside the coefficient estimate above.

# Interpretation: unlike Survey 1, Environmental Performance score is a genuinely well-supported predictor of Intervention scores in Survey 2 —
# both by AICc ranking (clear single best model, weight 0.43) and by the coefficient's own confidence interval and p-value. 
# Latitude was retained in the model structure but does not itself show a significant effect.
