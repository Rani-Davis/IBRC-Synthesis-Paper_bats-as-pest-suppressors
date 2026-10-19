# Code author: Rani Davis and Nayelli Rivera Villanueva  
# Last updated: 9 Oct 2026 with edits recommended by Diogo

# data 
Survey_1_long_indices_wLatLong_wRichness <- read_csv("Analysis/clean data/Survey 1_scored_with indices_w Lat Long_w Richness.csv") %>% mutate(RowID = row_number())


# ==============================================================================
# SURVEY 1: AgGDP x Score.type (ordinal mixed models)
# ==============================================================================
# 0. PACKAGES AND LABELS
# ! Before running, ensure your DHARMa package is version 0.5.0
library(tidyverse); library(ordinal); library(MuMIn); library(car); library(corrplot)
library(MASS, exclude = "select"); library(emmeans); library(parallel)
library(lme4); library(DHARMa); library(ggrepel); library(flextable); library(officer)

landscape <- prop_section(page_size = page_size(orient = "landscape"))

# Evidence is first, so it is the reference level of Score.type
S1_type_order <- c("Evidence", "Representativeness", "Limiting Factors",
                   "Available Interventions", "Monitoring Interventions",
                   "Breadth of Implementation of Interventions")
# Display names: the names (left) must equal the factor levels exactly
S1_type_labels <- c("Evidence" = "Evidence", "Representativeness" = "Representativeness",
                    "Limiting Factors" = "Limiting factors",
                    "Available Interventions" = "Available interventions",
                    "Monitoring Interventions" = "Monitoring",
                    "Breadth of Implementation of Interventions" = "Breadth of implementation")

S1_predictors <- c("GDP_per_capita", "AgForestryFish_ValueAdded_percentGDP",
                   "AgResearchAndDev_PPP2017", "EnviroPerformance_score",
                   "BatRichness", "Exact_latitude")
S1_check_predictors <- c("log10_BatRich", "log10_AgRD", "log10_GDP",
                         "EnviroPerformance_score", "log10_AgGDP", "abs_lat")

# Palette
region_colours <- c(
"Europe"                     = "#4F86F7",
"Africa"                     = "#C62828",
"Latin America / Caribbean"  = "#1B9E77",
"Australasia / Pacific"      = "#00B4D8",
"North America"              = "#7B1FA2",
"Middle East / Western Asia" = "#D95F02",
"South / Southeast Asia"     = "#E7298A",
"East Asia"                  = "#E6AB02"
)


# ==============================================================================
# 1. WRANGLE: one row per respondent x score type; complete cases for ALL candidates
#    (dredge needs identical rows in every model)
# ==============================================================================
S1_model_dat <- Survey_1_long_indices_wLatLong_wRichness %>%
  filter(!is.na(Country.clean), !is.na(Score), !is.na(Score.type)) %>%
  filter(if_all(all_of(S1_predictors), ~ !is.na(.))) %>%
  mutate(Score_ord        = factor(Score, levels = 0:3, ordered = TRUE),
         Score.type       = factor(str_replace_all(as.character(Score.type), "\n", " "), levels = S1_type_order),
         Country.clean    = droplevels(factor(Country.clean)),
         Respondent.clean = factor(Respondent.clean),
         log10_GDP        = log10(GDP_per_capita),
         log10_AgGDP      = log10(AgForestryFish_ValueAdded_percentGDP),
         log10_AgRD       = log10(AgResearchAndDev_PPP2017),
         log10_BatRich    = log10(BatRichness),
         abs_lat          = abs(Exact_latitude))

stopifnot(!anyNA(S1_model_dat$Score.type))      # catches level-name mismatches
table(S1_model_dat$Score.type, S1_model_dat$Score)   # near-empty cells?
n_distinct(S1_model_dat$Country.clean)               # countries in the dredge data


# ==============================================================================
# 2. CHECK SCORE DISTRIBUTIONS (informs the link function)
# ==============================================================================
# Overall: individual and cumulative proportions
S1_raw_props <- prop.table(table(S1_model_dat$Score_ord))
rbind(`Individual proportion` = round(S1_raw_props, 3),
      `Cumulative proportion` = round(cumsum(S1_raw_props), 3))

# By score type: does the skew change between steps?
round(prop.table(table(S1_model_dat$Score.type, S1_model_dat$Score_ord), margin = 1), 3)

p_S1_scoredist <- ggplot(S1_model_dat, aes(factor(Score), fill = Score.type)) +
  geom_bar(aes(y = after_stat(prop), group = Score.type), colour = "black", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ Score.type, labeller = as_labeller(S1_type_labels)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Survey 1: score distributions by step in pathway",
       x = "Ordinal score level", y = "Proportion of responses") +
  theme_minimal(base_size = 13) + theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
p_S1_scoredist
# Decision: distributions are broadly balanced and unimodal in the middle, so keep the logit link.


# ==============================================================================
# 3. COLLINEARITY BETWEEN COUNTRY-LEVEL PREDICTORS (across countries, not rows)
# ==============================================================================
S1_country_tbl <- S1_model_dat %>% group_by(Country.clean) %>%
  summarise(across(all_of(S1_check_predictors), mean), .groups = "drop")
S1_cor_mat <- cor(S1_country_tbl[S1_check_predictors], use = "complete.obs")
corrplot(S1_cor_mat, method = "number", type = "upper", diag = FALSE)

# Pairs with |r| >= 0.6 can never share a model; S1_excl_expr feeds the dredge
S1_bad_pairs <- which(abs(S1_cor_mat) >= 0.6 & upper.tri(S1_cor_mat), arr.ind = TRUE)
S1_excl <- tibble(a = rownames(S1_cor_mat)[S1_bad_pairs[, 1]], b = colnames(S1_cor_mat)[S1_bad_pairs[, 2]],
                  r = round(S1_cor_mat[S1_bad_pairs], 2))
S1_excl   # GDP, EP, AgGDP and latitude form one cluster: at most one per model
S1_excl_expr <- parse(text = paste0("!(", S1_excl$a, " && ", S1_excl$b, ")", collapse = " && "))[[1]]

# Descriptive only (NOT used to select predictors)
S1_country_score <- S1_model_dat %>% group_by(Country.clean, Score.type) %>%
  summarise(mean_score = mean(Score), n_resp = n_distinct(Respondent.clean),
            across(all_of(S1_check_predictors), first), .groups = "drop")
p_S1_bytype <- S1_country_score %>%
  pivot_longer(all_of(S1_check_predictors), names_to = "predictor", values_to = "x") %>%
  ggplot(aes(x, mean_score)) +
  geom_point(aes(size = n_resp), alpha = 0.6, colour = "purple") +
  geom_smooth(method = "lm", colour = "black", linewidth = 0.6, se = FALSE) +
  facet_grid(Score.type ~ predictor, scales = "free_x") +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = NULL, y = "Country mean score (0-3)", size = "Respondents") + theme_minimal(base_size = 9)


# ==============================================================================
# 4. STANDARDISE PREDICTORS (z-score using mean/SD across countries)
# ==============================================================================
S1_model_dat_z <- S1_model_dat
for (v in S1_check_predictors) {          # one of only two loops in this script
  ct <- tapply(S1_model_dat[[v]], S1_model_dat$Country.clean, mean)
  S1_model_dat_z[[v]] <- (S1_model_dat[[v]] - mean(ct)) / sd(ct)
}

# ==============================================================================
# 5. MODEL SELECTION: two dredges on the same data
#    Score.type is fixed in every model; random effects are always kept.
# ==============================================================================
options(na.action = "na.fail")    # required by dredge

# (A) Main effects only. m.lim counts Score.type: 3 = Score.type + up to 2 predictors
S1_global_main <- clmm(Score_ord ~ log10_BatRich + log10_AgRD + log10_GDP + EnviroPerformance_score +
                         log10_AgGDP + abs_lat + Score.type + (1 | Country.clean) + (1 | Respondent.clean),
                       data = S1_model_dat_z, link = "logit")
S1_dredge_main <- eval(bquote(dredge(S1_global_main, fixed = ~ Score.type, m.lim = c(0, 3),
                                     subset = .(S1_excl_expr))))

# (B) With predictor x Score.type interactions (allowed only if the main effect is present)
S1_global_int <- clmm(Score_ord ~ (log10_BatRich + log10_AgRD + log10_GDP + EnviroPerformance_score +
                                     log10_AgGDP + abs_lat) * Score.type + (1 | Country.clean) + (1 | Respondent.clean),
                      data = S1_model_dat_z, link = "logit")
S1_clu <- makeCluster(max(1, detectCores() - 1))
clusterEvalQ(S1_clu, library(ordinal)); clusterExport(S1_clu, "S1_model_dat_z")
S1_dredge_int <- eval(bquote(
  dredge(S1_global_int, cluster = S1_clu, fixed = ~ Score.type,
         m.lim  = c(0, 5),    # Score.type + 2 predictors + their 2 interactions
         subset = .(S1_excl_expr) &&
           sum(log10_BatRich, log10_AgRD, log10_GDP, EnviroPerformance_score, log10_AgGDP, abs_lat) <= 2 &&
           dc(log10_BatRich, `log10_BatRich:Score.type`) && dc(log10_AgRD, `log10_AgRD:Score.type`) &&
           dc(log10_GDP, `log10_GDP:Score.type`) && dc(EnviroPerformance_score, `EnviroPerformance_score:Score.type`) &&
           dc(log10_AgGDP, `log10_AgGDP:Score.type`) && dc(abs_lat, `abs_lat:Score.type`))))
stopCluster(S1_clu)
options(na.action = "na.omit")    # switch na.fail off for normal fitting
nrow(S1_dredge_main); nrow(S1_dredge_int)   # expect 16 and 49
saveRDS(S1_dredge_int, "S1_dredge_int.rds")


# ==============================================================================
# 6. TIDY AND PLOT THE DREDGE (the 49 models include all 16 main-effects models)
# ==============================================================================
S1_tab <- as.data.frame(S1_dredge_int)
S1_term_cols <- setdiff(names(S1_tab), c("(Intercept)", "Score.type", "df", "logLik", "AICc", "delta", "weight"))
S1_short <- c(log10_BatRich = "BatRich", log10_AgRD = "AgRD", log10_GDP = "GDP",
              EnviroPerformance_score = "EP", log10_AgGDP = "AgGDP", abs_lat = "Lat")
S1_tab$label <- NA_character_; S1_tab$has_int <- FALSE
for (i in seq_len(nrow(S1_tab))) {        # second loop: builds readable model labels
  present <- S1_term_cols[!is.na(S1_tab[i, S1_term_cols])]
  is_int  <- grepl(":", present)
  main    <- present[!is_int]
  ints    <- sub(":Score.type", "", present[is_int])
  S1_tab$has_int[i] <- any(is_int)
  S1_tab$label[i]   <- if (length(main) == 0) "Null (Score.type only)" else
    paste(ifelse(main %in% ints, paste0(S1_short[main], " x Score.type"), S1_short[main]), collapse = " + ")
}
S1_tab$structure <- ifelse(S1_tab$has_int, "With interaction", "Main effects only")
S1_tab$weight <- as.numeric(S1_tab$weight)

S1_best_by_structure <- S1_tab %>% group_by(structure) %>% slice_min(AICc, n = 1) %>% ungroup() %>%
  select(structure, label, df, logLik, AICc, delta, weight)
S1_best_by_structure
S1_tab %>% group_by(structure) %>% summarise(n = n(), total_weight = sum(weight))

p_S1_rank <- ggplot(S1_tab, aes(delta, reorder(label, -delta), colour = structure, size = weight)) +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "grey60") + geom_point() +
  scale_colour_manual(values = c("Main effects only" = "grey50", "With interaction" = "purple")) +
  labs(x = "Delta AICc (0 = best)", y = "Model terms", colour = NULL, size = "Akaike\nweight",
       title = "Survey 1: dredge rankings (complete-case data)") + theme_bw(base_size = 9)
p_S1_rank


# ==============================================================================
# 7. REFIT THE TOP MODEL ON ALL AVAILABLE DATA (only AgGDP needs to be non-NA)
# ==============================================================================
S1_full_Ag <- Survey_1_long_indices_wLatLong_wRichness %>%
  filter(!is.na(Country.clean), !is.na(Score), !is.na(Score.type),
         !is.na(AgForestryFish_ValueAdded_percentGDP)) %>%
  mutate(Score_ord        = factor(Score, levels = 0:3, ordered = TRUE),
         Score.type       = factor(str_replace_all(as.character(Score.type), "\n", " "), levels = S1_type_order),
         Country.clean    = droplevels(factor(Country.clean)),
         Respondent.clean = factor(Respondent.clean),
         log10_AgGDP      = log10(AgForestryFish_ValueAdded_percentGDP))
S1_Ag_by_ctry <- tapply(S1_full_Ag$log10_AgGDP, S1_full_Ag$Country.clean, mean)
S1_Ag_mu <- mean(S1_Ag_by_ctry); S1_Ag_sd <- sd(S1_Ag_by_ctry)       # kept to back-transform plots
S1_full_Ag$log10_AgGDP_z <- (S1_full_Ag$log10_AgGDP - S1_Ag_mu) / S1_Ag_sd

c(rows_dredge = nrow(S1_model_dat_z), rows_full = nrow(S1_full_Ag),
  countries_dredge = n_distinct(S1_model_dat_z$Country.clean), countries_full = n_distinct(S1_full_Ag$Country.clean))

S1_m_null <- clmm(Score_ord ~ Score.type + (1 | Country.clean) + (1 | Respondent.clean), data = S1_full_Ag, link = "logit")
S1_m_main <- clmm(Score_ord ~ log10_AgGDP_z + Score.type + (1 | Country.clean) + (1 | Respondent.clean), data = S1_full_Ag, link = "logit")
S1_m_int  <- clmm(Score_ord ~ log10_AgGDP_z * Score.type + (1 | Country.clean) + (1 | Respondent.clean), data = S1_full_Ag, link = "logit")


# ==============================================================================
# 8. DOES AgGDP MATTER, AND DOES IT DIFFER BY SCORE TYPE? (likelihood ratio tests)
# ==============================================================================
S1_lrt_main <- as.data.frame(anova(S1_m_null, S1_m_main))   # common AgGDP slope
S1_lrt_main
S1_lrt_int  <- as.data.frame(anova(S1_m_main, S1_m_int))    # do slopes differ between types
S1_lrt_int
S1_lrt_any  <- as.data.frame(anova(S1_m_null, S1_m_int))    # any AgGDP effect
S1_lrt_any 
AICc(S1_m_main, S1_m_int) # The interaction model is better

# Does bat richness add anything? Needs identical rows, so refit both on BatRich-complete data
S1_full_Ag_Rich <- S1_full_Ag %>% filter(!is.na(BatRichness)) %>%
  mutate(Country.clean = droplevels(Country.clean), log10_BatRich = log10(BatRichness))
S1_Ag_r  <- tapply(S1_full_Ag_Rich$log10_AgGDP,   S1_full_Ag_Rich$Country.clean, mean)
S1_Bat_r <- tapply(S1_full_Ag_Rich$log10_BatRich, S1_full_Ag_Rich$Country.clean, mean)
S1_full_Ag_Rich$log10_AgGDP_z   <- (S1_full_Ag_Rich$log10_AgGDP   - mean(S1_Ag_r))  / sd(S1_Ag_r)
S1_full_Ag_Rich$log10_BatRich_z <- (S1_full_Ag_Rich$log10_BatRich - mean(S1_Bat_r)) / sd(S1_Bat_r)
S1_m_int_r   <- clmm(Score_ord ~ log10_AgGDP_z * Score.type + (1 | Country.clean) + (1 | Respondent.clean),
                     data = S1_full_Ag_Rich, link = "logit")
S1_m_int_Bat <- clmm(Score_ord ~ log10_AgGDP_z * Score.type + log10_BatRich_z + (1 | Country.clean) + (1 | Respondent.clean),
                     data = S1_full_Ag_Rich, link = "logit")
anova(S1_m_int_r, S1_m_int_Bat) # No, adding bat richness does not sig improve the model
AICc(S1_m_int_r, S1_m_int_Bat) # and the AICc is still worse when refitting models with the more complete dataset

# Continue with this model =S1_m_int


# ==============================================================================
# 9. MODEL SUMMARY
# ==============================================================================
summary(S1_m_int)    
# check max.grad < 1e-3 and cond.H not huge. 
# Reference level = Evidence, so the log10_AgGDP_z row is the EVIDENCE slope only (effect of GDP on Evidence score):
# use section 10 for per-type slopes


# ==============================================================================
# 10. POST-HOCS (AgGDP slope = log-odds of a higher score per +1 SD of log10 AgGDP)
# ==============================================================================
S1_m_int_avg <- emtrends(S1_m_int, ~ 1, var = "log10_AgGDP_z")
summary(S1_m_int_avg, infer = TRUE)                         # average slope across the 6 types is sig

S1_m_int_slopes <- emtrends(S1_m_int, ~ Score.type, var = "log10_AgGDP_z")
summary(S1_m_int_slopes, infer = TRUE, adjust = "holm")     # which types are affected (Holm, 6 tests)
test(S1_m_int_slopes, adjust = "holm") # all sig slopes expect evidence
pairs(S1_m_int_slopes, adjust = "tukey")                    # which slopes differ from eachother (15 pairs)
joint_tests(S1_m_int)                                       # omnibus Wald tests per term


# ==============================================================================
# 11. SIMPLE TABLES (Word, landscape)
# ==============================================================================
# Table numbering: 1 = dredge (model selection), 2 = LRTs, 3 = slope per step, 4 = slope differences

# ---- Table 1: models within 2 AICc of the best (complete-case data used for the dredge) ----
S1_tab_dredge <- S1_tab %>%
  arrange(AICc) %>%
  filter(delta < 2) %>%
  transmute(`Model terms`   = paste0("Score.type + ", label),
            Structure       = structure,
            df              = df,
            logLik          = round(logLik, 1),
            AICc            = round(AICc, 1),
            `Delta AICc`    = round(delta, 2),
            `Akaike weight` = round(weight, 3))

# ---- Table 2: AICc of each refitted model (same rows, so directly comparable) ----
S1_tab_aicc <- data.frame(
  Model = c("Score.type only (null)",
            "Score.type + AgGDP",
            "Score.type + AgGDP + AgGDP x Score.type"),
  df   = c(attr(logLik(S1_m_null), "df"), attr(logLik(S1_m_main), "df"), attr(logLik(S1_m_int), "df")),
  AICc = round(c(as.numeric(AICc(S1_m_null)), as.numeric(AICc(S1_m_main)), as.numeric(AICc(S1_m_int))), 1))
S1_tab_aicc$`Delta AICc` <- round(S1_tab_aicc$AICc - min(S1_tab_aicc$AICc), 1)
names(S1_tab_aicc)[3] <- "AICc"

# ---- Table 3: likelihood ratio tests (narrow) ----
S1_tab_lrt <- data.frame(
  Question = c("Common AgGDP effect",
               "Effect differs by score type",
               "Any AgGDP effect"),
  `Models compared` = c("Score.type  vs  AgGDP + Score.type",
                        "AgGDP + Score.type  vs  AgGDP x Score.type",
                        "Score.type  vs  AgGDP x Score.type"),
  LR = round(c(S1_lrt_main$LR.stat[2], S1_lrt_int$LR.stat[2], S1_lrt_any$LR.stat[2]), 2),
  df = c(S1_lrt_main$df[2], S1_lrt_int$df[2], S1_lrt_any$df[2]),
  p  = format.pval(c(S1_lrt_main$`Pr(>Chisq)`[2], S1_lrt_int$`Pr(>Chisq)`[2], S1_lrt_any$`Pr(>Chisq)`[2]),
                   digits = 3, eps = 0.001),
  check.names = FALSE)

# ---- Table 4: AgGDP slope within each score type (tests slope = 0; Holm across the 6 steps) ----
S1_tab_slopes <- as.data.frame(summary(S1_m_int_slopes, infer = TRUE, adjust = "holm"))
names(S1_tab_slopes) <- c("Step in knowledge pathway", "Slope of AgGDP effect", "SE", "df",
                          "Lower CI", "Upper CI", "z", "p (Holm)")
S1_tab_slopes$`p (Holm)` <- format.pval(S1_tab_slopes$`p (Holm)`, digits = 3, eps = 0.001)

# ---- Table 5: do slopes differ between steps? (Tukey, 15 pairs) ----
S1_tab_pw <- as.data.frame(pairs(S1_m_int_slopes, adjust = "tukey"))
names(S1_tab_pw) <- c("Contrast", "Difference in slope", "SE", "df", "z", "p (Tukey)")
S1_tab_pw$`p (Tukey)` <- format.pval(S1_tab_pw$`p (Tukey)`, digits = 3, eps = 0.001)

# ---- Table S1: full model summary (thresholds labelled) ----
S1_tab_model <- as.data.frame(coef(summary(S1_m_int)))
S1_tab_model <- cbind(Term = rownames(S1_tab_model), S1_tab_model)
names(S1_tab_model) <- c("Term", "Estimate", "SE", "z", "p")
S1_tab_model$p    <- format.pval(S1_tab_model$p, digits = 3, eps = 0.001)
S1_tab_model$Term <- ifelse(S1_tab_model$Term %in% names(S1_m_int$alpha),
                            paste("Threshold", S1_tab_model$Term), S1_tab_model$Term)

# ---- Footer text ----
# Table 1: the dredge used complete cases because dredge needs identical rows in every model
S1_note_dredge <- paste0(
  "Models ranked by AICc on the complete-case dataset (", nrow(S1_model_dat_z), " scores, ",
  n_distinct(S1_model_dat_z$Country.clean), " countries). All models include Score.type, a country random ",
  "intercept and a respondent random intercept. Weights are relative to all ", nrow(S1_dredge_int), " candidate models.")

# Table 2: the refit has more data, so its AICc is not comparable with Table 1
S1_note_aicc <- paste0(
  "All models fitted to identical rows (", nrow(S1_full_Ag), " scores, ", n_distinct(S1_full_Ag$Country.clean),
  " countries); Delta AICc is relative to the best model. These AICc values are not comparable with Table 1 ",
  "(best dredge AICc ", round(min(S1_tab$AICc), 1), "), because the data differ.")

# Table 3: 
S1_note_lrt <- paste0(
  "Likelihood ratio tests: common AgGDP effect = Score.type vs Score.type + AgGDP; effect differs by score type = ",
  "Score.type + AgGDP vs Score.type + AgGDP + AgGDP x Score.type; any AgGDP effect = Score.type vs the full model. ",
  "Models refitted on all available data (", nrow(S1_full_Ag), " scores, ", n_distinct(S1_full_Ag$Country.clean), " countries).")
S1_note_formula <- paste0(
  "Best model: ", gsub("\\s+", " ", paste(deparse(formula(S1_m_int)), collapse = " ")),
  ". Score_ord = ordinal score (0-3); log10_AgGDP_z = log10 % GDP from agriculture, forestry and fishing, ",
  "standardised across countries; (1 | ...) = random intercepts. Cumulative link mixed model (ordinal::clmm), logit link.")


# Table 4: what the test is
S1_note_slopes <- paste0(
  "Each row tests whether the AgGDP slope for that step differs from zero. Slopes are log-odds of a higher score per ",
  "+1 SD of log10 AgGDP (negative = lower scores in more agriculture-dependent countries), from the refitted model (",
  n_distinct(S1_full_Ag$Country.clean), " countries). CIs and p-values are Holm-adjusted across the six steps.")


# ---- Build flextables (each built once, footers added here) ----
S1_ft_dredge <- flextable(S1_tab_dredge) |> theme_booktabs() |> add_footer_lines(S1_note_dredge) |> autofit()
S1_ft_lrt    <- flextable(S1_tab_lrt)    |> theme_booktabs() |> add_footer_lines(S1_note_lrt) |>
  add_footer_lines(S1_note_formula) |> autofit()
S1_ft_slopes <- flextable(S1_tab_slopes) |> colformat_double(digits = 2) |> theme_booktabs() |>
  add_footer_lines(S1_note_slopes) |> autofit()
S1_ft_pw     <- flextable(S1_tab_pw)     |> colformat_double(digits = 2) |> theme_booktabs() |>
  add_footer_lines("Pairwise differences in AgGDP slope between steps, Tukey-adjusted (refitted model).") |> autofit()
S1_ft_model  <- flextable(S1_tab_model)  |> colformat_double(digits = 2) |> theme_booktabs() |> autofit()
S1_ft_aicc <- flextable(S1_tab_aicc) |> theme_booktabs() |> add_footer_lines(S1_note_aicc) |> autofit()

# ---- Save (one call only: a second save_as_docx to the same path would overwrite this file) ----
save_as_docx("Table 1. Survey 1 top-ranked models from the dredge (complete-case data; delta AICc < 2)" = S1_ft_dredge,
             "Table 2. Survey 1 AICc of refitted models (models refitted on all available data)" = S1_ft_aicc,
             "Table 3. Survey 1 likelihood ratio tests (models refitted on all available data)" = S1_ft_lrt,
             "Table 4. Survey 1 AgGDP slope per score type (refitted model)" = S1_ft_slopes,
             "Table 5. Survey 1 AgGDP slope comparisons between score types (refitted model)" = S1_ft_pw,
             "Table S1. Survey 1 full model summary (refitted model)" = S1_ft_model,
             path = "Survey 1_best model_AgGDP_summary and post hocs.docx", pr_section = landscape)


# ==============================================================================
# 12. DIAGNOSTICS (DHARMa cannot read a clmm, so simulate scores from the fitted model)
# ==============================================================================
# SET SEED HERE FOR REPRODUCIBILITY
set.seed(123) 

S1_eta <- drop(model.matrix(~ log10_AgGDP_z * Score.type, S1_full_Ag)[, names(S1_m_int$beta)] %*% S1_m_int$beta) +
  unlist(ranef(S1_m_int)$Country.clean)[as.integer(S1_full_Ag$Country.clean)] +
  unlist(ranef(S1_m_int)$Respondent.clean)[as.integer(S1_full_Ag$Respondent.clean)]
S1_cum <- plogis(matrix(S1_m_int$alpha, nrow(S1_full_Ag), 3, byrow = TRUE) - S1_eta)   # P(score <= 0,1,2)
S1_u   <- matrix(runif(nrow(S1_full_Ag) * 500), nrow = nrow(S1_full_Ag))     # 250 simulations          
S1_dh  <- createDHARMa(simulatedResponse = (S1_u > S1_cum[, 1]) + (S1_u > S1_cum[, 2]) + (S1_u > S1_cum[, 3]),
                       observedResponse = as.numeric(S1_full_Ag$Score),
                       fittedPredictedResponse = rowSums(1 - S1_cum), integerResponse = TRUE)

plot(S1_dh)                                                   # QQ + residuals vs predicted
par(mfrow = c(1, 2)); testDispersion(S1_dh); testOutliers(S1_dh)
par(mfrow = c(3, 1))
testCategorical(S1_dh, S1_full_Ag$Score.type)
testCategorical(S1_dh, S1_full_Ag$Country.clean)
testCategorical(S1_dh, S1_full_Ag$Respondent.clean)
par(mfrow = c(1, 1))
nominal_test(clm(Score_ord ~ log10_AgGDP_z + Score.type, data = S1_full_Ag))   # proportional odds (rough)


# ==============================================================================
# 13. FIGURES
# ==============================================================================
# Fitted lines (mean.class is on a 1-4 scale, so subtract 1 for the 0-3 score)
S1_line <- as.data.frame(emmeans(S1_m_int, ~ log10_AgGDP_z | Score.type,
                                 at = list(log10_AgGDP_z = seq(min(S1_full_Ag$log10_AgGDP_z),
                                                               max(S1_full_Ag$log10_AgGDP_z), length.out = 40)),
                                 mode = "mean.class"))
S1_line$fit <- S1_line$mean.class - 1; S1_line$lwr <- S1_line$asymp.LCL - 1; S1_line$upr <- S1_line$asymp.UCL - 1
S1_line$x_raw <- 10^(S1_line$log10_AgGDP_z * S1_Ag_sd + S1_Ag_mu)       # back to % of GDP

# Holm-adjusted slope per type, for labels
S1_holm <- as.data.frame(test(S1_m_int_slopes, adjust = "holm"))
S1_holm$label <- paste0("slope = ", sprintf("%.2f", S1_holm$log10_AgGDP_z.trend),
                        "\np (Holm) = ", format.pval(S1_holm$p.value, digits = 2, eps = 0.001))

# Fig 1: overlay, lines only, slope + p at line ends
S1_line_end <- S1_line %>% group_by(Score.type) %>% slice_max(x_raw, n = 1) %>% ungroup() %>%
  left_join(S1_holm[, c("Score.type", "label")], by = "Score.type")

p_S1_overlay <- ggplot(S1_line, aes(x_raw, fit, colour = Score.type, fill = Score.type)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.3) +
  geom_text_repel(data = S1_line_end, aes(label = label), hjust = 0, direction = "y", nudge_x = 0.05,
                  size = 3, lineheight = 0.9, segment.size = 0.2, show.legend = FALSE) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 30), expand = expansion(mult = c(0.02, 0.45))) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2", name = "Score type", labels = S1_type_labels) +
  scale_fill_brewer(palette = "Dark2", name = "Score type", labels = S1_type_labels) +
  labs(x = "% of GDP from agriculture, forestry and fishing (log scale)", y = "Model-predicted knowledge score (0-3)") +
  theme_bw(base_size = 12)
p_S1_overlay
ggsave("Fig_S1_AgGDP_overlay.png", p_S1_overlay, width = 10, height = 5, dpi = 300)

# Fig 2: facets with raw points coloured by region, country labels, p per panel
set.seed(1)
S1_pts <- S1_full_Ag %>%
  mutate(x_raw = 10^((log10_AgGDP_z + runif(n(), -0.05, 0.05)) * S1_Ag_sd + S1_Ag_mu),
         y_jit = pmin(pmax(Score + runif(n(), -0.12, 0.12), 0), 3)) %>%
  group_by(World.region.clean) %>% mutate(n_region = n_distinct(Respondent.entry.label)) %>% ungroup() %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))
S1_region_tab  <- S1_pts %>% distinct(World.region.clean, Region.label) %>% mutate(colour = region_colours[World.region.clean])
S1_region_cols <- setNames(S1_region_tab$colour, S1_region_tab$Region.label)

p_S1_facet <- ggplot(S1_line, aes(x_raw, fit)) +
  geom_point(data = S1_pts, aes(x_raw, y_jit, colour = Region.label), alpha = 0.7, size = 1.8) +
  geom_text_repel(data = S1_pts, aes(x_raw, y_jit, label = Country.clean, colour = Region.label),
                  size = 2.1, show.legend = FALSE, max.overlaps = Inf, min.segment.length = 0,
                  segment.size = 0.2, box.padding = 0.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15) + geom_line(linewidth = 1) +
  geom_text(data = S1_holm, aes(x = Inf, y = 3, label = label), hjust = 1.05, vjust = 1.1, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ Score.type, labeller = as_labeller(S1_type_labels), nrow = 2) +
  scale_x_log10() + scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_manual(values = S1_region_cols) +
  labs(x = "% of GDP from agriculture, forestry and fishing (log scale)", y = "Knowledge score (0-3)", colour = "World region") +
  theme_bw(base_size = 12)
p_S1_facet
ggsave("Fig_S1_AgGDP_facet.png", p_S1_facet, width = 15, height = 7, dpi = 300)






