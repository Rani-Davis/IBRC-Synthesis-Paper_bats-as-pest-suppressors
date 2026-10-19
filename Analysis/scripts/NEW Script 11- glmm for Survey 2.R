# Code author: Rani Davis and Nayelli Rivera Villanueva  
# Last updated: 9 Oct 2026 with edits recommended by Diogo


# data 
Survey_2_long_indices_wLatLong_wRichness <- read_csv("Analysis/clean data/Survey 2_scored_with indices_w Lat Long_w Richness.csv") %>% mutate(RowID = row_number())

# ==============================================================================
# SURVEY 2: EP x Score.type (ordinal mixed models)
# ==============================================================================
# 0. PACKAGES AND LABELS (same packages as S1)
# ! Before running, ensure your DHARMa package is version 0.5.0
library(tidyverse); library(ordinal); library(MuMIn); library(car); library(corrplot)
library(MASS, exclude = "select"); library(emmeans); library(parallel)
library(lme4); library(DHARMa); library(ggrepel); library(flextable); library(officer)

landscape <- prop_section(page_size = page_size(orient = "landscape"))

# Artificial Roost is first, so it is the reference level of Score.type
S2_type_order <- c("Artificial Roost", "Agrochemical Reduction", "Revegetation",
                   "Bat Education", "Artificial Water Sources", "Acoustic Bat Lure")
S2_type_labels <- c("Artificial Roost" = "Artificial roost", "Agrochemical Reduction" = "Agrochemical reduction",
                    "Revegetation" = "Revegetation", "Bat Education" = "Bat education",
                    "Artificial Water Sources" = "Artificial water sources", "Acoustic Bat Lure" = "Acoustic bat lure")

S2_predictors <- c("GDP_per_capita", "AgForestryFish_ValueAdded_percentGDP",
                   "AgResearchAndDev_PPP2017", "EnviroPerformance_score",
                   "BatRichness", "Exact_latitude")
S2_check_predictors <- c("log10_BatRich", "log10_AgRD", "log10_GDP",
                         "EnviroPerformance_score", "log10_AgGDP", "abs_lat")   # defined BEFORE use

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
# 1. WRANGLE: one row per respondent x intervention; complete cases for ALL candidates
# ==============================================================================
S2_model_dat <- Survey_2_long_indices_wLatLong_wRichness %>%
  mutate(Score.type_clean = str_replace_all(as.character(Score.type), "\n", " ")) %>%
  filter(!is.na(Country.clean), !is.na(Score), !is.na(Score.type_clean)) %>%
  filter(if_all(all_of(S2_predictors), ~ !is.na(.))) %>%
  mutate(Score_ord        = factor(Score, levels = 0:3, ordered = TRUE),
         Score.type       = factor(Score.type_clean, levels = S2_type_order),
         Country.clean    = droplevels(factor(Country.clean)),
         Respondent.clean = factor(Respondent.clean),
         log10_GDP        = log10(GDP_per_capita),
         log10_AgGDP      = log10(AgForestryFish_ValueAdded_percentGDP),
         log10_AgRD       = log10(AgResearchAndDev_PPP2017),
         log10_BatRich    = log10(BatRichness),
         abs_lat          = abs(Exact_latitude))

stopifnot(!anyNA(S2_model_dat$Score.type))
table(S2_model_dat$Score.type, S2_model_dat$Score)    # near-empty cells? (several types have very few 3s)
n_distinct(S2_model_dat$Country.clean)

# ==============================================================================
# 2. CHECK SCORE DISTRIBUTIONS (informs the link function)
# ==============================================================================
S2_raw_props <- prop.table(table(S2_model_dat$Score_ord))
rbind(`Individual proportion` = round(S2_raw_props, 3),
      `Cumulative proportion` = round(cumsum(S2_raw_props), 3))
round(prop.table(table(S2_model_dat$Score.type, S2_model_dat$Score_ord), margin = 1), 3)

p_S2_scoredist <- ggplot(S2_model_dat, aes(factor(Score), fill = Score.type)) +
  geom_bar(aes(y = after_stat(prop), group = Score.type), colour = "black", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ Score.type, labeller = as_labeller(S2_type_labels)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Survey 2: score distributions by intervention type",
       x = "Ordinal score level", y = "Proportion of responses") +
  theme_minimal(base_size = 13) + theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
p_S2_scoredist
# Most types are left-skewed (mostly 0s), especially Acoustic Bat Lure; Bat Education is not.
# Keep the logit link for consistency with S1, but treat the sparse upper categories with caution.

# ==============================================================================
# 3. COLLINEARITY (same logic as S1; kept here so the script runs alone)
# ==============================================================================
S2_country_tbl <- S2_model_dat %>% group_by(Country.clean) %>%
  summarise(across(all_of(S2_check_predictors), mean), .groups = "drop")
S2_cor_mat <- cor(S2_country_tbl[S2_check_predictors], use = "complete.obs")
corrplot(S2_cor_mat, method = "number", type = "upper", diag = FALSE)

S2_bad_pairs <- which(abs(S2_cor_mat) >= 0.6 & upper.tri(S2_cor_mat), arr.ind = TRUE)
S2_excl <- tibble(a = rownames(S2_cor_mat)[S2_bad_pairs[, 1]], b = colnames(S2_cor_mat)[S2_bad_pairs[, 2]],
                  r = round(S2_cor_mat[S2_bad_pairs], 2))
S2_excl
S2_excl_expr <- parse(text = paste0("!(", S2_excl$a, " && ", S2_excl$b, ")", collapse = " && "))[[1]]

# ==============================================================================
# 4. STANDARDISE PREDICTORS (country-level mean/SD)
# ==============================================================================
S2_model_dat_z <- S2_model_dat
for (v in S2_check_predictors) {
  ct <- tapply(S2_model_dat[[v]], S2_model_dat$Country.clean, mean)
  S2_model_dat_z[[v]] <- (S2_model_dat[[v]] - mean(ct)) / sd(ct)
}

# ==============================================================================
# 5. MODEL SELECTION: two dredges on the same data
# ==============================================================================
options(na.action = "na.fail")

S2_global_main <- clmm(Score_ord ~ log10_BatRich + log10_AgRD + log10_GDP + EnviroPerformance_score +
                         log10_AgGDP + abs_lat + Score.type + (1 | Country.clean) + (1 | Respondent.clean),
                       data = S2_model_dat_z, link = "logit")
S2_dredge_main <- eval(bquote(dredge(S2_global_main, fixed = ~ Score.type, m.lim = c(0, 3),
                                     subset = .(S2_excl_expr))))

S2_global_int <- clmm(Score_ord ~ (log10_BatRich + log10_AgRD + log10_GDP + EnviroPerformance_score +
                                     log10_AgGDP + abs_lat) * Score.type + (1 | Country.clean) + (1 | Respondent.clean),
                      data = S2_model_dat_z, link = "logit")
S2_clu <- makeCluster(max(1, detectCores() - 1))
clusterEvalQ(S2_clu, library(ordinal)); clusterExport(S2_clu, "S2_model_dat_z")
S2_dredge_int <- eval(bquote(
  dredge(S2_global_int, cluster = S2_clu, fixed = ~ Score.type, m.lim = c(0, 5),
         subset = .(S2_excl_expr) &&
           sum(log10_BatRich, log10_AgRD, log10_GDP, EnviroPerformance_score, log10_AgGDP, abs_lat) <= 2 &&
           dc(log10_BatRich, `log10_BatRich:Score.type`) && dc(log10_AgRD, `log10_AgRD:Score.type`) &&
           dc(log10_GDP, `log10_GDP:Score.type`) && dc(EnviroPerformance_score, `EnviroPerformance_score:Score.type`) &&
           dc(log10_AgGDP, `log10_AgGDP:Score.type`) && dc(abs_lat, `abs_lat:Score.type`))))
stopCluster(S2_clu)
options(na.action = "na.omit")
nrow(S2_dredge_main); nrow(S2_dredge_int)
saveRDS(S2_dredge_int, "S2_dredge_int.rds")
head(S2_dredge_int)      # CONFIRM the top model is EP x Score.type before running section 7

# ==============================================================================
# 6. TIDY AND PLOT THE DREDGE
# ==============================================================================
S2_tab <- as.data.frame(S2_dredge_int)
S2_term_cols <- setdiff(names(S2_tab), c("(Intercept)", "Score.type", "df", "logLik", "AICc", "delta", "weight"))
S2_short <- c(log10_BatRich = "BatRich", log10_AgRD = "AgRD", log10_GDP = "GDP",
              EnviroPerformance_score = "EP", log10_AgGDP = "AgGDP", abs_lat = "Lat")
S2_tab$label <- NA_character_; S2_tab$has_int <- FALSE
for (i in seq_len(nrow(S2_tab))) {
  present <- S2_term_cols[!is.na(S2_tab[i, S2_term_cols])]
  is_int  <- grepl(":", present)
  main    <- present[!is_int]
  ints    <- sub(":Score.type", "", present[is_int])
  S2_tab$has_int[i] <- any(is_int)
  S2_tab$label[i]   <- if (length(main) == 0) "Null (Score.type only)" else
    paste(ifelse(main %in% ints, paste0(S2_short[main], " x Score.type"), S2_short[main]), collapse = " + ")
}
S2_tab$structure <- ifelse(S2_tab$has_int, "With interaction", "Main effects only")
S2_tab$weight <- as.numeric(S2_tab$weight)

S2_best_by_structure <- S2_tab %>% group_by(structure) %>% slice_min(AICc, n = 1) %>% ungroup() %>%
  select(structure, label, df, logLik, AICc, delta, weight)
S2_best_by_structure
S2_tab %>% group_by(structure) %>% summarise(n = n(), total_weight = sum(weight))

p_S2_rank <- ggplot(S2_tab, aes(delta, reorder(label, -delta), colour = structure, size = weight)) +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "grey60") + geom_point() +
  scale_colour_manual(values = c("Main effects only" = "grey50", "With interaction" = "purple")) +
  labs(x = "Delta AICc (0 = best)", y = "Model terms", colour = NULL, size = "Akaike\nweight",
       title = "Survey 2: dredge rankings (complete-case data)") + theme_bw(base_size = 9)
p_S2_rank

# ==============================================================================
# 7. REFIT THE TOP MODEL ON ALL AVAILABLE DATA (only EP needs to be non-NA)
# ==============================================================================
S2_full_EP <- Survey_2_long_indices_wLatLong_wRichness %>%
  filter(!is.na(Country.clean), !is.na(Score), !is.na(Score.type), !is.na(EnviroPerformance_score)) %>%
  mutate(Score_ord        = factor(Score, levels = 0:3, ordered = TRUE),
         Score.type       = factor(str_replace_all(as.character(Score.type), "\n", " "), levels = S2_type_order),
         Country.clean    = droplevels(factor(Country.clean)),
         Respondent.clean = factor(Respondent.clean))
S2_EP_by_ctry <- tapply(S2_full_EP$EnviroPerformance_score, S2_full_EP$Country.clean, mean)
S2_EP_mu <- mean(S2_EP_by_ctry); S2_EP_sd <- sd(S2_EP_by_ctry)
S2_full_EP$EP_z <- (S2_full_EP$EnviroPerformance_score - S2_EP_mu) / S2_EP_sd

c(rows_dredge = nrow(S2_model_dat_z), rows_full = nrow(S2_full_EP),
  countries_dredge = n_distinct(S2_model_dat_z$Country.clean), countries_full = n_distinct(S2_full_EP$Country.clean))

S2_m_null <- clmm(Score_ord ~ Score.type + (1 | Country.clean) + (1 | Respondent.clean), data = S2_full_EP, link = "logit")
S2_m_main <- clmm(Score_ord ~ EP_z + Score.type + (1 | Country.clean) + (1 | Respondent.clean), data = S2_full_EP, link = "logit")
S2_m_int  <- clmm(Score_ord ~ EP_z * Score.type + (1 | Country.clean) + (1 | Respondent.clean), data = S2_full_EP, link = "logit")

# ==============================================================================
# 8. DOES EP MATTER, AND DOES IT DIFFER BY INTERVENTION? (likelihood ratio tests)
# ==============================================================================
S2_lrt_main <- as.data.frame(anova(S2_m_null, S2_m_main))
S2_lrt_int  <- as.data.frame(anova(S2_m_main, S2_m_int))
S2_lrt_any  <- as.data.frame(anova(S2_m_null, S2_m_int))     # was a duplicate of the line above in your script
AICc(S2_m_main, S2_m_int)

# ==============================================================================
# 9. MODEL SUMMARY (reference level = Artificial Roost, so the EP_z row is that type's slope only)
# ==============================================================================
summary(S2_m_int)

# ==============================================================================
# 10. POST-HOCS (EP slope = log-odds of a higher score per +1 SD of EP)
# ==============================================================================
S2_m_int_avg <- emtrends(S2_m_int, ~ 1, var = "EP_z")
summary(S2_m_int_avg, infer = TRUE)

S2_m_int_slopes <- emtrends(S2_m_int, ~ Score.type, var = "EP_z")
summary(S2_m_int_slopes, infer = TRUE, adjust = "holm")
test(S2_m_int_slopes, adjust = "holm")
pairs(S2_m_int_slopes, adjust = "tukey")
joint_tests(S2_m_int)


# ==============================================================================
# 11. SURVEY 2 TABLES (Word, landscape)
# ==============================================================================
# Assumes EP x Score.type won the S2 dredge: check head(S2_dredge_int) first.
# Table numbering: 1 = dredge, 2 = AICc of refitted models, 3 = LRTs, 4 = slope per intervention, 5 = slope differences

# ---- Table 1: models within 2 AICc of the best (complete-case data used for the dredge) ----
S2_tab_dredge <- S2_tab %>%
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
S2_tab_aicc <- data.frame(
  Model = c("Score.type only (null)",
            "Score.type + EP",
            "Score.type + EP + EP x Score.type"),
  df   = c(attr(logLik(S2_m_null), "df"), attr(logLik(S2_m_main), "df"), attr(logLik(S2_m_int), "df")),
  AICc = round(c(as.numeric(AICc(S2_m_null)), as.numeric(AICc(S2_m_main)), as.numeric(AICc(S2_m_int))), 1))
S2_tab_aicc$`Delta AICc` <- round(S2_tab_aicc$AICc - min(S2_tab_aicc$AICc), 1)

# ---- Table 3: likelihood ratio tests, with the models compared ----
S2_tab_lrt <- data.frame(
  Question = c("Common EP effect",
               "Effect differs by intervention",
               "Any EP effect"),
  `Models compared` = c("Score.type  vs  EP + Score.type",
                        "EP + Score.type  vs  EP x Score.type",
                        "Score.type  vs  EP x Score.type"),
  LR = round(c(S2_lrt_main$LR.stat[2], S2_lrt_int$LR.stat[2], S2_lrt_any$LR.stat[2]), 2),
  df = c(S2_lrt_main$df[2], S2_lrt_int$df[2], S2_lrt_any$df[2]),
  p  = format.pval(c(S2_lrt_main$`Pr(>Chisq)`[2], S2_lrt_int$`Pr(>Chisq)`[2], S2_lrt_any$`Pr(>Chisq)`[2]),
                   digits = 3, eps = 0.001),
  check.names = FALSE)

# ---- Table 4: EP slope within each intervention (tests slope = 0; Holm across the 6 interventions) ----
S2_tab_slopes <- as.data.frame(summary(S2_m_int_slopes, infer = TRUE, adjust = "holm"))
names(S2_tab_slopes) <- c("Intervention", "Slope of EP effect", "SE", "df",
                          "Lower CI", "Upper CI", "z", "p (Holm)")
S2_tab_slopes$`p (Holm)` <- format.pval(S2_tab_slopes$`p (Holm)`, digits = 3, eps = 0.001)

# ---- Table 5: do slopes differ between interventions? (Tukey, 15 pairs) ----
S2_tab_pw <- as.data.frame(pairs(S2_m_int_slopes, adjust = "tukey"))
names(S2_tab_pw) <- c("Contrast", "Difference in slope", "SE", "df", "z", "p (Tukey)")
S2_tab_pw$`p (Tukey)` <- format.pval(S2_tab_pw$`p (Tukey)`, digits = 3, eps = 0.001)

# ---- Table S1: full model summary (thresholds labelled) ----
S2_tab_model <- as.data.frame(coef(summary(S2_m_int)))
S2_tab_model <- cbind(Term = rownames(S2_tab_model), S2_tab_model)
names(S2_tab_model) <- c("Term", "Estimate", "SE", "z", "p")
S2_tab_model$p    <- format.pval(S2_tab_model$p, digits = 3, eps = 0.001)
S2_tab_model$Term <- ifelse(S2_tab_model$Term %in% names(S2_m_int$alpha),
                            paste("Threshold", S2_tab_model$Term), S2_tab_model$Term)

# ---- Footers (defined once, before the flextables that use them) ----
# Table 1: the dredge used complete cases because dredge needs identical rows in every model
S2_note_dredge <- paste0(
  "Models ranked by AICc on the complete-case dataset (", nrow(S2_model_dat_z), " scores, ",
  n_distinct(S2_model_dat_z$Country.clean), " countries). All models include Score.type, a country random ",
  "intercept and a respondent random intercept. Weights are relative to all ", nrow(S2_dredge_int), " candidate models.")

# Table 2: refit data, so AICc is not comparable with Table 1
S2_note_aicc <- paste0(
  "All models fitted to identical rows (", nrow(S2_full_EP), " scores, ", n_distinct(S2_full_EP$Country.clean),
  " countries); Delta AICc is relative to the best model. These AICc values are not comparable with Table 1 ",
  "(best dredge AICc ", round(min(S2_tab$AICc), 1), ") if the data differ.")

# Table 3: LRTs plus the best-model formula, built from the fitted model
S2_note_lrt <- paste0(
  "EP x Score.type = EP + Score.type + their interaction. All models include country and respondent random ",
  "intercepts. Models refitted on all available data (", nrow(S2_full_EP), " scores, ",
  n_distinct(S2_full_EP$Country.clean), " countries).")
S2_note_formula <- paste0(
  "Best model: ", gsub("\\s+", " ", paste(deparse(formula(S2_m_int)), collapse = " ")),
  ". Score_ord = ordinal score (0-3); EP_z = Environmental Performance score standardised across countries; ",
  "(1 | ...) = random intercepts. Cumulative link mixed model (ordinal::clmm), logit link.")

# Table 4: what the test is
S2_note_slopes <- paste0(
  "Each row tests whether the EP slope for that intervention differs from zero. Slopes are log-odds of a higher score per ",
  "+1 SD of Environmental Performance score, from the refitted model (", n_distinct(S2_full_EP$Country.clean),
  " countries). CIs and p-values are Holm-adjusted across the six interventions.")

# ---- Build flextables (each built once) ----
S2_ft_dredge <- flextable(S2_tab_dredge) |> theme_booktabs() |> add_footer_lines(S2_note_dredge) |> autofit()
S2_ft_aicc   <- flextable(S2_tab_aicc)   |> theme_booktabs() |> add_footer_lines(S2_note_aicc) |> autofit()
S2_ft_lrt    <- flextable(S2_tab_lrt)    |> theme_booktabs() |> add_footer_lines(S2_note_lrt) |>
  add_footer_lines(S2_note_formula) |> autofit()
S2_ft_slopes <- flextable(S2_tab_slopes) |> colformat_double(digits = 2) |> theme_booktabs() |>
  add_footer_lines(S2_note_slopes) |> autofit()
S2_ft_pw     <- flextable(S2_tab_pw)     |> colformat_double(digits = 2) |> theme_booktabs() |>
  add_footer_lines("Pairwise differences in EP slope between interventions, Tukey-adjusted (refitted model).") |> autofit()
S2_ft_model  <- flextable(S2_tab_model)  |> colformat_double(digits = 2) |> theme_booktabs() |> autofit()

# ---- Save (one call only: a second save_as_docx to the same path would overwrite this file) ----
save_as_docx("Table 1. Survey 2 top-ranked models from the dredge (complete-case data; delta AICc < 2)" = S2_ft_dredge,
             "Table 2. Survey 2 AICc of refitted models (models refitted on all available data)" = S2_ft_aicc,
             "Table 3. Survey 2 likelihood ratio tests (models refitted on all available data)" = S2_ft_lrt,
             "Table 4. Survey 2 EP slope per intervention (refitted model)" = S2_ft_slopes,
             "Table 5. Survey 2 EP slope comparisons between interventions (refitted model)" = S2_ft_pw,
             "Table S1. Survey 2 full model summary (refitted model)" = S2_ft_model,
             path = "Survey 2_best model_EP_summary and post hocs.docx", pr_section = landscape)
               

# ==============================================================================
# 12. DIAGNOSTICS (simulate scores from the fitted clmm; DHARMa cannot read it)
# ==============================================================================
set.seed(123) 

S2_eta <- drop(model.matrix(~ EP_z * Score.type, S2_full_EP)[, names(S2_m_int$beta)] %*% S2_m_int$beta) +
  unlist(ranef(S2_m_int)$Country.clean)[as.integer(S2_full_EP$Country.clean)] +
  unlist(ranef(S2_m_int)$Respondent.clean)[as.integer(S2_full_EP$Respondent.clean)]
S2_cum <- plogis(matrix(S2_m_int$alpha, nrow(S2_full_EP), 3, byrow = TRUE) - S2_eta)   # uses S2_eta, not S1's eta
S2_u   <- matrix(runif(nrow(S2_full_EP) * 250), nrow = nrow(S2_full_EP))
S2_dh  <- createDHARMa(simulatedResponse = (S2_u > S2_cum[, 1]) + (S2_u > S2_cum[, 2]) + (S2_u > S2_cum[, 3]),
                       observedResponse = as.numeric(S2_full_EP$Score),
                       fittedPredictedResponse = rowSums(1 - S2_cum), integerResponse = TRUE)

plot(S2_dh)
par(mfrow = c(1, 2)); testDispersion(S2_dh); testOutliers(S2_dh)
par(mfrow = c(3, 1))
testCategorical(S2_dh, S2_full_EP$Score.type)
testCategorical(S2_dh, S2_full_EP$Country.clean)
testCategorical(S2_dh, S2_full_EP$Respondent.clean)
par(mfrow = c(1, 1))
nominal_test(clm(Score_ord ~ EP_z + Score.type, data = S2_full_EP))

# ==============================================================================
# 13. FIGURES
# ==============================================================================
S2_line <- as.data.frame(emmeans(S2_m_int, ~ EP_z | Score.type,
                                 at = list(EP_z = seq(min(S2_full_EP$EP_z), max(S2_full_EP$EP_z), length.out = 40)),
                                 mode = "mean.class"))
S2_line$fit <- S2_line$mean.class - 1; S2_line$lwr <- S2_line$asymp.LCL - 1; S2_line$upr <- S2_line$asymp.UCL - 1
S2_line$x_raw <- S2_line$EP_z * S2_EP_sd + S2_EP_mu          # back to raw EP score (linear, no log)

S2_holm <- as.data.frame(test(S2_m_int_slopes, adjust = "holm"))
S2_holm$label <- paste0("slope = ", sprintf("%.2f", S2_holm$EP_z.trend),
                        "\np (Holm) = ", format.pval(S2_holm$p.value, digits = 2, eps = 0.001))

# Fig 1: overlay, lines only, slope + p at line ends
S2_line_end <- S2_line %>% group_by(Score.type) %>% slice_max(x_raw, n = 1) %>% ungroup() %>%
  left_join(S2_holm[, c("Score.type", "label")], by = "Score.type")

p_S2_overlay <- ggplot(S2_line, aes(x_raw, fit, colour = Score.type, fill = Score.type)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.3) +
  geom_text_repel(data = S2_line_end, aes(label = label), hjust = 0, direction = "y", nudge_x = 2,
                  size = 3, lineheight = 0.9, segment.size = 0.2, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(20, 80, 10), expand = expansion(mult = c(0.02, 0.3))) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_brewer(palette = "Dark2", name = "Intervention", labels = S2_type_labels) +
  scale_fill_brewer(palette = "Dark2", name = "Intervention", labels = S2_type_labels) +
  labs(x = "Environmental Performance Index score", y = "Model-predicted score for testing of interventions (0-3)") +
  theme_bw(base_size = 12)
p_S2_overlay
ggsave("Fig_S2_EP_overlay.png", p_S2_overlay, width = 10, height = 5, dpi = 300)

# Fig 2: facets with raw points coloured by region, country labels, p per panel
set.seed(1)
S2_pts <- S2_full_EP %>%
  mutate(x_raw = (EP_z + runif(n(), -0.05, 0.05)) * S2_EP_sd + S2_EP_mu,
         y_jit = pmin(pmax(Score + runif(n(), -0.12, 0.12), 0), 3)) %>%
  group_by(World.region.clean) %>% mutate(n_region = n_distinct(Respondent.entry.label)) %>% ungroup() %>%
  mutate(Region.label = paste0(World.region.clean, " (n=", n_region, ")"))
S2_region_tab  <- S2_pts %>% distinct(World.region.clean, Region.label) %>% mutate(colour = region_colours[World.region.clean])
S2_region_cols <- setNames(S2_region_tab$colour, S2_region_tab$Region.label)

p_S2_facet <- ggplot(S2_line, aes(x_raw, fit)) +
  geom_point(data = S2_pts, aes(x_raw, y_jit, colour = Region.label), alpha = 0.7, size = 1.8) +
  geom_text_repel(data = S2_pts, aes(x_raw, y_jit, label = Country.clean, colour = Region.label),
                  size = 2.1, show.legend = FALSE, max.overlaps = Inf, min.segment.length = 0,
                  segment.size = 0.2, box.padding = 0.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15) + geom_line(linewidth = 1) +
  geom_text(data = S2_holm, aes(x = Inf, y = 3, label = label), hjust = 1.05, vjust = 1.1, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ Score.type, labeller = as_labeller(S2_type_labels), nrow = 2) +
  scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
  scale_colour_manual(values = S2_region_cols) +
  labs(x = "Environmental Performance Index score", y = "Score (0-3)", colour = "World region") +
  theme_bw(base_size = 12)
p_S2_facet
ggsave("Fig_S2_EP_facet.png", p_S2_facet, width = 15, height = 7, dpi = 300)

