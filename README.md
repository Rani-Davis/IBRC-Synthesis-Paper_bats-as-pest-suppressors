<h1 align="center">🦇 IBRC Synthesis Paper: Bats as Pest Suppressors 🦇</h1>

Our global synthesis paper summarises published and unpublished knowledge on insectivorous bats as pest suppressors in agricultural systems, and the interventions trialled to support them.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

---

## 📋 Overview of Data

Contributors were asked to complete two surveys to gather knowledge about the research conducted in their study systems. A **study system** is defined as a crop type $\times$ region combination in which studies were done.

| Survey | Focus | Purpose |
| :--- | :--- | :--- |
| **#1: Knowledge scoring** | Conceptual pathway | Places each study system within our conceptual "knowledge pathway" (see **Figure 1** below). Scores at each step along the pathway scale from 0 (no knowledge) to 3 (extensive knowledge). |
| **#2: Implementation scoring** | Interventions | Gathers information on what interventions have been implemented and tested for their effects on bats in each study system. Scores for each intervention scale from 0 (intervention not tried) to 3 (intervention tried and evaluated). |

---

## 🧠 Conceptual Knowledge Pathway

<p align="center">
  <img src="figure%20exports/Our%20Conceptual%20Knowledge%20Pathway_with%20questions.png" alt="Our conceptual knowledge pathway" /><br>
  <sub><i>Figure 1: Conceptual knowledge pathway used to score study systems (see Script 0).</i></sub>
</p>

---

# 🗺️ Study System Maps

Explore the global coverage of our study systems using these interactive maps:

* 📍 [Interactive study system map (Pin markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/teardrop%20map_interactive.html)
* 🔵 [Interactive study system map (Circle markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/circle%20map_interactive.html)

---

# 📊 Key Figures

## Knowledge Pathway Scores (Survey 1)

<p align="center">
  <img src="figure%20exports/Knowledge%20pathway%20scores/Draft_composite%20knowledge%20pathway%20plot_By%20Crop%20Type.png" alt="Knowledge pathway results by crop" /><br>
  <sub><i>Figure 2: Knowledge pathway progress across crop types, showing mean scores (± SE) with raw respondent-level data points. Click to enlarge.</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Knowledge%20pathway%20scores/Draft_composite%20knowledge%20pathway%20plot_By%20World%20Region.png" alt="Knowledge pathway results by region" /><br>
  <sub><i>Figure 3: Knowledge pathway progress across regions, showing mean scores (± SE) for each question in Survey 1 with raw respondent-level data points. Click to enlarge.</i></sub>
</p>

---

## Intervention Testing Scores (Survey 2)

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Overall.intervention.testing.stackedbar.jpeg" alt="Implementation results stacked bar" /><br>
  <sub><i>Figure 4: Proportion of study systems that have tested different bat-supportive interventions (from Survey 2). Click to enlarge. </i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Mean.SE.ByCrop.Intervention.testing.jpeg" alt="Mean testing scores by crop" /><br>
  <sub><i>Figure 5: Mean intervention testing scores (± SE) by crop type (from Survey 2). Click to enlarge.</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Mean.SE.ByRegion.Intervention.testing.jpeg" alt="Mean testing scores by region" /><br>
  <sub><i>Figure 6: Mean intervention testing scores (± SE) by world region (from Survey 2). Click to enlarge.</i></sub>
</p>

---

# 📈 Generalised Linear Mixed Models (GLMMs)

## Exploring predictors of Knowledge Pathway and Intervention Testing scores
We are exploring whether variation in Knowledge Pathway scores (Survey 1) and Intervention Testing scores (Survey 2) can be explained by broader **socio-economic, environmental, and agricultural factors**, while also considering important study-system characteristics such as crop type.

Socio-economic and environmental indices explored:

- 🌍 Environmental Performance Index (EP)
- 💰 GDP
- 🌾 Agricultural contribution to GDP (AgGDP)
- 🔬 Agricultural research & development investment

Additional predictors included in models:

- 🌱 Crop type.
- 🦇 Bat species richness - more complex systems with more species may be more challenging to gain knowledge in.
- 🧭 Latitude (joined in Script 8).

Our predictor 'budget':

- 💻 We have responses from 24 countries, and the socio-economic and bat species richness data will be at country level. Therefore, we can only ‘afford’ 2 predictors following the 10 observations per predictor rule (excluding random effects).

Random effects:

- 🌍 (1 | Country) - This accounts for multiple respondents per country sharing the same GDP/Env Performance etc value.
- 👤 (1 | Respondent) - This accounts for multiple responses per person.

## Modelling approach

Each survey was modelled with one row per score (Survey 1: each step in the knowledge pathway; Survey 2: each intervention), with `Score.type` (the step or intervention) as a fixed effect. This lets us ask both whether a country-level predictor is associated with scores overall, and whether that association differs among steps or interventions (a `predictor x Score.type` interaction).

The workflow for each survey was:

1. **Build a complete-case dataset** with no NAs for any candidate predictor.
2. **Run `dredge()`** (MuMIn), using `subset =` to prevent correlated variables being included together and `m.lim =` to cap the number of fixed effects. Models within 2 AICc units of the best model are considered.
3. **Refit the best predictor on the full dataset** (all rows with valid values for that predictor, ignoring NAs in the other indices) and test whether the interaction with `Score.type` is needed.
4. **Post-hoc tests** of the slope of the predictor at each step/intervention (Holm-adjusted across the six comparisons).
5. **Model diagnostics** (quantile residual checks).

> AICc values are only comparable between models fitted to the same dataset. The complete-case (step 2) and full-dataset (step 3) AICc values therefore differ and should not be compared with each other.

---

## Survey 1: Knowledge Pathway score models

**Response:** score at each step in the knowledge pathway (`Score.type`).

### Step 1: Model comparison (complete-case dataset)

Models within 2 AICc units of the best model:

| Model terms | Structure | df | logLik | AICc | Delta AICc | Akaike weight |
| :--- | :--- | ---: | ---: | ---: | ---: | ---: |
| Score.type + AgGDP x Score.type | With interaction | 16 | -389.8 | 813.1 | 0.00 | 0.543 |
| Score.type + AgGDP x Score.type + BatRich | With interaction | 17 | -389.6 | 814.8 | 1.79 | 0.221 |

<!-- TODO: add dredge model comparison figure (grey dashed line = 2 AICc above best) -->

### Step 2: Does bat richness improve the model?

Because bat richness appeared in a model within 2 AICc units, we tested it on the full dataset (no NAs for AgGDP or bat richness; other indices not required).

| Model | df | AICc | Delta AICc |
| :--- | ---: | ---: | ---: |
| Score.type + AgGDP + AgGDP x Score.type | 16 | 907.44 | 0.00 |
| Score.type + AgGDP + AgGDP x Score.type + BatRich | 17 | 909.18 | 1.74 |

**Result:** No. Adding bat richness did not improve the model, so we proceed **without** bat richness.

### Step 3: Is an interaction with Score.type needed? (full dataset)

| Model | df | AICc | Delta AICc |
| :--- | ---: | ---: | ---: |
| Score.type only (null) | 10 | 956.4 | 37.8 |
| Score.type + AgGDP | 11 | 948.2 | 29.6 |
| Score.type + AgGDP + AgGDP x Score.type | 16 | 918.6 | 0.0 |

**Result:** Yes, the interaction model is clearly best (ΔAICc ≥ 29.6 over the alternatives), so we proceed with it.

### Step 4: Post-hoc tests of the AgGDP effect at each step

Each row tests whether the AgGDP slope for that step differs from zero. Slopes are log-odds of a higher score per +1 SD of log10 AgGDP (negative = lower scores in more agriculture-dependent countries), from the refitted model (24 countries). CIs and p-values are Holm-adjusted across the six steps.

| Step in knowledge pathway | Slope of AgGDP effect | SE | df | Lower CI | Upper CI | z | p (Holm) |
| :--- | ---: | ---: | :---: | ---: | ---: | ---: | ---: |
| Evidence | 0.56 | 0.36 | Inf | -0.39 | 1.51 | 1.55 | 0.120 |
| Representativeness | -1.38 | 0.35 | Inf | -2.31 | -0.45 | -3.93 | < 0.001 *** |
| Limiting Factors | -0.88 | 0.35 | Inf | -1.81 | 0.05 | -2.51 | 0.024 * |
| Available Interventions | -0.95 | 0.35 | Inf | -1.87 | -0.03 | -2.72 | 0.019 * |
| Monitoring Interventions | -1.10 | 0.36 | Inf | -2.06 | -0.14 | -3.03 | 0.010 ** |
| Breadth of Implementation of Interventions | -1.72 | 0.39 | Inf | -2.74 | -0.69 | -4.42 | < 0.001 *** |

**Summary:** Countries where agriculture makes up a larger share of GDP had lower scores at every step beyond "Evidence", with the strongest negative effects for Representativeness and Breadth of Implementation of Interventions. The slope for Evidence was positive but not distinguishable from zero.

> **Note:** For Limiting Factors the reported CI (-1.81 to 0.05) just includes zero even though p (Holm) = 0.024. The CIs and p-values appear to be adjusted by different methods in the post-hoc output; check the adjustment used for each before finalising.

<!-- TODO: add post-hoc slope figure (slope estimates with CIs and Holm p-values) -->

### Step 5: Model diagnostics

<!-- TODO: add Survey 1 diagnostic plots and brief comment on any deviations -->

---

## Survey 2: Intervention Testing score models

**Response:** testing score for each intervention (`Score.type`).

### Step 1: Model comparison (complete-case dataset)

Models within 2 AICc units of the best model:

| Model terms | Structure | df | logLik | AICc | Delta AICc | Akaike weight |
| :--- | :--- | ---: | ---: | ---: | ---: | ---: |
| Score.type + EP | Main effects only | 11 | -316.1 | 655.0 | 0.00 | 0.171 |
| Score.type + Lat x Score.type | With interaction | 16 | -310.6 | 655.1 | 0.07 | 0.165 |
| Score.type + EP x Score.type | With interaction | 16 | -310.9 | 655.7 | 0.67 | 0.122 |

Three models have essentially equal support, so there is real model uncertainty. Environmental Performance (EP) appears in two of the three, and was taken forward as the best-supported predictor.

<!-- TODO: add dredge model comparison figure (grey dashed line = 2 AICc above best) -->

### Step 2: Is an interaction with Score.type needed? (full dataset)

| Model | df | AICc | Delta AICc |
| :--- | ---: | ---: | ---: |
| Score.type only (null) | 10 | 742.4 | 10.8 |
| Score.type + EP | 11 | 733.8 | 2.2 |
| Score.type + EP + EP x Score.type | 16 | 731.6 | 0.0 |

**Result:** The interaction model had the lowest AICc, so we proceed with it. Support for the interaction over the main-effects model is modest (ΔAICc = 2.2).

**Why does this conflict with the dredge results?** The dredge screening was restricted to a subset of rows where all six country-level predictors had complete data (no NAs). On that restricted dataset, the main-effects EP model came out on top, with no interaction with `Score.type`. When the models were re-specified on the full dataset, all rows with valid EP values but missing values for the other indices were recovered, and in this larger dataset the interaction model had the lower AICc.

### Step 3: Post-hoc tests of the EP effect on each intervention

Each row tests whether the EP slope for that intervention differs from zero. Slopes are log-odds of a higher score per +1 SD of Environmental Performance score, from the refitted model (24 countries). CIs and p-values are Holm-adjusted across the six interventions.

| Intervention | Slope of EP effect | SE | df | Lower CI | Upper CI | z | p (Holm) |
| :--- | ---: | ---: | :---: | ---: | ---: | ---: | ---: |
| Artificial Roost | 0.34 | 0.29 | Inf | -0.42 | 1.10 | 1.18 | 0.713 |
| Agrochemical Reduction | 0.90 | 0.32 | Inf | 0.06 | 1.74 | 2.82 | 0.029 * |
| Revegetation | 0.84 | 0.35 | Inf | -0.07 | 1.76 | 2.42 | 0.068 |
| Bat Education | -0.16 | 0.25 | Inf | -0.83 | 0.50 | -0.65 | 1.000 |
| Artificial Water Sources | 1.29 | 0.52 | Inf | -0.09 | 2.67 | 2.47 | 0.068 |
| Acoustic Bat Lure | 0.28 | 0.43 | Inf | -0.84 | 1.40 | 0.65 | 1.000 |

**Summary:** Only Agrochemical Reduction showed a clear positive association with EP (about 2.5 times higher odds of a higher testing score per +1 SD of EP). Artificial Water Sources and Revegetation had similarly large positive slopes, but their confidence intervals included zero (both p = 0.068). Slopes for Artificial Roost, Acoustic Bat Lure and Bat Education were small and not distinguishable from zero.

<!-- TODO: add post-hoc slope figure (slope estimates with CIs and Holm p-values) -->

### Step 4: Model diagnostics

Quantile residual diagnostics for the best Survey 2 model showed **significant quantile deviations**, so the model's distributional assumptions are not perfectly met and the results should be interpreted with caution.

<!-- TODO: add Survey 2 diagnostic plots -->

---

<details>
<summary><b>Earlier exploratory models (mean scores across all steps / interventions)</b></summary>

These earlier models used a single mean score per respondent and system, rather than a score per step or intervention. They are superseded by the models above but are retained for reference.

**Survey 1 model:** `Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean)`

Family: Beta (logit link) | n = 75 observations, 24 countries, 37 respondents

| | Estimate | Std. Error | z value | p value |
|:---|---:|---:|---:|---:|
| (Intercept) | -1.374 | 0.483 | -2.848 | 0.0044 ** |
| Environmental Performance Index | 0.023 | 0.009 | 2.537 | 0.0112 * |

| Random effect | Variance | Std. Dev. |
|:---|---:|---:|
| Country | 0.186 | 0.431 |
| Respondent | <0.001 | <0.001 |

*AIC = -44.3, BIC = -32.7, log-likelihood = 27.2*

<p align="center">
  <img src="Analysis/model%20selection/Survey%201-%20Model%20predicted%20mean%20knowledge%20score%20by%20EnvPerformance.png" /><br>
  <sub><i>GLMM-predicted relationship between Environmental Performance Index and mean (±95% CI) Knowledge score (Survey 1, mean across knowledge pathway).</i></sub>
</p>

**Survey 2 model:** `Survey_2_mean_rescaled_beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean)`

Family: Beta (logit link) | n = 58 observations, 24 countries, 36 respondents

| | Estimate | Std. Error | z value | p value |
|:---|---:|---:|---:|---:|
| (Intercept) | -2.538 | 0.399 | -6.361 | <0.001 *** |
| Environmental Performance Index | 0.024 | 0.007 | 3.367 | <0.001 *** |

| Random effect | Variance | Std. Dev. |
|:---|---:|---:|
| Country | 0.026 | 0.162 |
| Respondent | <0.001 | <0.001 |

*AIC = -88.2, BIC = -77.9, log-likelihood = 49.1*

<p align="center">
  <img src="Analysis/model%20selection/Survey%202-%20Model%20predicted%20mean%20intervention%20score%20by%20EnvPerformance.png" /><br>
  <sub><i>GLMM-predicted relationship between Environmental Performance Index and mean (±95% CI) Intervention Testing scores (averaged across all 6 interventions).</i></sub>
</p>

</details>

---

# 🤝 Seeking GLMM expertise

If you are particularly experienced with GLMM modelling, especially involving socio-economic, environmental, or agricultural predictors while accounting for important covariates and hierarchical structure, we would welcome input on alternative model structures.

Please contact:

rani.davis@uqconnect.edu.au

Collaborators can be provided access to edit the GitHub repository and explore alternative approaches.

---

# 📂 Repository Structure

## Folders

* 📁 `Analysis/raw data/` — Raw survey data as submitted by contributors
* 📁 `Analysis/clean data/` — Cleaned and scored survey datasets
* 📁 `Analysis/scripts/` — Data processing, modelling, and visualisation scripts
* 📁 `Analysis/model selection/` — AICc model comparisons, diagnostic plots and model predictions
* 📁 `figure exports/` — All key figures produced by scripts
* 📁 `docs/map exports/` — Interactive HTML maps showing spatial coverage

---

# 📑 Scripts

| Script | Purpose |
| :--- | :--- |
| `Map Script` | Creates maps showing spatial coverage of study systems |
| `Colour palette Script` | Defines consistent colour palettes used across maps and figures |
| `Script 0` | Visualises the conceptual knowledge pathway |
| `Script 1` | Data wrangling for Survey 1 (Knowledge scoring) |
| `Script 2` | Produces figures summarising Survey 1 scores |
| `Script 3` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 4` | Produces figures summarising Survey 2 scores  |
| `Script 5` | Extracts socio-economic, environmental performance, and R&D spending indices |
| `Script 6` | Visualises Survey 1 scores against socio-economic indices |
| `Script 7` | Visualises Survey 2 scores against socio-economic indices |
| `Script 8` | Joins latitude data and prepares variables for modelling |
| `Script 9` | Joins bat richness data and prepares variables for modelling |
| `Script 10` | Fits GLMMs evaluating predictors of Knowledge Pathway scores (Survey 1 data) |
| `Script 11` | Fits GLMMs evaluating predictors of Intervention Testing scores (Survey 2 data) |

---
