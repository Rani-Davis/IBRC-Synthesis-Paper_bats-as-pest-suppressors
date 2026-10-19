<h1 align="center">🦇 IBRC Synthesis Paper: Bats as Pest Suppressors 🦇</h1>

Our global synthesis paper summarises published and unpublished knowledge on insectivorous bats as pest suppressors in agricultural systems, and the interventions trialled to support them.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

---

## 📋 Overview of Data

Contributors completed two surveys about the research conducted in their study systems. A **study system** is a crop type $\times$ region combination in which studies were done.

| Survey | Focus | Purpose |
| :--- | :--- | :--- |
| **#1: Knowledge scoring** | Conceptual pathway | Places each study system within our conceptual "knowledge pathway" (**Figure 1**). Scores at each step range from 0 (no knowledge) to 3 (extensive knowledge). |
| **#2: Implementation scoring** | Interventions | Records which interventions have been implemented and tested for their effects on bats. Scores for each intervention range from 0 (not tried) to 3 (tried and evaluated). |

<p align="center">
  <img src="figure%20exports/Our%20Conceptual%20Knowledge%20Pathway_with%20questions.png" alt="Our conceptual knowledge pathway" /><br>
  <sub><i>Figure 1: Conceptual knowledge pathway used to score study systems (see Script 0).</i></sub>
</p>

---

# 🗺️ Study System Maps

* 📍 [Interactive study system map (Pin markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/teardrop%20map_interactive.html)
* 🔵 [Interactive study system map (Circle markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/circle%20map_interactive.html)

---

# 📊 Key Figures

## Knowledge Pathway Scores (Survey 1)

<p align="center">
  <img src="figure%20exports/Knowledge%20pathway%20scores/Draft_composite%20knowledge%20pathway%20plot_By%20Crop%20Type.png" alt="Knowledge pathway results by crop" /><br>
  <sub><i>Figure 2: Knowledge pathway progress across crop types, showing mean scores (± SE) with raw respondent-level data points.</i></sub>
</p>

<p align="center">
  <img src="figure%20exports/Knowledge%20pathway%20scores/Draft_composite%20knowledge%20pathway%20plot_By%20World%20Region.png" alt="Knowledge pathway results by region" /><br>
  <sub><i>Figure 3: Knowledge pathway progress across regions, showing mean scores (± SE) for each question in Survey 1 with raw respondent-level data points.</i></sub>
</p>

## Intervention Testing Scores (Survey 2)

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Overall.intervention.testing.stackedbar.jpeg" alt="Implementation results stacked bar" /><br>
  <sub><i>Figure 4: Proportion of study systems that have tested different bat-supportive interventions.</i></sub>
</p>

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Mean.SE.ByCrop.Intervention.testing.jpeg" alt="Mean testing scores by crop" /><br>
  <sub><i>Figure 5: Mean intervention testing scores (± SE) by crop type.</i></sub>
</p>

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Mean.SE.ByRegion.Intervention.testing.jpeg" alt="Mean testing scores by region" /><br>
  <sub><i>Figure 6: Mean intervention testing scores (± SE) by world region.</i></sub>
</p>

---

# 📈 Do country-level factors explain knowledge and intervention testing? (CLMMs)

We tested whether scores in each survey were associated with broader **socio-economic, environmental and agricultural factors** at the country level, while accounting for the step or intervention being scored. Because scores are ordinal (0 to 3), we fitted separate **Cumulative Link Mixed Models (CLMMs)** for each survey.

**Full methods** (predictor sources and transformations, candidate model set, and AICc ranking) are in the manuscript Methods and Supplementary Table X. Code is in `Script 10` (Survey 1) and `Script 11` (Survey 2).

## 🔍 Predictors explored

Socio-economic, environmental and agricultural indices:

- 🌍 **Environmental Performance Index (EP)**: 2026 scores from Yale's EPI, which combines 47 indicators across Ecosystem Vitality, Environmental Health and Climate Change to rank 177 countries on progress towards environmental policy targets. More information: https://epi.yale.edu
- 💰 **GDP**: the scale of each country's economy (World Bank, via the `WDI` R package).
- 🌾 **Agricultural contribution to GDP (AgGDP)**: agriculture, forestry and fishing, value added (% of GDP), an indicator of how much a country's economy relies on these sectors (World Bank, via `WDI`).
- 🔬 **Agricultural research & development investment**: public agricultural R&D expenditure (million 2017 PPP$), from the GRAPE dataset.

Additional predictors:

- 🦇 **Bat species richness**: more complex systems with more species may be harder to gain knowledge in (country-level species counts from batnames.org).
- 🧭 **Latitude** of each study system (joined in Script 8).

<!-- TODO: confirm which predictors were log10-transformed (methods list GDP, agricultural R&D and bat richness; the Survey 1 post-hoc slopes are per SD of log10 AgGDP) -->

## 💻 Predictor 'budget'

We have responses from 24 countries, and the socio-economic and bat species richness data are at country level. Following the 10-observations-per-predictor rule, we can only 'afford' up to **2 country-level predictors** (excluding random effects), plus `Score.type` and its interactions.

## 🎲 Random effects

- 🌍 `(1 | Country)`: accounts for multiple respondents per country sharing the same GDP, EP etc. values.
- 👤 `(1 | Respondent)`: accounts for multiple responses per person.

## ✅ Best-supported models

| Survey | Response variable | Best model |
| :--- | :--- | :--- |
| Survey 1: Knowledge pathway | Score at each step of the pathway | `Score ~ Score.type + AgGDP × Score.type + (1 \| Country) + (1 \| Respondent)` |
| Survey 2: Intervention testing | Testing score for each intervention | `Score ~ Score.type + EP × Score.type + (1 \| Country) + (1 \| Respondent)` |

**How to read these models.** `Score.type` lets the average score differ among pathway steps (or interventions). The interaction (`× Score.type`) means the effect of the country-level predictor is allowed to **differ for each step or intervention**, rather than being a single overall slope. Each slope in the post-hoc tables is the change in the cumulative log-odds of a higher score for each +1 SD of the predictor. Positive slopes mean higher scores as the predictor increases, and negative slopes mean lower scores. P-values and confidence intervals are Holm-adjusted across the six steps or interventions, and a slope is considered clearly different from zero when its confidence interval excludes zero.

<details>
<summary><b>How the best models were chosen (click to expand)</b></summary>

Candidate models were ranked with `dredge()` and AICc on a complete-case dataset (no missing values for any candidate predictor). Models within 2 AICc units of the best were considered comparable. The best predictor was then refitted on the full dataset (all rows with valid values for that predictor) to test whether the interaction with `Score.type` was needed. AICc values are only comparable between models fitted to the same dataset.

**Survey 1: models within 2 AICc (complete-case dataset)**

| Model terms | df | logLik | AICc | Delta AICc | Akaike weight |
| :--- | ---: | ---: | ---: | ---: | ---: |
| Score.type + AgGDP × Score.type | 16 | -389.8 | 813.1 | 0.00 | 0.543 |
| Score.type + AgGDP × Score.type + BatRich | 17 | -389.6 | 814.8 | 1.79 | 0.221 |

Bat richness did not improve the model on the full dataset (ΔAICc = 1.74), so it was excluded.

**Survey 1: is the interaction needed? (full dataset)**

| Model | df | AICc | Delta AICc |
| :--- | ---: | ---: | ---: |
| Score.type only (null) | 10 | 956.4 | 37.8 |
| Score.type + AgGDP | 11 | 948.2 | 29.6 |
| Score.type + AgGDP + AgGDP × Score.type | 16 | 918.6 | 0.0 |

**Survey 2: models within 2 AICc (complete-case dataset)**

| Model terms | df | logLik | AICc | Delta AICc | Akaike weight |
| :--- | ---: | ---: | ---: | ---: | ---: |
| Score.type + EP | 11 | -316.1 | 655.0 | 0.00 | 0.171 |
| Score.type + Lat × Score.type | 16 | -310.6 | 655.1 | 0.07 | 0.165 |
| Score.type + EP × Score.type | 16 | -310.9 | 655.7 | 0.67 | 0.122 |

**Survey 2: is the interaction needed? (full dataset)**

| Model | df | AICc | Delta AICc |
| :--- | ---: | ---: | ---: |
| Score.type only (null) | 10 | 742.4 | 10.8 |
| Score.type + EP | 11 | 733.8 | 2.2 |
| Score.type + EP + EP × Score.type | 16 | 731.6 | 0.0 |

The Survey 2 dredge on the complete-case dataset favoured the main-effects EP model, but refitting on the full dataset recovered rows with valid EP values but missing values for other indices, and the interaction model then had the lowest AICc. Support for the interaction is modest (ΔAICc = 2.2).

</details>

<!-- TODO: add model summary tables (coefficients and random-effect variances) for each best model; see S1_AgGDP_model_summary_tables.docx and S2_EP_model_summary_tables.docx -->

---

## 🌾 Survey 1: Knowledge Pathway

**What the best model says:** the more a country's economy relies on agriculture (higher AgGDP), the **lower** the knowledge scores, at every step of the pathway except Evidence. The effect was strongest for Representativeness and Breadth of Implementation of Interventions.

<p align="center">
  <img src="figure%20exports/Model-predictions%20for%20indices/Fig_S1_AgGDP_by_Score.type_overlay.png" alt="Model predictions of knowledge score by agriculture share of GDP for each pathway step" /><br>
  <sub><i>Figure 7: CLMM-predicted relationship between the percentage of GDP contributed by agriculture, forestry and fishing and scores at each step of the knowledge pathway.</i></sub>
</p>

**Post-hoc test of the AgGDP effect at each step.** Each row tests whether the AgGDP slope for that step differs from zero (negative = lower scores in more agriculture-dependent countries). Slopes are on the cumulative log-odds scale per +1 SD of AgGDP, from the refitted model (24 countries).

| Step in knowledge pathway | Slope | SE | Lower CI | Upper CI | z | p (Holm) |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Evidence | 0.56 | 0.36 | -0.39 | 1.51 | 1.55 | 0.120 |
| Representativeness | -1.38 | 0.35 | -2.31 | -0.45 | -3.93 | < 0.001 *** |
| Limiting Factors | -0.88 | 0.35 | -1.81 | 0.05 | -2.51 | 0.024 * |
| Available Interventions | -0.95 | 0.35 | -1.87 | -0.03 | -2.72 | 0.019 * |
| Monitoring Interventions | -1.10 | 0.36 | -2.06 | -0.14 | -3.03 | 0.010 ** |
| Breadth of Implementation of Interventions | -1.72 | 0.39 | -2.74 | -0.69 | -4.42 | < 0.001 *** |

> **Note:** For Limiting Factors the CI just includes zero (-1.81 to 0.05) even though p (Holm) = 0.024, so treat this step as borderline.

<!-- TODO: add Survey 1 post-hoc slope figure and diagnostics -->

---

## 🌿 Survey 2: Intervention Testing

**What the best model says:** countries with higher Environmental Performance tended to have higher intervention testing scores, but this differed among interventions. Only **Agrochemical Reduction** showed a clear positive association (about 2.5 times higher odds of a higher score per +1 SD of EP). Artificial Water Sources and Revegetation had similarly large positive slopes with intervals including zero (both p = 0.068), and the remaining interventions had slopes close to zero.

<p align="center">
  <img src="figure%20exports/Model-predictions%20for%20indices/Fig_S2_EP_overlay.png" alt="Model predictions of intervention testing score by Environmental Performance Index for each intervention" /><br>
  <sub><i>Figure 8: CLMM-predicted relationship between Environmental Performance Index and testing scores for each intervention.</i></sub>
</p>

**Post-hoc test of the EP effect on each intervention.** Each row tests whether the EP slope for that intervention differs from zero. Slopes are on the cumulative log-odds scale per +1 SD of Environmental Performance, from the refitted model (24 countries).

| Intervention | Slope | SE | Lower CI | Upper CI | z | p (Holm) |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| Artificial Roost | 0.34 | 0.29 | -0.42 | 1.10 | 1.18 | 0.713 |
| Agrochemical Reduction | 0.90 | 0.32 | 0.06 | 1.74 | 2.82 | 0.029 * |
| Revegetation | 0.84 | 0.35 | -0.07 | 1.76 | 2.42 | 0.068 |
| Bat Education | -0.16 | 0.25 | -0.83 | 0.50 | -0.65 | 1.000 |
| Artificial Water Sources | 1.29 | 0.52 | -0.09 | 2.67 | 2.47 | 0.068 |
| Acoustic Bat Lure | 0.28 | 0.43 | -0.84 | 1.40 | 0.65 | 1.000 |

> **Caution:** The interaction model had modest support over the main-effects model (ΔAICc = 2.2), and model diagnostics showed significant quantile deviations. Interpret these results cautiously.

<!-- TODO: add Survey 2 post-hoc slope figure and diagnostics -->

---

# 🤝 Seeking modelling expertise

If you are experienced with mixed models involving socio-economic, environmental or agricultural predictors and hierarchical structure, we would welcome input on alternative model structures.

Please contact: rani.davis@uqconnect.edu.au

Collaborators can be given access to edit the GitHub repository and explore alternative approaches.

---

# 📂 Repository Structure

* 📁 `Analysis/raw data/` — Raw survey data as submitted by contributors
* 📁 `Analysis/clean data/` — Cleaned and scored survey datasets
* 📁 `Analysis/scripts/` — Data processing, modelling, and visualisation scripts
* 📁 `Analysis/model selection/` — AICc model comparisons, diagnostics and model predictions
* 📁 `figure exports/` — Key figures produced by scripts
* 📁 `docs/map exports/` — Interactive HTML maps showing spatial coverage

# 📑 Scripts

| Script | Purpose |
| :--- | :--- |
| `Map Script` | Creates maps showing spatial coverage of study systems |
| `Colour palette Script` | Defines consistent colour palettes used across maps and figures |
| `Script 0` | Visualises the conceptual knowledge pathway |
| `Script 1` | Data wrangling for Survey 1 (Knowledge scoring) |
| `Script 2` | Produces figures summarising Survey 1 scores |
| `Script 3` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 4` | Produces figures summarising Survey 2 scores |
| `Script 5` | Extracts socio-economic, environmental performance, and R&D spending indices |
| `Script 6` | Visualises Survey 1 scores against socio-economic indices |
| `Script 7` | Visualises Survey 2 scores against socio-economic indices |
| `Script 8` | Joins latitude data and prepares variables for modelling |
| `Script 9` | Joins bat richness data and prepares variables for modelling |
| `Script 10` | Fits CLMMs evaluating predictors of Knowledge Pathway scores (Survey 1 data) |
| `Script 11` | Fits CLMMs evaluating predictors of Intervention Testing scores (Survey 2 data) |

