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

Potential explanatory indices being explored include:

- 🌍 Environmental Performance Index
- 💰 GDP
- 🌱 Agricultural contribution to GDP
- 🔬Agricultural research & development investment


## Why GLMMs?

The simple linear models shown above do not account for study systems not being independent, and these relationships are unlikely to be driven by a single factor (e.g. GDP). 

Therefore, we are moving beyond simple correlations and using **Generalised Linear Mixed Models (GLMMs)** to evaluate multiple predictors simultaneously

The current models investigate:

- 🌱 Crop type.
- 🦇 Bat species richness - more complex systems with more species may be more challenging to gain knowledge in.
- 🌍 The socio-economic, environmental, and agricultural indices mentioned above.

Among tested indices, Environmental Performance Index appears to be the strongest predictor and is included in the current best-supported models. You can find more about how the Environmental Performance Index is calculated here https://epi.yale.edu

---

## Current best model structures

| Survey | Response variable | Fixed effects | Random effects |
| :--- | :--- | :--- | :--- |
| Survey 1 | Knowledge Pathway score (`MeanScore.allSteps`) | Environmental Performance Index  | Country + Respondent |
| Survey 2 | Intervention score (`MeanScore.allInterventions`) | Environmental Performance Index | Country + Respondent |

---

## Current GLMM results

#### Survey 1 — Knowledge Pathway scores

The current best-supported GLMM for predicting mean Knowledge Pathway scores (averaged across the entire pathway) is:

**Model:** `Survey_1_mean_rescaled_beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean)`
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
  <sub><i>Figure 13: GLMM-predicted relationship between Environmental Performance Index and mean (±95% CI) Knowledge score (Survey 1, mean across knowledge pathway).  </i></sub>
</p>

<br>


---

#### Survey 2 — Intervention scores

The current best-supported GLMM for predicting mean Intervention Testing scores (across all 6 interventions) is:

**Model:** `Survey_2_mean_rescaled_beta ~ EnviroPerformance_score + (1|Country.clean) + (1|Respondent.clean)`
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
  <sub><i>Figure 13: GLMM-predicted relationship between Environmental Performance Index and mean (±95% CI) Intervention Testing scores (averaged across all 6 interventions)  </i></sub>
</p>

<br>



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
| `Script 10` | Fits GLMMs evaluating predictors of mean Knowledge Pathway scores (Survey 1 data) |
| `Script 11` | Fits GLMMs evaluating predictors of mean Intervention Testing scores (Survey 2 data) |

---