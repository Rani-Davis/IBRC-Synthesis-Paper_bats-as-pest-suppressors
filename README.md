<h1 align="center">🦇 IBRC Synthesis Paper: Bats as Pest Suppressors 🦇</h1>

Our global synthesis paper summarises published and unpublished knowledge on insectivorous bats as pest suppressors in agricultural systems, and the interventions trialled to support them.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

---

## 📋 Overview of Data

Contributors were asked to complete two surveys to gather knowledge about the research conducted in their study systems. A **study system** is defined as a crop type $\times$ region combination in which studies were done.

### Survey Breakdown

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
  <sub><i>Figure 2: Knowledge pathway progress across crop types, showing mean scores (± SE) with raw respondent-level data points.</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Knowledge%20pathway%20scores/Draft_composite%20knowledge%20pathway%20plot_By%20World%20Region.png" alt="Knowledge pathway results by region" /><br>
  <sub><i>Figure 3: Knowledge pathway progress across regions, showing mean scores (± SE) with raw respondent-level data points.</i></sub>
</p>

---

## Intervention Testing Scores (Survey 2)

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Overall.intervention.testing.stackedbar.jpeg" alt="Implementation results stacked bar" /><br>
  <sub><i>Figure 4: Proportion of study systems that have tested different bat-supportive interventions.</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Mean.SE.ByCrop.Intervention.testing.jpeg" alt="Mean testing scores by crop" /><br>
  <sub><i>Figure 5: Mean intervention testing scores by crop type.</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%20testing%20scores/Mean.SE.ByRegion.Intervention.testing.jpeg" alt="Mean testing scores by region" /><br>
  <sub><i>Figure 6: Mean intervention testing scores by world region.</i></sub>
</p>

---

# 📈 Generalised Linear Mixed Models (GLMMs)

## Why GLMMs?

The descriptive figures above summarise patterns in survey responses but do not account for study systems not being independent. There can be multiple studies within the same country, crop type, or completed by the same contributor.

To identify the factors associated with knowledge progression or intervention implementation while accounting for this non-independence, we fitted **Generalised Linear Mixed Models (GLMMs)** using the `glmmTMB` package in R.

GLMMs allow us to:

- quantify relationships between survey scores and explanatory variables;
- account for variation among countries using random effects;
- estimate the influence of environmental and socio-economic predictors;
- generate model-predicted effects with 95% confidence intervals;
- compare crop types using estimated marginal means (EMMs) and Tukey-adjusted pairwise comparisons.

The current models investigate:

- 🌱 Crop type
- 🌍 Socio-economic and environmental indices (GDP, R&D expenditure in all sectors, Agricultural R&D investment,Agricultural contribution to GDP, and Environmental Performance Index)
- 📍 Absolute latitude

Among tested indices, Environmental Performance Index currently appears to be the strongest predictor and is included in the current best-supported models. You can find more about how the Environmental Performance Index is calculated here https://epi.yale.edu

---

# Current best model structure

| Survey | Response variable | Fixed effects | Random effect |
| :--- | :--- | :--- | :--- |
| Survey 1 | Knowledge Pathway score (`MeanScore.allSteps`) | Crop type + Environmental Performance Index + Absolute latitude | Country |
| Survey 2 | Intervention score (`MeanScore.allInterventions`) | Crop type + Environmental Performance Index + Absolute latitude | Country |

---

# Current GLMM results

## Survey 1 — Knowledge Pathway scores

The current best-supported model indicates that mean Knowledge Pathway scores vary among crop types and are associated with environmental and geographic context.

Key findings:

- Knowledge Pathway scores differed among crop types.
- Environmental Performance Index showed a positive relationship with predicted Knowledge Pathway scores.
- Absolute latitude contributed additional explanatory power.

### Model-predicted effects

*(GLMM effect figures will be added here)*

---

## Survey 2 — Intervention scores

The current best-supported model indicates that mean intervention testing scores vary among crop types and are associated with environmental and geographic context.

Key findings:

- Intervention scores differed among crop types.
- Environmental Performance Index was positively associated with predicted intervention scores.
- Absolute latitude contributed additional explanatory power.

### Model-predicted effects

*(GLMM effect figures will be added here)*

---

# 🤝 Seeking GLMM expertise

If you are particularly experienced with GLMM modelling, especially approaches involving multiple socio-economic, environmental, or agricultural predictors while accounting for important covariates and hierarchical structure, we would welcome input on alternative model structures.

Please contact:

rani.davis@uqconnect.edu.au

Collaborators can be provided access to edit the GitHub repository and explore alternative approaches.

---

# 📂 Repository Structure

## Folders

* 📁 `Analysis/raw data/` — Raw survey data as submitted by contributors
* 📁 `Analysis/clean data/` — Cleaned and scored survey datasets
* 📁 `Analysis/scripts/` — Data processing, modelling, and visualisation scripts
* 📁 `Analysis/model outputs/` — Model summaries, diagnostics, and statistical outputs
* 📁 `figure exports/` — All key figures produced by scripts
* 📁 `docs/map exports/` — Interactive HTML maps showing spatial coverage

---

# Scripts

| Script | Purpose |
| :--- | :--- |
| `Map Script` | Creates maps showing spatial coverage of study systems |
| `Colour palette Script` | Defines consistent colour palettes used across maps and figures |
| `Script 0` | Visualises the conceptual knowledge pathway |
| `Script 1` | Data wrangling for Survey 1 (Knowledge scoring) |
| `Script 2` | Produces figures summarising Survey 1 scores |
| `Script 3` | Extracts socio-economic, environmental performance, and R&D spending indices |
| `Script 4` | Visualises Survey 1 scores against socio-economic indices |
| `Script 5` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 6` | Produces figures summarising Survey 2 scores |
| `Script 7` | Visualises Survey 2 scores against socio-economic indices |
| `Script 8` | Joins latitude data and prepares variables for modelling |
| `Script 9` | Fits initial GLMMs evaluating predictors of Knowledge Pathway and Intervention scores |

---