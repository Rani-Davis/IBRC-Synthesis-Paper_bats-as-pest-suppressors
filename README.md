<h1 align="center">🦇 IBRC Synthesis Paper: Bats as Pest Suppressors 🦇</h1>

Our global synthesis paper summarises published and unpublished knowledge on insectivorous bats as pest suppressors in agricultural systems, and the interventions trialled to support them.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

---

## 📋 Overview of Data

Contributors were asked to complete two surveys to gather knowledge about the research conducted in their study systems. A **study system** is defined as a crop type $\times$ region combination in which studies were done.

### Survey Breakdown

| Survey | Focus | Purpose |
| :--- | :--- | :--- |
| **# 1: Knowledge scoring** | Conceptual pathway | Places each study system within our conceptual "knowledge pathway" (see **Figure 1** below). Scores at each step along the pathway scale from 0 (no knowledge) to 3 (extensive knowledge)|
| **# 2: Implementation scoring** | Interventions | Gathers information on what interventions have been implemented and tested for their effects on bats in each study system. Scores for each intervention scale from 0 (intervention not tried) to 3 (intervention tried and evaluated)|

### Conceptual Knowledge Pathway

<p align="center">
  <img src="figure%20exports/Survey%1- Knowledge pathway figures/Our%20Conceptual%20Knowledge%20Pathway_with%20questions.png" alt="Our conceptual knowledge pathway" /><br>
  <sub><i>Figure 1: Conceptual knowledge pathway used to score study systems (see Script 0) (click on image to view larger).</i></sub>
</p>

---

## 🗺️ Study System Maps

Explore the global coverage of our study systems using these interactive maps:
* 📍 [Interactive study system map (Pin markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/teardrop%20map_interactive.html)
* 🔵 [Interactive study system map (Circle markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/circle%20map_interactive.html)

---

## 📊 Key Figures

<h2 align="center"> Knowledge Pathway Scores (Survey 1) </h2>

<p align="center">
  <img src="figure%20exports/Knowledge%pathway%scores/Draft_composite%20knowledge%20pathway%20plot_By%20Crop%20Type.png" alt="Knowledge pathway results by crop" /><br>
  <sub><i>Figure 2: Knowledge pathway progress (Evidence → Implementation) across crop types, showing mean scores (± SE) with raw respondent-level data points (click to enlarge).</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Knowledge%pathway%scores/Draft_composite%20knowledge%20pathway%20plot_By%20World%20Region.png" alt="Knowledge pathway results by region" /><br>
  <sub><i>Figure 3: Knowledge pathway progress (Evidence → Implementation) across regions, showing mean scores (± SE) with raw respondent-level data points (click to enlarge).</i></sub>
</p>

---

<h2 align="center"> Intervention Testing Scores (Survey 2) </h2> 

<p align="center">
  <img src="figure%20exports/Intervention%testing%scores/Overall.intervention.testing.stackedbar.jpeg" alt="Implementation results stacked bar" /><br>
  <sub><i>Figure 4: Proportion of systems that have tested different bat-supportive interventions. 'NAs' correspond to incomplete surveys, which will be updated when all survey entries are received (click to enlarge).</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%testing%scores/Mean.SE.ByCrop.Intervention.testing.jpeg" alt="Mean testing scores by crop" /><br>
  <sub><i>Figure 5: Mean +/- SE of testing scores by crop (click to enlarge).</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%testing%scores/Mean.SE.ByRegion.Intervention.testing.jpeg" alt="Mean testing scores by region" /><br>
  <sub><i>Figure 6: Mean +/- SE of testing scores by region (click to enlarge).</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Intervention%scores%vs%indexes%models/EnvPerformance.vs.Overall.intervention.testing.score.jpeg" alt="Environmental Performance Index vs Intervention Score" /><br>
  <sub><i>Figure 7: Country-level mean intervention score (total score across all interventions) against the <a href="https://epi.yale.edu">Yale Environmental Performance Index</a>. (Note: Total intervention scores against GDP, agricultural R&D spending, etc., can be viewed in the figure exports folder. Next steps include modelling as GLMMs).</i></sub>
</p>

---

# 📈 Generalised Linear Mixed Models (GLMMs)

## Why GLMMs?

Study systems are not statistically independent because multiple observations can occur within the same country, crop type, or contributor.

To identify the factors associated with knowledge progression and intervention implementation while accounting for this non-independence, we fitted **Generalised Linear Mixed Models (GLMMs)** using the `glmmTMB` package in R.

The current set of models investigate the influence of:

- 🌱 Crop type
- 🌍 Various indices (GDP, value added by Agriculture as a % of GDP, Environmental Performance Index. But Environmental Performance Index appears to be the best predictor.
- 📍 Absolute latitude

# Current best model structure

| Survey | Response variable | Fixed effects | Random effect |
| :--- | :--- | :--- | :--- |
| Survey 1 | Knowledge Pathway score (`MeanScore.allSteps`) | Crop type + Environmental Performance + Absolute latitude | Country |
| Survey 2 | Intervention score (`MeanScore.allInterventions`) | Crop type + Environmental Performance + Absolute latitude | Country |

# Current GLMM results

## Survey 1 — Knowledge Pathway scores

The current best-supported model indicates that mean Knowledge Pathway scores vary among crop types and are associated with environmental and geographic context.

Key findings:

- Knowledge Pathway scores differed significantly among crop types.
- Environmental Performance Index showed a positive relationship with predicted Knowledge Pathway scores.
- Absolute latitude contributed additional explanatory power.

### Model-predicted effects

# Survey 2 — Intervention scores
The current best-supported model indicates that mean intervention testing scores vary among crop types and are associated with environmental and geographic context.

Key findings:

- Intervention scores differed among crop types.
- Environmental Performance Index was positively associated with predicted intervention scores.
- Absolute latitude contributed additional explanatory power.


### Model-predicted effects


# What we need from you:
If you are particularly skilled with GLMM models, especially considering multiple indices of socio-economic, environmental or agricultural performance, whilst considering other important covariates, please reach out to rani.davis@uqconnect.edu.au and you can get access to editing the github try alternative models

---

## 📂 Repository Structure

### Folders:
* 📁 `Analysis/raw data/` — Raw survey data as submitted by contributors
* 📁 `Analysis/clean data/` — Cleaned and scored survey data
* 📁 `Analysis/scripts/` — Core analysis scripts 
* 📁 `figure exports/` — All key figures produced by scripts
* 📁 `docs/map exports/` — Interactive HTML maps showing spatial coverage

### Scripts:

| Script | Purpose |
| :--- | :--- |
| `Map Script` | Creates maps to show spatial coverage of study systems in the dataset |
| `Colour palette Script` | Defines consistent colour palettes used across maps and figures |
| `Script 0` | Visualises our conceptual knowledge pathway |
| `Script 1` | Data wrangling for Survey 1 (Knowledge scoring) |
| `Script 2` | Produces figures summarising Survey 1 scores |
| `Script 3` | Extracts socio-economic, environmental performance, and R&D spending indices |
| `Script 4` | Visualises Survey 1 scores against socio-economic indices |
| `Script 5` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 6` | Produces figures summarising Survey 2 scores |
| `Script 7` | Visualises Survey 2 scores against socio-economic indices |
