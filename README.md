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

## Exploring predictors of Knowledge Pathway and Intervention Testing scores
We are exploring whether variation in Knowledge Pathway scores (Survey 1) and Intervention Testing scores (Survey 2) can be explained by broader **socio-economic, environmental, and agricultural factors**, while also considering important study-system characteristics such as crop type.

Potential explanatory indices being explored include:

- 🌍 Environmental Performance Index
- 💰 GDP
- 🌱 Agricultural contribution to GDP
- 🔬 National research & development investment (all sectors)
- 🔬Agricultural research & development investment


As an initial exploration, we visualised the relationship between country-level mean Knowledge Pathway and Intervention scores and each index. 

Below are simple linear models of two example indices (GDP and Environmental Performance Index) against the mean Knowledge and Intervention scores per country. You can view figures for all simple linear models inside 'figure exports/Models with indices/.../simple linear models'

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Knowledge%20scores/simple%20linear%20models/GDP%20vs%20Mean%20Knowledge%20Score%20for%20Country%20Means.jpeg" alt="GDP versus Knowledge Pathway score" /><br>
  <sub><i>Figure 7: Country-level mean Knowledge Pathway scores plotted against GDP, showing a simple linear relationship. This exploratory analysis motivates the use of more complex models that incorporate additional predictors and hierarchical structure.</i></sub>
</p>

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Intervention%20scores/simple%20linear%20models/Environmental%20Performance%20vs%20Mean%20Intervention%20Score%20for%20Country%20Means.jpeg" alt="Environmental Performance versus Intervention score" /><br>
  <sub><i>Figure 8: Country-level mean Intervention Testing scores plotted against Environmental Performance Index, showing a simple linear relationship. This exploratory analysis motivates the use of more complex models that incorporate additional predictors and hierarchical structure.</i></sub>
</p>

## Why GLMMs?

The simple linear models shown above do not account for study systems not being independent, and these relationships are unlikely to be driven by a single factor (e.g. GDP). 

Therefore, we are moving beyond simple correlations and using **Generalised Linear Mixed Models (GLMMs)** to evaluate multiple predictors simultaneously

The current models investigate:

- 🌱 Crop type.
- 📍 Absolute latitude. As a proxy for biodiversity as more complex systems with more species may be more challenging to gain knowledge in.
- 🌍 The socio-economic, environmental, and agricultural indices mentioned above.

Among tested indices, Environmental Performance Index currently appears to be the strongest predictor and is included in the current best-supported models. You can find more about how the Environmental Performance Index is calculated here https://epi.yale.edu

---

## Current best model structures

| Survey | Response variable | Fixed effects | Random effect |
| :--- | :--- | :--- | :--- |
| Survey 1 | Knowledge Pathway score (`MeanScore.allSteps`) | Crop type + Environmental Performance Index + Absolute latitude | Country |
| Survey 2 | Intervention score (`MeanScore.allInterventions`) | Crop type + Environmental Performance Index + Absolute latitude | Country |

---

## Current GLMM results

#### Survey 1 — Knowledge Pathway scores

The current best-supported GLMM investigates how crop type, Environmental Performance Index, and absolute latitude influence Knowledge Pathway scores while accounting for non-independence among study systems.

Key model-predicted relationships are shown below.

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Knowledge%20scores/GLMM%20predictions/Model-predicted%20Env%20Performance%20effect%20on%20mean%20Knowledge%20Scores.jpeg" alt="Environmental Performance effect on Knowledge Pathway scores" /><br>
  <sub><i>Figure 10: GLMM-predicted relationship between Environmental Performance Index and mean Knowledge Pathway scores (±95% CI).</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Knowledge%20scores/GLMM%20predictions/Model-predicted%20Latitude%20effect%20on%20mean%20Knowledge%20Scores.jpeg" alt="Latitude effect on Knowledge Pathway scores" /><br>
  <sub><i>Figure 11: GLMM-predicted relationship between absolute latitude and mean Knowledge Pathway scores (±95% CI).</i></sub>

</p>

<br>

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Knowledge%20scores/GLMM%20predictions/Model-predicted%20Crop%20Type%20effect%20on%20mean%20Knowledge%20Scores.jpeg" alt="Crop type effect on Knowledge Pathway scores" /><br>
  <sub><i>Figure 9: GLMM-predicted effect of crop type on mean Knowledge Pathway scores (±95% CI).</i></sub>

</p>


---

#### Survey 2 — Intervention scores

The current best-supported GLMM investigates how crop type, Environmental Performance Index, and absolute latitude influence Intervention Testing scores while accounting for non-independence among study systems.

Key model-predicted relationships are shown below.

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Intervention%20scores/GLMM%20predictions/Model-predicted%20Env%20Performance%20effect%20on%20mean%20Intervention%20Scores.jpeg" alt="Environmental Performance effect on Intervention scores" /><br>
  <sub><i>Figure 13: GLMM-predicted relationship between Environmental Performance Index and mean Intervention Testing scores (±95% CI).</i></sub>
</p>

<br>

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Intervention%20scores/GLMM%20predictions/Model-predicted%20Latitude%20effect%20on%20mean%20Intervention%20Scores.jpeg" alt="Latitude effect on Intervention scores" /><br>
  <sub><i>Figure 14: GLMM-predicted relationship between absolute latitude and mean Intervention Testing scores (±95% CI).</i></sub>

</p>

<br>

<p align="center">
  <img src="figure%20exports/Models%20with%20indices/Intervention%20scores/GLMM%20predictions/Model-predicted%20Crop%20Type%20effect%20on%20mean%20Intervention%20Scores.jpeg" alt="Crop type effect on Intervention scores" /><br>
  <sub><i>Figure 12: GLMM-predicted effect of crop type on mean Intervention Testing scores (±95% CI).</i></sub>
</p>


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
| `Script 3` | Extracts socio-economic, environmental performance, and R&D spending indices |
| `Script 4` | Visualises Survey 1 scores against socio-economic indices |
| `Script 5` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 6` | Produces figures summarising Survey 2 scores |
| `Script 7` | Visualises Survey 2 scores against socio-economic indices |
| `Script 8` | Joins latitude data and prepares variables for modelling |
| `Script 9` | Fits initial GLMMs evaluating predictors of Knowledge Pathway and Intervention scores |

---