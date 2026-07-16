<h1 align="center">🦇 IBRC Synthesis Paper: Bats as Pest Suppressors 🦇</h1>

Our global synthesis paper summarises published and unpublished knowledge on insectivorous bats as pest suppressors in agricultural systems, and the interventions trialled to support them.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

---

## 📋 Overview of Data

Contributors were asked to complete two surveys to gather knowledge about the research conducted in their study systems. A **study system** is defined as a crop type $\times$ region combination in which studies were done.

### Survey Breakdown

| Survey | Focus | Purpose |
| :--- | :--- | :--- |
| **# 1: Knowledge scoring** | Conceptual pathway | Places each study system within our conceptual "knowledge pathway" (see **Figure 1** below). |
| **# 2: Implementation scoring** | Interventions | Gathers information on what interventions have been implemented and tested for their effects on bats in each study system. |

### Figure 1: Conceptual Knowledge Pathway
![Our conceptual knowledge pathway](figure%20exports/Our%20Conceptual%20Knowledge%20Pathway_with%20questions.png)
*Figure 1: Conceptual knowledge pathway used to score study systems (see Script 0) (click on image to view larger).*

---

## 🗺️ Study System Maps

Explore the global coverage of our study systems using these interactive maps:
* 📍 [Interactive study system map (Pin markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/teardrop%20map_interactive.html)
* 🔵 [Interactive study system map (Circle markers)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/circle%20map_interactive.html)

---

## 📊 Key Figures

### Knowledge Pathway Results (Survey 1)

![Knowledge pathway results by crop](figure%20exports/Draft_composite%20knowledge%20pathway%20plot_By%20Crop%20Type.png)
*Figure 2: Knowledge pathway progress (Evidence → Implementation) across crop types, showing mean scores (± SE) with raw respondent-level data points (click to enlarge).*

![Knowledge pathway results by region](figure%20exports/Draft_composite%20knowledge%20pathway%20plot_By%20World%20Region.png)
*Figure 3: Knowledge pathway progress (Evidence → Implementation) across regions, showing mean scores (± SE) with raw respondent-level data points (click to enlarge).*

### Implementation of Interventions (Survey 2)

![Implementation results stacked bar](figure%20exports/Overall.intervention.testing.stackedbar.jpeg)
*Figure 4: Proportion of systems that have tested different bat-supportive interventions. 'NAs' correspond to incomplete surveys, which will be updated when all survey entries are received (click to enlarge).*

![Mean testing scores by crop](figure%20exports/Mean.SE.ByCrop.Intervention.testing.jpeg)
*Figure 5: Mean +/- SE of testing scores by crop (click to enlarge).*

![Mean testing scores by region](figure%20exports/Mean.SE.ByRegion.Intervention.testing.jpeg)
*Figure 6: Mean +/- SE of testing scores by region (click to enlarge).*

![Environmental Performance Index vs Intervention Score](figure%20exports/EnvPerformance.vs.Overall.intervention.testing.score.jpeg)
*Figure 7: Country-level mean intervention score (total score across all interventions) against the [Yale Environmental Performance Index](https://epi.yale.edu). (Note: Total intervention scores against GDP, agricultural R&D spending, etc., can be viewed in the figure exports folder. Next steps include modelling as GLMMs).*

---

## 📂 Repository Structure

### Folders:
* 📁 `Analysis/raw data/` — Raw survey data as submitted by contributors
* 📁 `Analysis/clean data/` — Cleaned and scored survey data
* 📁 `Analysis/` — Core analysis scripts 
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
