# IBRC Synthesis Paper: Bats as Pest Suppressors

Our global synthesis paper summarises published and unpublished knowledge on insectivorous bats as pest suppressors in agricultural systems, and the interventions trialled to support them.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

## Overview of Data
Contributors were asked to complete two surveys to gather knowledge about the research conducted in their study systems. A **study system** is defined as a crop type x region combination in which studies were done.

| Survey | Description |
|---|---|
| # 1: Knowledge scoring | Places each study system within our conceptual "knowledge pathway"<br><br>![Our conceptual knowledge pathway](figure%20exports/Our%20Conceptual%20Knowledge%20Pathway_with%20questions.png)<br>*Figure 1: Conceptual knowledge pathway used to score study systems (see Script 0).* |
| # 2: Implementation scoring | Gathers information on what interventions have been implemented and tested for their effects on bats in each study system |

## Maps showing the global coverage of our study systems

- [Interactive study system map (with dropdown options to colour by specific crop, crop type, country or world region)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/survey%20map_drop%20down%20options.html)
- [Static study system map (coloured by crop type)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/teardrop%20map_coloured%20by%20crop.html)
- [Static study system map (consistent colour)](https://rani-davis.github.io/IBRC-Synthesis-Paper_bats-as-pest-suppressors/map%20exports/teardrop%20map_consisent%20colour.html)

## Key Figures
![Knowledge pathway results](figure%20exports/Draft_composite%20knowledge%20pathway%20plot_By%20Crop%20Type.png)
*Figure 2: Knowledge pathway progress (Evidence → Implementation) across crop types, showing mean maturity scores (± SE) with raw respondent-level data points.*

## Repository Structure

| Script | Purpose |
|--------|---------|
| `Script 0` | Visualises our conceptual knowledge pathway |
| `Map Script 1: make map.R` | Creates maps to show spatial coverage of study systems in the dataset |
| `Script 1` | Data wrangling for Survey 1 (Knowledge scoring) |
| `Script 2` | Produces figures summarising Survey 1 scores |
| `Script 3` | Extracts socio-economic, environmental performance, and R&D spending indices; visualises against Survey 1 scores |
| `Script 4` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 5` | Produces figures summarising Survey 2 scores |
| `Script 6` | Extracts socio-economic, environmental performance, and R&D spending indices; visualises against Survey 2 scores |

**Folders:**

- `raw data/` — raw survey data as submitted by contributors
- `clean data/` — cleaned and scored survey data
- `figure exports/` — all key figures produced by the analysis scripts
- `docs/map exports/` — maps showing spatial coverage of study systems