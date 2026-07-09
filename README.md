# IBRC Synthesis Paper: Bats as Pest Suppressors

Our global synthesis paper aims to summarise knowledge on the role of insectivorous bats as pest suppressors in agricultural systems.

This repository contains the raw survey data from all contributors, data-wrangling scripts, cleaned datasets, and scripts for visualising and analysing the data.

## Overview

Contributors were asked to complete two surveys to gather knowledge about the research conducted in their study systems. A **study system** is defined as a crop type x region combination in which studies were done.

- **Survey 1 (Knowledge scoring)** — places each study system within our conceptual "knowledge pathway"
- **Survey 2 (Implementation scoring)** — gathers information on what interventions have been implemented and tested for their effects on bats in each study system

In both surveys, key questions had pre-selected responses on a scale from 0 to 3.

## Repository Structure

| Script | Purpose |
|--------|---------|
| `Script 0` | Visualises our conceptual knowledge pathway |
| `Map Script 1: make map.R` | Analyses spatial coverage of study systems in the dataset |
| `Script 1` | Data wrangling for Survey 1 (Knowledge scoring) |
| `Script 2` | Produces figures summarising Survey 1 scores |
| `Script 3` | Extracts socio-economic, environmental performance, and R&D spending indices; visualises against Survey 1 scores |
| `Script 4` | Data wrangling for Survey 2 (Implementation scoring) |
| `Script 5` | Produces figures summarising Survey 2 scores |
| `Script 6` | Extracts socio-economic, environmental performance, and R&D spending indices; visualises against Survey 2 scores |

**Folders:**

- `raw data/` — raw survey data as submitted by contributors
- `figure exports/` — all key figures produced by the analysis scripts
- `map exports/` — maps showing spatial coverage of study systems