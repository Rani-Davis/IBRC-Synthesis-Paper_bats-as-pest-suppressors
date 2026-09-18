
library(dplyr);library(tidyverse)

# Attach bat richness data 
BatRichnessData <- read_csv("Analysis/bat richness data/Country level bat richness.csv") %>% mutate(RowID = row_number())

# Rename to a formula-safe name (no spaces) before using in any model
BatRichnessData <- BatRichnessData %>%
  rename(BatRichness = `Bat richness_extant`)

head(BatRichnessData)
str(BatRichnessData)

# Attach to Survey datasets
head(Survey_1_long_indices_wLatLong)
Survey_1_long_indices_wLatLong_wRichness <- Survey_1_long_indices_wLatLong %>%
  left_join(BatRichnessData %>% select(Country, BatRichness),
            by = c("Country.clean" = "Country"))
#View(Survey_1_long_indices_wLatLong_wRichness)
write.csv(Survey_1_long_indices_wLatLong_wRichness,"Analysis/clean data/Survey 1_scored_with indices_w Lat Long_w Richness.csv", row.names = FALSE)

head(Survey_2_long_indices_wLatLong)
Survey_2_long_indices_wLatLong_wRichness <- Survey_2_long_indices_wLatLong %>%
  left_join(BatRichnessData %>% select(Country, BatRichness),
            by = c("Country.clean" = "Country"))
#View(Survey_2_long_indices_wLatLong_wRichness)
write.csv(Survey_2_long_indices_wLatLong_wRichness,"Analysis/clean data/Survey 2_scored_with indices_w Lat Long_w Richness.csv", row.names = FALSE)

