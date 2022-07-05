library(tidyverse)
## See line 100 for summary of highly correlated variables to remove
## See line 135 for summary of highly skewed variables which were not easily transformed and need to be addressed



## read current raw data for exploration 
data <- read.csv("data.csv")
data$X <- NULL
#can take a brief look at histograms
data %>% skimr::skim()

# fix names
data <- data %>%
  janitor::clean_names() 

# prelim visualizations: looking for strong correlations
data %>%
  select(4:11) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

data %>%
  select(4:11) %>% 
  mutate(
    total_seafood_consumption = log1p(total_seafood_consumption)
    ) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
# Gov_effect, regu_qual, rule_law, control_corrupt, all strong correlations. 
# also need to transform total_seafood_consumption


#Stefan reduced down the export/import data. looking at that (also needs to be  transformed):
data %>%
  select(12:14) %>%
  map_df(., log1p) %>%
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
# trade balance 2019 (export_minus_import) strongly correlates to both export and import sum.
# So just include trade balance 2019 in the final analysis?

## exploring the rest of the variables: 
data %>%
  select(15:24) %>%
  #    map_df(., log1p) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
#need to transform, lots of zero-inflated datasets. see below
data %>%
  select(15:24) %>%
  map_df(., log1p) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
## HDI and GDP .942*** correlation. HDI includes GDP in index. Which to keep? HDI encompasses wider range of
## variables. But GDP easier to interpret in our results.
## Population and rural pop are somewhat strongly correlated (0.825***)
## value_added_agriforestfish strongly correlated with population (0.917***)


## now looking at aquaculture variables (reduced # variables from previous version)
data %>%
  select(25:33,40,45:47) %>% 
 # map_df(., log1p) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
#again, will need to transform, see below:
data %>%
  select(25:33,40,45:47) %>% 
   map_df(., log1p) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
## Other than total marine + marine species, other production types + # species aren't strongly correlated
## aquaculture production ratios, especially brackish and marine, very skewed/zero inflated. what to do?


## the indexes i don't think have changed any since the last version. Strongly correlated epi and htl.
##
## need to look at the new variables.
data %>%
  select(33:44) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

#checking out the new variables, columns 41:44
ggplot(data, aes(prevalence_of_anemia_among_women_of_reproductive_age_15_49_years)) + geom_density()
ggplot(data, aes(per_capita_food_supply_variability_kcal_cap_day)) + geom_density()
ggplot(data, aes(prevalence_of_undernourishment_percent_3_year_average)) + geom_density()
ggplot(data, aes(domestic_supply_seafood_tonnes_2018)) + geom_density() 

### From 33:44:
data %>%
  select(33:44) %>% 
  mutate(
    eez_area_km_seas_around_us = log1p(eez_area_km_seas_around_us),
    water_stress_index_fao = log1p(water_stress_index_fao),
    land_equipped_for_irrigation_fao_most_recent_year_1000ha = log1p(land_equipped_for_irrigation_fao_most_recent_year_1000ha),
    inland_water_area_fao_2017_1000ha = log1p(inland_water_area_fao_2017_1000ha),
    coastline_length_km_cia_world_fact_book = log1p(coastline_length_km_cia_world_fact_book),
    prevalence_of_anemia_among_women_of_reproductive_age_15_49_years = log1p(prevalence_of_anemia_among_women_of_reproductive_age_15_49_years),
    per_capita_food_supply_variability_kcal_cap_day = log1p(per_capita_food_supply_variability_kcal_cap_day),
    prevalence_of_undernourishment_percent_3_year_average = log1p(prevalence_of_undernourishment_percent_3_year_average),
    domestic_supply_seafood_tonnes_2018 = log1p(domestic_supply_seafood_tonnes_2018)
  ) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
## prevalence of anemia variable fairly strongly correlated with epi (-.772***) and hlt(-.817***)

### Notes from Stefan and Ben on variables to drop, November 9th
###Variables to consider dropping due to strong collinearity
data$gov_effect
data$regu_qual
data$rule_law
data$control_corrup
data$export_sum_value_2019
data$import_sum_value_2019
data$hdi #strongly correlates to gdp, gdp easier to interpret
data$population #strongly correlates to valueadded agriforestfish
data$coastline_length_km_cia_world_fact_book #OR data$eez_area_km_seas_around_us somewhat strongly correlated
data$hlt_new # or data$epi_new, both are strongly correlated 
## see below:
data %>%
  select(4:16,24,38,39) %>% 
  mutate(
    total_seafood_consumption = log1p(total_seafood_consumption),
    export_sum_value_2019 = log1p(export_sum_value_2019),
    import_sum_value_2019 = log1p(import_sum_value_2019),
    trade_balance_2019_export_minus_import = log1p(trade_balance_2019_export_minus_import),
    cri_score = log1p(cri_score),
    gdp = log1p(gdp),
    hdi = log1p(hdi),
    hlt_new = log1p(hlt_new),
    epi_new = log1p(epi_new)
  ) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
# Decision as of November 9th: 
# drop population (already have more meaningful population datasets)
# drop hlt (epi better from an interpretive/connection to aquaculture standpoint)
# drop export/import (both highly correlate to trade_balance_2019)
# drop regu_qual, rule_law, control_corrup (keep gov_effect)
# drop hdi


#Skewed datasets. From an interpretive standpoint, these distributions make sense. But will these highly skewed datasets
#impact the stability of our clustering analysis? Maybe Juan can help with interpreting this
data$marine_production_ratio
data$brackish_production_ratio
data$freshwater_production_ratio
data$percent_of_aqua_y
data$marine_species_count
data$brackishwater_species_count
data$prevalence_of_undernourishment_percent_3_year_average
#The following have negative values so log transformations won't work
#Leave as is? Is it a problem for these datasets to be heavily skewed?
data$ten_year_growth_rate
data$three_year_growth_rate
data$netmigration
data$trade_balance_2019_export_minus_import

## See below 
data %>%
  select(25:37,43,45:47) %>% 
  #map_df(., log1p) %>% 
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

#Dropping countries with NAs
library(tidyr)
dataNoNA <- data %>% drop_na()
write.csv(dataNoNA, "dataNoNA.csv")



