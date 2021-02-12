library(tidyverse)
library(tidymodels)


load("data/data.RData")

dat %>% 
    # get rid of heavily correlated vars and zero inflated
    select(-starts_with("reexports"), -regu_qual, -rule_law, -hdi, -urbanpop,
           -total_aq_production, -freshwater_amphibia_reptilia, -freshwater_crustacea,
           -freshwater_invertebrata, -freshwater_mollusca, -freshwater_pisces, 
           -freshwater_plantae, -marine_aq_total, -starts_with("brackish"),
           -starts_with("gentry")) %>% 
    # log-transform:
    mutate(
        across(starts_with("import"), log1p),
        across(starts_with("export"), log1p),
        fish_consumption_total = log1p(fish_consumption_total),
        # urbanpop = log1p(urbanpop), 
        ruralpop = log1p(ruralpop),
        coastalpop = log1p(coastalpop),
        valueadded_agriforestfish = log1p(valueadded_agriforestfish),
        eez_area_km_seas_around_us = log1p(eez_area_km_seas_around_us),
        water_stress_index_fao = log1p(water_stress_index_fao),
        land_equipped_for_irrigation_fao_most_recent_year_1000ha = 
            log1p(land_equipped_for_irrigation_fao_most_recent_year_1000ha),
        inland_water_area_fao_2017_1000ha = log1p(inland_water_area_fao_2017_1000ha),
        coastline_length_km_cia_world_fact_book = 
            log1p(coastline_length_km_cia_world_fact_book)) %>%  
    ## center to zero mean and unit variance
    mutate(
        across(where(is.numeric), scales::)
    )
