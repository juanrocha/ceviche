library(tidyverse)


## read raw data for exploration
# dat <- googlesheets4::read_sheet(
#     "https://docs.google.com/spreadsheets/d/1GiCDbKRnGq7WfFOLwoUvrOCHkWGWy2ofbGqtOHBGCX4/edit#gid=1404296305")

## data file from email:
dat <- read_csv("data/data.csv")

dat %>% skimr::skim()

# fix names
dat <- dat %>%
    rename(id = `...1`) |> 
    janitor::clean_names() 

names(dat)
# prelim vis: looking for strong correlations

dat %>%
    select(starts_with("export")) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>%
    select(starts_with("export")) %>% 
    map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## The imports / exports are measured in US dollars, probably they need to be log transformed. 
## Suggestion: use biomass when possible
## Suggestion: remove reexports, it's mainly zeroes, or engineer a feature that is reexports > 0 

# governance indicators
dat %>% 
    select(6:11) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))
## strong correlations in gov_effect, regu_qual, rule_law: what makes more sense to keep?

# fish and aquaculture vars
dat %>% 
    select(5,13,14,15,24,26:27) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## social stats: population stats needs transformations
dat %>% 
    select(12,16:22, 25, 28,29) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>% 
    select(23, 24,30:33) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>% 
    select(34:41) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>% 
    select(41:45) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>% 
    select(46:48) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## log-transform fish_consumption_total

dat %>%
    select(52:62) %>%
    #    map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## check if gdp is used to calculated HDI? strong relationship
## strong correlation urban/rural pop. Chose one, keep coastal in anycase
## valueadded_agriforestfish = not explained in meta-data

ggplot(dat, aes(valueadded_agriforestfish)) + geom_density() + scale_x_log10()
ggplot(dat, aes(fish_consumption_per_capita_kg_per_year)) + geom_density() 
ggplot(dat, aes(netmigration)) + geom_density() # clarify units

## strong correlations between total, marine, and freshwater aquaculture productions
## clarify units (biomass?), use total, and as secondary variable a ratio marine/freshwater
# dat %>%
#     select(62:70) %>% 
#     map_df(., log1p) %>% 
#     GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## very strong correlations (1 pretty much), not sure is useful
# dat %>%
#     select(starts_with("freshwater")) %>% 
#     #    map_df(., log1p) %>% 
#     GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## strong correlations with total, but the groups do have some information. Mollusca and invertebrata are strongly correlated. Log-transform improves correlations.
dat %>%
    select(starts_with("marine")) %>% 
    map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## zero inflated distributions: check what transformation alleviates skewness
dat %>%
    select(starts_with("brackish")) %>% 
    #map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## No idea what gentry is, incomplete info on metadata file
dat %>%
    select(starts_with("gentry")) %>% 
    #map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## the indexes although new are hard to interpret because they are in themselves summary of other hidden dimensions. Strongly correlated epi and htl.
dat %>%
    select(87:91, 98:100) %>% 
    mutate(
        eez_area_km_seas_around_us = log1p(eez_area_km_seas_around_us),
        water_stress_index_fao = log1p(water_stress_index_fao),
        land_equipped_for_irrigation_fao_most_recent_year_1000ha = log1p(land_equipped_for_irrigation_fao_most_recent_year_1000ha),
        inland_water_area_fao_2017_1000ha = log1p(inland_water_area_fao_2017_1000ha),
        coastline_length_km_cia_world_fact_book = log1p(coastline_length_km_cia_world_fact_book)
    ) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

# needs transformation
ggplot(dat, aes(water_stress_index_fao)) + geom_density() + scale_x_log10()
ggplot(dat, aes(land_equipped_for_irrigation_fao_most_recent_year_1000ha)) + geom_density() + scale_x_log10()
ggplot(dat, aes(inland_water_area_fao_2017_1000ha)) + geom_density() + scale_x_log10()

names(dat)


## Some cleaning
zero_inflated <- c(
    "export_sum_value_2019", "import_sum_value_2019",
    "capture_fisheries_production_y", "valueadded_agriforestfish", "total_aq_production" ,
    "brackish_production_ratio", "water_stress_index_fao" ,"land_equipped_for_irrigation_fao_most_recent_year_1000ha" ,
    "inland_water_area_fao_2017_1000ha", "coastline_length_km_cia_world_fact_book" ) 
needs_logtrans <- c("gdp","eez_area_km_seas_around_us")


dat_pro <- dat %>% 
    # get rid of vars with missing values
    select(-prevalence_of_anemia_among_women_of_reproductive_age_15_49_years,
           -per_capita_food_supply_variability_kcal_cap_day ,
           -prevalence_of_undernourishment_percent_3_year_average,
           -domestic_supply_seafood_tonnes_2018, -population, -popdensity, 
           -total_seafood_consumption, -ruralpop, -coastalpop) %>%
    # get rid of heavily correlated vars and zero inflated
    select(-rule_law, -hdi, -hlt_new) %>% 
    # log-transform:
    mutate(across(.cols = everything(), fns = forecast::BoxCox)) %>%  
    ## center to zero mean and unit variance
    mutate(
        across(
            where(is.numeric), 
            function(x) { 
                z <- (x-mean(x, na.rm = TRUE) ) / sd(x, na.rm = TRUE)
                return(z)
            })
    )


## Correlations:
dat_pro %>% select(where(is.numeric)) %>% 
    cor() %>% 
    gplots::heatmap.2(
        trace = "none", margins = c(12,12),
        col = RColorBrewer::brewer.pal(10, "RdBu"),
        keysize = 1.2, key.title = "",
        cexRow = 0.5, cexCol = 0.5)

dat_pro %>% 
    select(15:25) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))








## save a copy of the data in disk. Only update when the team update online.
## Current version 210208
#save(dat, file = "data/data.RData")
