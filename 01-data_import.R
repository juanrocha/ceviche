library(tidyverse)


## read raw data for exploration
dat <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1GiCDbKRnGq7WfFOLwoUvrOCHkWGWy2ofbGqtOHBGCX4/edit#gid=1404296305")

dat %>% skimr::skim()

# fix names
dat <- dat %>%
    janitor::clean_names() 

# prelim vis: looking for strong correlations

dat %>%
    select(starts_with("export")) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>%
    select(starts_with("exports")) %>% 
    map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## The imports / exports are measured in US dollars, probably they need to be log transformed. 
## Suggestion: use biomass when possible
## Suggestion: remove reexports, it's mainly zeroes, or engineer a feature that is reexports > 0 

dat %>%
    select(4:11) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## strong correlations in gov_effect, regu_qual, rule_law: what makes more sense to keep?
## log-transform fish_consumption_total

dat %>%
    select(52:62) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## check if gdp is used to calculated HDI? strong relationship
## strong correlation urban/rural pop. Chose one, keep coastal in anycase
## valueadded_agriforestfish = not explained in meta-data

ggplot(dat, aes(valueadded_agriforestfish)) + geom_density() + scale_x_log10()
ggplot(dat, aes(fish_consumption)) + geom_density() 
ggplot(dat, aes(netmigration)) + geom_density() # clarify units

## strong correlations between total, marine, and freshwater aquaculture productions
## clarify units (biomass?), use total, and as secondary variable a ratio marine/freshwater
dat %>%
    select(62:70) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## very strong correlations (1 pretty much), not sure is useful
dat %>%
    select(starts_with("freshwater")) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## strong correlations with total, but the groups do have some information. Mollusca and invertebrata are strongly correlated. Log-transform improves correlations.
dat %>%
    select(starts_with("marine")) %>% 
    map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## zero inflated distributions: check what transformation alleviates skewness
dat %>%
    select(starts_with("brackish")) %>% 
    map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

## No idea what sentry is, incomplete info on metadata file
dat %>%
    select(starts_with("gentry")) %>% 
    # map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

dat %>%
    select(87:91, 98:100) %>% 
    # map_df(., log1p) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.15)))

names(dat)

## save a copy of the data in disk. Only update when the team update online.
## Current version 210208
save(dat, file = "data/data.RData")
