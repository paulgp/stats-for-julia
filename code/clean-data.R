
library(tidyverse)
library(stringr)
library(ggthemes)
library(tidycensus)
library(readxl)
library(viridis)
## key = Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html
key = "6dc01cb16b173a97a1215ea441de09bc0d2da25d"

acs_varnames  <- load_variables(2016, "acs5", cache = TRUE)

pull_vars <- c("B01001_002",
               "B01001_026",
               "B01002_001",
               "B01002A_001",
               "B01002B_001",
               "B01002C_001",
               "B01002D_001",
               "B01002E_001",
               "B01002F_001",
               "B01002G_001",
               "B01002H_001",
               "B01002I_001",
               "B02001_001",
               "B02001_002",
               "B02001_003",                              
               "B02001_004",
               "B02001_005",
               "B02001_006",
               "B02001_007",
               "B02001_008",
               "B02001_009",
               "B02001_010"               
               )


pull_vars_varnames <- acs_varnames %>%
    filter(name %in% pull_vars) %>%
    rename(variable = name)

blockgroup_data <- get_acs(geography = "block group",
                           variables = pull_vars,
                           state     = "PA",
                           county    = "Philadelphia",
                           key       = key,
                           cache     = TRUE
                           )

blockgroup_data <- blockgroup_data %>%
    left_join(by="variable", pull_vars_varnames)


CensusMapping <- read_excel("~/repos/stats-for-julia/raw-data/CensusMapping.xlsx", 
                            col_types = c( "numeric", "text","numeric", 
                                          "blank"), n_max = 28)
colnames(CensusMapping) <- c("division", "GEOID", "share")


race_data <- blockgroup_data %>%
    right_join(by="GEOID", CensusMapping) %>%
    filter(concept =="RACE") %>%
    separate(col = "label",
             into = c("A", "B", "C"),
             sep = "!!") %>%
    group_by(division, C) %>%
    mutate(scale_estimate = estimate * share) %>%
    summarize(total_scale= sum(scale_estimate),
              total = sum(estimate)) %>%
    rename(race = C) %>%
    mutate(race = replace_na(race, "Total")) %>%
    group_by(division) %>%
    filter(race != "Total") %>%
    mutate(all_total = sum(total), all_total_scale = sum(total_scale)) %>%
    mutate(share_frac = total / all_total,
           share_frac_scale= total_scale / all_total_scale) %>%
    ungroup()

g <-ggplot(data = race_data %>% filter(race != "Total") %>%
               mutate(division = as.factor(division)))

g + geom_col(aes(division,share_frac, fill=race)) +
    theme_tufte() 


ggsave("../output/figure_race.pdf")
