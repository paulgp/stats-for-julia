
library(tidyverse)
library(stringr)
library(ggthemes)

col_names <- names(read_csv('../raw-data/ACS_16_5YR_B02001_with_ann.csv', n_max = 0 ))
var_names <- read_csv('../raw-data/ACS_16_5YR_B02001_with_ann.csv', n_max = 1 )
data <- read_csv('../raw-data/ACS_16_5YR_B02001_with_ann.csv', col_names = col_names, skip = 2)



clean_data2 <- data %>% select(-contains('02_')) %>% gather(variable, value, starts_with("HD"), -GEO.id, -GEO.id2, -`GEO.display-label`) %>%
    select(-GEO.id, -GEO.id2) 
 

var_data <- var_names %>% select(-GEO.id, -GEO.id2, -`GEO.display-label`, -contains('02_')) %>%
    gather(variable, varname ) %>%
        mutate(varname = str_replace(varname, 'Estimate; ' , '')) %>%
            mutate(varname = str_replace(varname, ':' , '')) %>%
                mutate(varname = str_replace(varname, '- ' , ''))


clean_data <- inner_join(clean_data2, var_data, by='variable') %>% rename(location = `GEO.display-label`) %>% select(-variable)
clean_data <- clean_data %>% mutate(location = case_when(
                          location == "Pennsylvania" ~ "state",
                          location == "Philadelphia County, Pennsylvania" ~ "county",
                          TRUE ~ 'block_group')) 


totals <- clean_data %>% filter(varname == "Total") %>% rename(total = value) %>%
    select(-varname)

race_share <- clean_data %>% inner_join(totals, by='location') %>%
    mutate(frac = value / total) %>%
    select(-value, -total)    %>%
        spread(location,frac)  %>% filter(varname != "Total")  %>%
            mutate(varname = str_replace(varname, 'Total ', '')) %>%
                filter(!grepl('Two or more races: - Two races excluding', varname)) %>%
                    filter(!grepl('Two or more races: - Two races including', varname)) %>%
                        gather(location, value, c(block_group, county, state), -varname)



ggplot(data = race_share) +
    geom_col(aes(x = varname,y = value)) + theme_tufte() +
        coord_flip()  + facet_grid(. ~ location)

ggsave("../output/figure_race.pdf")
