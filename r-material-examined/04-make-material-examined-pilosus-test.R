#test with full Perilampus dataset

#cool thing would be to convert into a rmarkdown document with word output

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)

?fct_recode

data <- read_excel(here("r-material-examined", "data", 
                        "hyalinus group for Material Examined_5July2024b.xlsx"), 
                   guess_max = 10000) %>% 
  clean_names() 

data <- data %>% 
  rename("species" = specific_epithet, 
         "type" = types) %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), #remove any leading/trailing whitespace
         sex = str_to_lower(sex)) %>% 
  filter(!type %in% c("Holotype", "Neotype")) %>% 
  mutate(type = case_when(type == "AME" ~ "Additional Material Examined", 
                          is.na(type) ~ "Regular Specimens", 
                          TRUE ~ type)) %>% 
  mutate(#county = str_replace(county, "\\bCo\\.", "County"), 
         county = str_replace_all(county, "(\\w+)\\.$", "\\1")) #removes any final period, from chatGPT
         #locality = str_remove_all(locality, "\\[|\\]|\\(|\\)")) #removes all brackets from locality



#list out counties for dad to check
data %>%
  select(country,
         county) %>%
  distinct() %>%
  arrange(country, county) %>%
  write_csv(here("r-material-examined", "county-to-check-5july2024.csv"))


data %>% 
  tabyl(species)

data %>% 
  tabyl(type) %>%
  pull(type)

names(data)
glimpse(data)

#Create DCD locality
#Add national park column

data <- data %>% 
  mutate(dcd_locality = case_when(is.na(park) ~ glue("{locality}"),
                                  TRUE ~ glue("{park}, {locality}")),
         dcd_locality = case_when(is.na(verbatim_latitude) ~ glue("{dcd_locality}"),
                                  TRUE ~ glue("{dcd_locality}, {verbatim_latitude}, {verbatim_longitude}")))


head(data$dcd_locality)
data %>% 
  filter(!is.na(park)) %>% 
  select(dcd_locality)

# #country summaries by species, sex -----------------------------------------------------
data %>% 
  group_by(species, 
           country, 
           sex) %>% 
  count() %>% 
  pivot_wider(id_cols = c("country", 
                          "species"), 
              names_from = sex, 
              values_from = n, 
              values_fill = 0)

# specimen summary-----------------------------------------------------
names(data)

#check type
data %>% 
  tabyl(type)

#add state/province
data %>% 
  tabyl(state_province)

data <- data %>% 
  select(species, 
         type, 
         object_number, 
         repository, 
         bin_number_coi, 
         its2,
         sex, 
         country,
         state_province, 
         county,
         dcd_locality) %>% 
  mutate(#type = case_when(!type %in% c("Paratype", "Holotype")  ~ "Additional material examined", 
                          #TRUE ~ type), 
        type = fct_explicit_na(type, "Other specimens"))
  # mutate(type = fct_recode(type, "Additional material examined"= "AME"), 
  #        type = fct_relevel(type, c("Holotype", 
  #                                  "Paratype", 
  #                                  "Other specimens",
  #                                  "Additional material examined")))

data %>% 
  tabyl(type)

data %>% 
  tabyl(county)

# Make specimen summary ---------------------------------------------------
options(dplyr.summarise.inform = FALSE)

names(data)

summary_specimens <- data %>% 
  #filter(!is.na(type)) %>% 
  mutate(id = case_when(!is.na(its2) ~ glue("{object_number}-{repository}; {bin_number_coi}; {its2}"),
                        !is.na(bin_number_coi) ~ glue("{object_number}-{repository}; {bin_number_coi}"),
                         TRUE ~ glue("{object_number}-{repository}"))) %>% 
  group_by(species, 
           type,
           country, 
           state_province, 
           county,
           dcd_locality, 
           #verbatim_date, 
           #collector_s,
           #collecting_method, 
           sex) %>% 
  summarize(n = n(), 
            specimen_id = paste(id, collapse = "; ")) %>% 
  mutate(#type = as.character(type), 
         sex = case_when(
           sex == "male" & n > 1 ~ "males", 
           sex == "female" & n > 1 ~ "females", 
           TRUE ~ sex)) %>% 
  mutate(specimens_by_sex = #case_when( 
    #type == "Holotype" ~ glue("{sex}: {specimen_id}"), #don't put '1' when it is a Holotype
    #TRUE ~ 
      glue("{n} {sex}: {specimen_id}")) %>% 
  group_by(species, 
           type, 
           country, 
           state_province, 
           county, 
           dcd_locality,
           #type, 
           #verbatim_date
           ) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = ". "))

summary_specimens
glimpse(summary_specimens)
head(summary_specimens$specimens)

summary_specimens %>% 
  pull(specimens)

# full loop ---------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)

#test with one species
# data <- data %>%
#   filter(species == "hyalinus")

species_list <- data %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) %>% 
  pull(species)

species_list

  
for (i in species_list) {
  cat(" \n")
  print(glue("Perilampus {i}"))
  
  species_data <- data %>%  
    filter(species == i)

  type_list <- species_data %>% 
    select(type) %>% 
    distinct(type) %>% 
    arrange(type) %>% 
    pull(type)
  
  type_list
  
  for (j in type_list) {
    #j <- "Additional Material Examined"
    cat(" \n")
    print(glue("{j}"))
    
    country_list <- species_data %>% 
      filter(type == j) %>% 
      distinct(country) %>%
      arrange(country) %>% 
      pull(country)
    
    country_list
    
    for (k in country_list) {
      #k <- "Canada"
      specimens_sex_country <- species_data %>%
        filter(species == i &
                 type == j &
                 country == k) %>% 
        group_by(species, 
                 country, 
                 sex) %>% 
        count() %>% 
        mutate(sex = case_when(
          n > 1 ~ glue("{n} {sex}s"), 
          TRUE ~ glue("{n} {sex}"))) %>% 
        group_by(species, 
                 country) %>%
        summarize(country_summary = paste(sex, collapse = ", "))
      
      specimens_sex_country
      specimens_sex_country$country_summary
      
      state_list <- species_data %>%
        filter(species == i &
                 type == j &
                 country == k) %>% 
        distinct(state_province) %>% 
        arrange(state_province) %>% 
        pull(state_province)
      
      state_list
      
      cat(" \n")
      print(glue("{k}: {specimens_sex_country$country_summary}. "))
      
      
      for (l in state_list) {
        #l <- "Ontario"
        specimens_sex_state <- species_data %>%
          filter(species == i &
                   type == j &
                   country == k & 
                   state_province == l) %>% 
          group_by(species, 
                   type,
                   country, 
                   state_province, 
                   sex) %>% 
          count() %>% 
          mutate(sex = case_when(
            n > 1 ~ glue("{n} {sex}s"), 
            TRUE ~ glue("{n} {sex}"))) %>% 
          group_by(species, 
                   type, 
                   country, 
                   state_province) %>%
          summarize(state_summary = paste(sex, collapse = ", "))
        
        specimens_sex_state
        specimens_sex_state$state_summary
        
        print(glue("{l}: {specimens_sex_state$state_summary}. "))
        
        if (k %in% c("USA", "Canada")) {
          #k <- "Canada"
          county_list <- species_data %>%
            filter(species == i &
                     type == j &
                     country == k & 
                     state_province == l) %>% 
            distinct(county) %>% 
            arrange(county) %>% 
            pull(county)
          
          county_list
          
          for (m in county_list) {
            # Print the selected county information before specimens
            print(glue("{m}., "))
            
            
            locality_specimens <- summary_specimens %>%
              filter(species == i &
                       type == j &
                       country == k & 
                       state_province == l & 
                       county == m) %>% 
              group_by(county, 
                       dcd_locality) %>% 
              summarize(specimens = paste(specimens, collapse = "; ")) %>% 
              mutate(specimens = paste(glue("{dcd_locality}: ({specimens})")))

            print(glue("{locality_specimens$specimens}. "))
          }
          
        } else {
          
          locality_specimens_no_county <- summary_specimens %>%
            filter(species == i &
                     type == j &
                     country == k & 
                     state_province == l) %>% 
            group_by(dcd_locality) %>% 
            summarize(specimens = paste(specimens, collapse = "; ")) %>% 
            mutate(collecting_summary = glue("({specimens})")) %>% 
            group_by(dcd_locality) %>% 
            summarize(collecting_summary_by_locality = paste(collecting_summary, collapse = "; ")) %>% 
            mutate(locality_summary = glue("{dcd_locality}: {collecting_summary_by_locality}")) %>% 
            summarize(to_print = paste(locality_summary, collapse = "; ")) %>% 
            select(to_print)

          
          print(glue("{locality_specimens_no_county}. ")) 
        }
      }
    }
  }
 }


