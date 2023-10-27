#test with full Perilampus dataset

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)

data <- read_excel(here("r-material-examined", "data", 
                        "pilosus for Emily - a few bogus red and yellow.xlsx"), 
                   guess_max = 10000) %>% 
  clean_names() 

data
names(data)
glimpse(data)

data <- data %>% 
  rename("species" = specific_epithet) %>% 
  #filter(species == "dobnos") %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), #remove any leading/trailing whitespace
         sex = str_to_lower(sex)) 

glimpse(data)
glimpse(data$locality)

data %>% 
  tabyl(species)

#Weird slashes in verbatim lat/longs? 
glimpse(data$verbatim_latitude)

library(textclean)
vec2 <- replace_non_ascii(data$verbatim_latitude)
head(vec2)

#Dunno re: weird slashes - remove in word..

data <- data %>% 
  mutate(dcd_locality = case_when(is.na(verbatim_latitude) ~ glue("{locality}"),
                                  TRUE ~ glue("{locality}, {verbatim_latitude}, {verbatim_longitude}")))


head(data$dcd_locality)

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

#add state/province
data %>% 
  tabyl(state_province)

data %>% 
  select(species, 
         object_number, 
         repository, 
         bin_number_coi, 
         sex, 
         country,
         state_province, 
         dcd_locality) %>% 
  glimpse()


# Make specimen summary ---------------------------------------------------
options(dplyr.summarise.inform = FALSE)

names(data)

summary_specimens <- data %>% 
  mutate(id = case_when(!is.na(bin_number_coi) ~ glue("{object_number}-{repository}; {bin_number_coi}"),
                         TRUE ~ glue("{object_number}-{repository}"))) %>% 
  group_by(species, 
           country, 
           state_province, 
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
           country, 
           state_province, 
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

species_list <- data %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) %>% 
  pull(species)

species_list


#for (i in species_list) {

for (i in species_list) {
  
  #i <- "pilosus"
  print(glue("Perilampus {i}"))
  
  species_data <- data %>%  
    filter(species == i)
  
  species_data %>% 
    tabyl(species)
  
  #create country list for species and type
  country_list <- species_data %>% 
    distinct(country) %>%
    arrange(country) %>% #alphabetical
    pull(country)
  
  country_list
  
  for (j in country_list) {
      
      #j <- "Mexico"
      
      #summarize specimens by country
      specimens_sex_country <- species_data %>%
        filter(species == i &
                 country == j) %>% 
        group_by(species, 
                 country, 
                 #type, 
                 sex) %>% 
        count() %>% 
        mutate(sex = case_when(
          n > 1 ~ glue("{n} {sex}s"), 
          TRUE ~ glue("{n} {sex}"))) %>% 
        group_by(species, 
                 #type, 
                 country) %>%
        summarize(country_summary = paste(sex, collapse = ", "))
      
      specimens_sex_country
      specimens_sex_country$country_summary
      
      print(glue("{j}: {specimens_sex_country$country_summary}."))
      
      #create state list
      state_list <- species_data %>%
        filter(species == i &
                 country == j) %>% 
        distinct(state_province) %>% 
        arrange(state_province) %>% #alphabetical
        pull(state_province)
      
      state_list
      
      for (k in state_list) {
        
      #k <- "Baja California Sur"
      
      #summarize specimens by state/province
      specimens_sex_state <- species_data %>%
        filter(species == i &
                 country == j & 
                 state_province == k)  %>% 
        group_by(species, 
                 country, 
                 state_province, 
                 #type, 
                 sex) %>% 
        count() %>% 
        mutate(sex = case_when(
          n > 1 ~ glue("{n} {sex}s"), 
          TRUE ~ glue("{n} {sex}"))) %>% 
        group_by(species, 
                 #type, 
                 country, 
                 state_province) %>%
        summarize(state_summary = paste(sex, collapse = ", "))
      
      specimens_sex_state
      specimens_sex_state$state_summary
      
      #make locality and specimens text
      
      locality_specimens <- summary_specimens %>%
        filter(species == i & 
                 country == j & 
                 state_province ==k) %>% 
        group_by(dcd_locality) %>% 
        summarize(specimens = paste(specimens, collapse = "; ")) %>% 
        #mutate(collecting_summary_by_date = glue("{verbatim_date} ({specimens})")) %>% 
        mutate(collecting_summary = glue("({specimens})")) %>% 
        group_by(dcd_locality) %>% 
        summarize(collecting_summary_by_locality = paste(collecting_summary, collapse = "; ")) %>% 
        mutate(locality_summary = glue("{dcd_locality}: {collecting_summary_by_locality}")) %>% 
        ungroup() %>% 
        summarize(locality_final = paste(locality_summary, collapse = ". ")) %>% 
        pull(locality_final) 
      
      locality_specimens
      
      #print sex tally only for non-type records
      #j <- "Material examined"
      
      print(glue("{k}: {specimens_sex_state$state_summary}. {locality_specimens}."))
      }}}


#copy output into a Word file (I know..)
