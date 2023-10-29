#test with full Perilampus dataset

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)

data <- read_excel(here("r-material-examined", "data", 
                        "pilosus for Emily - a few bogus red and yellow DCD update Oct27.xlsx"), 
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
#Don't copy over into Google docs file :) 
glimpse(data$verbatim_latitude)

library(textclean)
vec2 <- replace_non_ascii(data$verbatim_latitude)
head(vec2)

#Create DCD locality
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
         dcd_locality) %>% 
  mutate(#type = case_when(!type %in% c("Paratype", "Holotype")  ~ "Additional material examined", 
                          #TRUE ~ type), 
        type = fct_recode(type, "Additional material examined"= "AME"), 
        type = fct_relevel(type, c("Holotype", "Paratype", "Additional material examined"))) %>% 
  glimpse()

data %>% 
  tabyl(type)

# Make specimen summary ---------------------------------------------------
options(dplyr.summarise.inform = FALSE)

names(data)

#remove blank type (problematic specimen, don't include in material examined)

summary_specimens <- data %>% 
  filter(!is.na(type)) %>% 
  mutate(id = case_when(!is.na(its2) ~ glue("{object_number}-{repository}; {bin_number_coi}; {its2}"),
                        !is.na(bin_number_coi) ~ glue("{object_number}-{repository}; {bin_number_coi}"),
                         TRUE ~ glue("{object_number}-{repository}"))) %>% 
  group_by(species, 
           type,
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
           type, 
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

#look at paratype vs others
data %>% 
  tabyl(type)


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
  
  #create type list for species - should all be the same but who knows
  
  type_list <- species_data %>% 
    select(type) %>% 
    distinct(type) %>% 
    arrange(type) %>% 
    pull(type)
  
  type_list
  
  for (j in type_list) {
  
  #j <- "Holotype"
  #create country list for species and type
  
    print(glue("{j}"))
  
    country_list <- species_data %>% 
      filter(type == j) %>% 
      distinct(country) %>%
      arrange(country) %>% #alphabetical
      pull(country)
    
    country_list
    
  for (k in country_list) {
      
      #k <- "USA"
      
      #summarize specimens by country
      specimens_sex_country <- species_data %>%
        filter(species == i &
                 type == j &
                 country == k) %>% 
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
      
      print(glue("{k}: {specimens_sex_country$country_summary}."))
      
      #create state list
      state_list <- species_data %>%
        filter(species == i &
                 type == j &
                 country == k) %>% 
        distinct(state_province) %>% 
        arrange(state_province) %>% #alphabetical
        pull(state_province)
      
      state_list
      
      for (l in state_list) {
        
      #l <- "Texas"
      
      #summarize specimens by state/province
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
      
      #make locality and specimens text
      
      locality_specimens <- summary_specimens %>%
        filter(species == i &
                 type == j &
                 country == k & 
                 state_province == l) %>% 
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
      
      print(glue("{l}: {specimens_sex_state$state_summary}. {locality_specimens}."))
      }}}}


#copy output into a Word file (I know..)
