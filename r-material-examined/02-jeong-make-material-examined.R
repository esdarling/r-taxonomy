#test with full Perilampus dataset

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)

data <- read_excel(here("r-material-examined", "data", 
                        "P hyalinus group export - J Yoo thesis spec 2022-11-22 testing (1).xlsx"), 
                   range = "A1:AO118", 
                   guess_max = 10000) %>% #set range because header on second row
  clean_names() 

data
names(data)
glimpse(data)

data <- data %>% 
  rename("species" = specific_epithet) %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), #remove any leading/trailing whitespace
         sex = str_to_lower(sex)) 

glimpse(data)
glimpse(data$locality)

data %>% 
  tabyl(species)

#use hymenoptera_on_line_locality
data %>% 
  select(contains("locality")) 

#make dcd_locality from original columns
#don't use hol_locality

names(data)
data <- data %>% 
  # filter(species == "dobnos" & 
  #          country == "Costa Rica") %>% 
  # select(park, 
  #        locality,
  #        verbatim_latitude, 
  #        verbatim_longitude, 
  #        elevation) %>%
  mutate(location_info = case_when(!is.na(park) ~ glue("{park}, {locality}"),
                                  TRUE ~ glue("{locality}")), 
         dcd_locality = case_when(!is.na(verbatim_latitude) ~ 
                                    glue("{location_info}, {verbatim_latitude}, {verbatim_longitude}"),
                                  TRUE ~ glue("{location_info}")))
data %>% 
  select(location_info) %>%
  distinct(location_info) %>%
  view()

data %>% 
  select(dcd_locality) %>%
  distinct(dcd_locality) %>%
  view()


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

#save for dad to check 
 # .Last.value %>% 
 #   write_csv(here("r-material-examined/outputs", "P hyalinus-by-country.csv"))
 # 

# specimen summary with dobnos-----------------------------------------------------
names(data)

# Select columns for use --------------------------------------------------

#add state/province
data %>% 
  tabyl(state_province)

data %>% 
  tabyl(verbatim_date)  #oh boy

names(data)

summary_specimens <- data %>% 
  select(species, 
         object_number, 
         repository, 
         bin_number, 
         host_id,
         sex, 
         country,
         state_province, 
         dcd_locality, 
         verbatim_date) %>%
  # select(object_number, 
  #        repository, 
  #        bin_number, 
  #        host_id) %>% 
  # distinct() %>% 
  # view()
  mutate(id = case_when(!is.na(repository) & !is.na(host_id) & !is.na(bin_number) ~
                          glue("{object_number}-{repository}, {host_id}, {bin_number}"),
                        !is.na(repository) & !is.na(host_id) & is.na(bin_number) ~
                          glue("{object_number}-{repository}, {host_id}"), 
                        !is.na(repository) & is.na(host_id) & !is.na(bin_number) ~
                          glue("{object_number}-{repository}, {bin_number}"), 
                        !is.na(repository) & is.na(host_id) & is.na(bin_number) ~
                          glue("{object_number}-{repository}"),
                        TRUE ~ glue("{object_number}"))) %>% 
  # select(id) %>% 
  # distinct(id) %>% 
  # view() 
group_by(species, 
           country, 
           state_province, 
           dcd_locality, 
           verbatim_date, 
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
           verbatim_date) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = ". "))

summary_specimens
glimpse(summary_specimens)
head(summary_specimens$specimens)

summary_specimens

# full loop ---------------------------------------------------------------

species_list <- data %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) %>% 
  pull(species)

species_list

#for (i in species_list) {


for (i in species_list) {
#for (i in "dobnos") {  
  
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
  
  for (j in country_list) {
      
      #j <- "Costa Rica"
      
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
      
      for (k in state_list) {
        
      #k <- "Guanacaste"
      
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
        group_by(dcd_locality,
                 verbatim_date) %>% 
        summarize(specimens = paste(specimens, collapse = "; ")) %>% 
        mutate(collecting_summary_by_date = glue("{verbatim_date} ({specimens})")) %>% 
        group_by(dcd_locality) %>% 
        summarize(collecting_summary_by_locality = paste(collecting_summary_by_date, collapse = "; ")) %>% 
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