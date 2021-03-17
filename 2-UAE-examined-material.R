library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)


# #load material species --------------------------------------------------

data <- read_excel(here("data", "Yemen and UAE Perilampus-Darwin Core Revised for Emily 2.xlsx")) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  mutate(across(where(is.character), str_trim), 
         sex = str_to_lower(sex)) %>% 
  rename("species" = specific_epithet)

data

data %>% 
  tabyl(species)

data %>% 
  tabyl(sex)

data %>% 
  tabyl(locality)

data %>% 
  tabyl(country)


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


# specimen summary with rainerius -----------------------------------------------------
names(data)

data$verbatim_longitude
#extra backslashes? 
#str_remove(data$verbatim_latitude, "\\\\")

data %>% 
  filter(date_label == "1-31 Mar 2016") %>% 
  view()

summary <- data %>% 
  #filter(species == "rainerius") %>%
  mutate(id = glue("{object_number}-{repository}"), 
         locality_id = glue("{locality}, {latitude}, {longitude}")) %>% 
  group_by(species, 
           country, 
           locality_id, 
           date_label, 
           collector_s,
           collecting_method, 
           sex) %>% 
  summarize(n = n(), 
            specimen_id = paste(id, collapse = ", ")) %>% 
  mutate(sex = case_when(
    sex == "male" & n > 1 ~ "males", 
    sex == "female" & n > 1 ~ "females", 
    TRUE ~ sex), 
    specimens_by_sex = glue("{n} {sex}: {specimen_id}")) %>% 
  #names() %>% 
  select(-sex, 
         -n, 
         -specimen_id) %>% 
  group_by(species, 
           country, 
           locality_id, 
           date_label, 
           collector_s,
           collecting_method) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = "; "))
  
summary %>% 
  filter(date_label == "1-30 Jun 2016") %>% 
  view()
  
summary

summary %>% 
  group_by(species, 
           locality_id) %>% 
  count(date_label) %>% 
  arrange(-n)

# full loop ---------------------------------------------------------------
species_list <- unique(data$species)

#for (i in species_list) {
for (i in species_list) {
  print(glue("Perilampus {i}"))
  
  species_data <- data %>%  
    filter(species == i)

  country_list <- unique(species_data$country) 
  
  for (j in country_list) {
    
    sex_tally <- species_data %>% 
      filter(country == j) %>% 
      #filter(country != "United Arab Emirates") %>% 
      group_by(sex) %>% 
      count() %>% 
      mutate(sex_name = case_when(
        n > 1 ~ glue("{sex}s"), 
        TRUE ~ sex), 
        print_sex = glue("{n} {sex_name}")) %>% 
      ungroup() %>% 
      summarize(print = paste(print_sex, collapse = ", "))
    
    print(glue("{j}: {sex_tally}"))
    
    locality_list <- species_data %>% 
      filter(country == j) %>%
      distinct(locality) %>% 
      pull(locality)
    
 for (k in locality_list) {
  
  locality_specimens <- summary %>%
    mutate(locality_id = as.character(locality_id)) %>%
    filter(species == i & 
             str_detect(locality_id, k)) %>% 
    mutate(date_collector_method_specimens = glue("{date_label}, {collector_s}, {collecting_method} ({specimens})")) %>%
    ungroup() %>%
    summarize(x = paste(date_collector_method_specimens, collapse = "; ")) %>% 
    pull(x)
  
  print(glue("{k}: {locality_specimens}.")) }}}












  
           


