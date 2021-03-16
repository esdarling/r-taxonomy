library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)


# #load material species --------------------------------------------------

data <- read_excel(here("data", "Yemen and UAE Perilampus-Darwin Core Revised for Emily.xlsx")) %>% 
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

summary <- data %>% 
  filter(species == "rainerius") %>%
  mutate(id = glue("{object_number}-{repository}"), 
         locality_id = glue("{locality}, {latitude}, {longitude}")) %>% 
  group_by(country, 
           locality_id, 
           date_label, 
           collector_s,
           collecting_method, 
           sex) %>% 
  summarize(n = n(), 
            specimens = paste(id, collapse = "; ")) %>% 
  mutate(sex = case_when(
    sex == "male" & n > 1 ~ "males", 
    sex == "female" & n > 1 ~ "females", 
    TRUE ~ sex), 
    specimens_by_sex = glue("{n} {sex}: {specimens}")) %>% 
  #names() %>% 
  select(-sex, 
         -n, 
         -specimens)
  
summary

locality_list <- unique(summary$locality_id)
locality_list



# full loop ---------------------------------------------------------------
species_list <- unique(data$species)

for (i in species_list) {
  species_data <- data %>%  
    filter(species == i)
  #filter(species == "rainerius")
  print(i)
  
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

for (k in locality_list) {
  
  locality_specimens <- summary %>%
    mutate(locality_id = as.character(locality_id)) %>%
    filter(locality_id == k) %>%
    #filter(str_detect(locality_id, "Al Houbara")) %>%
    mutate(date_collector_method_specimens = glue("{date_label}, {collector_s}, {collecting_method}, ({specimens_by_sex})")) %>%
    ungroup() %>%
    summarize(x = paste(date_collector_method_specimens, collapse = "; "))
  
  locality_specimens$x
  
  print(glue("{k}: {locality_specimens$x}"))
}
}
}











  
           


