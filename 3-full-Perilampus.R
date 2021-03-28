#test with full Perilampus dataset

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)


data <- read_excel(here("data", "Perilampus", 
                        "Copy of All Perilampus 2021.xlsx")) %>% 
  clean_names() %>% 
  rename("species" = specific_epithet) %>% 
  filter(species == "platigaster") %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), 
         sex = str_to_lower(sex)) 

#keep NA sex and count 
names(data)

#use hymenoptera_on_line_locality
data %>% 
  select(contains("locality")) 

#find-replace backslashes to delete in Word 
data %>% 
  select(hymenoptera_on_line_locality) %>% 
  head() %>% 
  print()
  
#why backslahses in lat? Remove later = easy answer
data %>% 
  select(contains("latitude"))

data %>% 
  tabyl(sex)

#check lat-longs
data %>% 
  tabyl(verbatim_latitude)


#no type yet
# data %>% 
#   tabyl(type)

data %>% 
  tabyl(species)

data %>% 
  tabyl(sex)

data %>% 
  tabyl(hymenoptera_on_line_locality)

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


# specimen summary with platogaster -----------------------------------------------------
names(data)

# Select columns for use --------------------------------------------------
names(data)

data %>% 
  select(contains())

data %>% 
  tabyl(verbatim_date)

data_use <- data %>% 
  select(species, 
         rome_number, 
         repository, 
         sex, 
         country, 
         hymenoptera_on_line_locality, 
         verbatim_date) %>% 
  rename("locality_id"= hymenoptera_on_line_locality)


# Make specimen summary ---------------------------------------------------
options(dplyr.summarise.inform = FALSE)

summary <- data_use %>% 
  #filter(species == "rainerius") %>%
  mutate(id = glue("{rome_number}-{repository}")) %>% #add notes column if present 
  #id = case_when(is.na(notes) ~ glue("{object_number}-{repository}"), 
  #TRUE ~ glue("{object_number}-{repository}, {notes}")
  group_by(species, 
           country, 
           locality_id, 
           verbatim_date, 
           #collector_s,
           #collecting_method, 
           sex) %>% 
  summarize(n = n(), 
            specimen_id = paste(id, collapse = ", ")) %>% 
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
           locality_id,
           #type, 
           verbatim_date) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = "; "))

head(summary$specimens)

# full loop ---------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)

species_list <- unique(data_use$species)

#for (i in species_list) {
for (i in species_list) {
  print(glue("Perilampus {i}"))
  
  #i <- "platigaster"
  
  species_data <- data_use %>%  
    filter(species == i)
  
  #create country list for species and type
  country_list <- species_data %>% 
    distinct(country) %>%
    arrange(country) %>% #alphabetical
    pull(country)
  
  for (j in country_list) {
      
      #j <- "Canada"
      
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
        summarize(specimens_by_sex = paste(sex, collapse = ", "))
      
      specimens_sex_country
      
      summary
      
      locality_specimens <- summary %>%
        filter(species == i & 
                 country == j) %>% 
        group_by(locality_id,
                 verbatim_date) %>% 
        summarize(specimens = paste(specimens, collapse = "; ")) %>% 
        mutate(collecting_summary_by_date = glue("{verbatim_date} ({specimens})")) %>% 
        group_by(locality_id) %>% 
        summarize(collecting_summary_by_locality = paste(collecting_summary_by_date, collapse = "; ")) %>% 
        mutate(locality_summary = glue("{locality_id}: {collecting_summary_by_locality}")) %>% 
        ungroup() %>% 
        summarize(locality_final = paste(locality_summary, collapse = ". ")) %>% 
        pull(locality_final) 
      
      locality_specimens
      
      #print sex tally only for non-type records
      #j <- "Material examined"
      
      print(glue("{j}: {specimens_sex_country$specimens_by_sex}. {locality_specimens}."))
      
    }}

















