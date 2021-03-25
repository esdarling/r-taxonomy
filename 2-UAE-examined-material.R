library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)


# #load material species --------------------------------------------------

data <- read_excel(here("data", "Yemen and UAE Perilampus-Darwin Core Revised for Emily 3.xlsx")) %>% 
  clean_names() %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), 
         sex = str_to_lower(sex)) %>% 
  rename("species" = specific_epithet) %>% 
  mutate(type = str_to_title(type), 
         type = case_when(
           is.na(type) ~ "Other material examined", 
           TRUE ~ type), 
         type = fct_relevel(type, 
                            "Holotype", 
                            "Paratype", 
                            "Other material examined")) %>% 
  mutate(lat_degree = parse_parts_lat(verbatim_latitude), 
         lon_degree = parse_parts_lon(verbatim_longitude)) %>%
  tidyr::unpack(c(lat_degree, lon_degree), 
                names_sep = "_") %>% 
  mutate(locality_id = glue("{locality}, {lat_degree_deg}degree{lat_degree_min}'N, {lon_degree_deg}degree{lon_degree_min}'E"))

data

data %>% 
  tabyl(species)

data %>% 
  tabyl(sex)

data %>% 
  tabyl(locality_id)

data %>% 
  tabyl(country)

data %>% 
  tabyl(type)


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



# holotypes paratypes -----------------------------------------------------
data %>% 
  tabyl(type)

data %>% 
  filter(!is.na(type)) %>% 
  group_by(species, 
           country, 
           sex, 
           date_label) %>% 
  count(type) %>% 
  arrange(-n) %>% 
  filter(type == "Holotype")


# specimen summary with rainerius -----------------------------------------------------
names(data)

data %>% 
  filter(date_label == "1-31 Mar 2016")

names(data)

summary <- data %>% 
  #filter(species == "rainerius") %>%
  mutate(id = glue("{object_number}-{repository}")) %>% 
  group_by(species, 
           country, 
           locality_id, 
           type, 
           date_label, 
           collector_s,
           collecting_method, 
           sex) %>% 
  summarize(n = n(), 
            specimen_id = paste(id, collapse = ", ")) %>% 
  mutate(type = as.character(type), 
         sex = case_when(
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
           type, 
           date_label, 
           collector_s,
           collecting_method) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = "; "))
  
summary %>% 
  filter(date_label == "1-30 Jun 2016") %>% 
  pull(specimens)
  
summary %>% 
  tabyl(type)

summary %>% 
  group_by(species, 
           locality_id, 
           type) %>% 
  count(date_label) %>% 
  arrange(-n) %>% 
  arrange(species, 
          type)

# full loop ---------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)

species_list <- unique(data$species)

#for (i in species_list) {
for (i in species_list) {
  print(glue("Perilampus {i}"))
  
  #i <- "rainerius"
  
  species_data <- data %>%  
    filter(species == i) %>% 
    arrange(type)

  type_list <- species_data %>% 
    select(type) %>% 
    distinct() %>% 
    mutate(type = as.character(type)) %>% 
    pull(type)
  
  for (j in type_list) {
  
    #j <- "Other material examined"
    
    sex_tally <- species_data %>% 
      filter(type == j) %>% 
      #filter(country != "United Arab Emirates") %>% 
      group_by(sex) %>% 
      count() %>% 
      mutate(sex_count = case_when(
        n > 1 ~ glue("{n} {sex}s"), 
        TRUE ~ glue("{n} {sex}"))) %>% 
      ungroup() %>% 
      summarize(specimens_by_sex = paste(sex_count, collapse = ", "))
    
    #sex_tally
    
    #print Material Examined in front of holotype.
    ifelse(j == "Holotype",
           print(glue("Material Examined. {j}. {sex_tally}.")), 
           print(glue("{j}. {sex_tally}.")))
    
    country_list <- species_data %>% 
      filter(type == j)  %>% 
      distinct(country) %>%
      arrange(country) %>% #alphabetical
      pull(country)
    
    for (k in country_list) {
      
    #k <- "United Arab Emirates"
    
    locality_specimens <- summary %>%
      filter(species == i & 
               type == j & 
               country == k) %>% 
      group_by(locality_id, date_label, collector_s, collecting_method) %>% 
    # mutate(record = glue("{locality_id} {date_label}, {collector_s}, {collecting_method} ({specimens})")) %>%
    # ungroup() %>% 
      summarize(specimens = paste(specimens, collapse = "; ")) %>% 
      mutate(collecting_summary_by_date =glue("{date_label}, {collector_s}, {collecting_method} ({specimens})")) %>% 
      group_by(locality_id) %>% 
      summarize(collecting_summary_by_locality = paste(collecting_summary_by_date, collapse = "; ")) %>% 
      mutate(locality_summary = glue("{locality_id}: {collecting_summary_by_locality}")) %>% 
      ungroup() %>% 
      summarize(locality_final = paste(locality_summary, collapse = ". ")) %>% 
      pull(locality_final)
    
    locality_specimens
    
    #START HERE
    
    print(glue("{k}: {locality_specimens}.")) 
    }}}












  
           


