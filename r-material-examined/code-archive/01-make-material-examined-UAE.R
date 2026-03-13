library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)


# #load material species --------------------------------------------------
here()
data <- read_excel(here("r-material-examined", "Yemen and UAE Perilampus-Darwin Core Final.xlsx")) %>% 
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
                            "Other material examined")) 

#recraft lat-longitudes 
#use verbatim (no matter the formatting)
#if no verbatim, then parse a lat-degree-min from decimal degrees

#update: dad will check holotype lat-long by hand
data %>% 
  select(contains("latitude"))

data %>% 
  select(contains("longitude"))

data <- data %>% 
  # mutate(lat_degree = parse_parts_lat(verbatim_latitude), 
  #        lon_degree = parse_parts_lon(verbatim_longitude)) %>%
  # tidyr::unpack(c(lat_degree, lon_degree), 
  #               names_sep = "_") %>% 
  #mutate(locality_id = glue("{locality}, {lat_degree_deg}{lat_degree_min}'N, {lon_degree_deg}degree{lon_degree_min}'E"))
  mutate(locality_id = 
           glue("{locality}, {verbatim_latitude} {verbatim_longitude}"))

data %>% 
  pull(locality_id) %>% 
  head()

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

data %>% 
  tabyl(notes)


summary <- data %>% 
  #filter(species == "rainerius") %>%
  #mutate(id = glue("{object_number}-{repository}")) %>% #add notes column if present 
  mutate(id = case_when(
    is.na(notes) ~ glue("{object_number}-{repository}"), 
    TRUE ~ glue("{object_number}-{repository}, {notes}"))) %>% 
  #filter(!is.na(notes)) %>% pull(id)
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
    TRUE ~ sex)) %>% 
  mutate(specimens_by_sex = case_when( 
    type == "Holotype" ~ glue("{sex}: {specimen_id}"), #don't put '1' when it is a Holotype
    TRUE ~ glue("{n} {sex}: {specimen_id}"))) %>% 
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
  filter(str_detect(locality_id, "Wadi Bih")) %>% 
  pull(specimens)

summary %>% 
  filter(str_detect(specimens, "DC Darling"))

summary %>% 
  filter(type == "Holotype") %>% 
  pull(specimens)

summary %>% 
  filter(type == "Paratype") %>% 
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
  
  #i <- "houbaraensis"
  
  species_data <- data %>%  
    filter(species == i) %>% 
    arrange(type)

  type_list <- species_data %>% 
    select(type) %>% 
    distinct() %>% 
    mutate(type = as.character(type)) %>% 
    pull(type)
  
  for (j in type_list) {
  
    #j <- "Paratype"
    #j <- "Other material examined"
    
    #what type names do use - depends on how many types there are
    species_data %>% 
      tabyl(type)
    
    type_detail <- species_data %>%
      filter(species == i) %>% 
      count(type) %>% 
      mutate(n_types = n(), 
             type_to_print = case_when(
               n_types == 1 & type == "Other material examined" ~ "Material examined", #make "Material examined for non-type
               type == "Paratype" & n>1 ~ "Paratypes", 
               TRUE ~ as.character(type)))
  
    type_sex <- species_data %>%
      filter(species == i) %>%  
      group_by(species, 
               type, 
               sex) %>% 
      count() %>% 
      mutate(sex = case_when(
        n > 1 ~ glue("{n} {sex}s"), 
        TRUE ~ glue("{n} {sex}"))) %>% 
      group_by(species, 
               type) %>%
      summarize(specimens_by_sex = paste(sex, collapse = ", "))
    
    type_detail <- type_sex %>% 
      left_join(type_detail, by = "type") %>% 
      filter(type == j)
    
    #type_detail
    
    #print type title nd required detail
    #Material examined. Holotype. 
    #Paratype(s). Sex count. 
    #Material examined or Other material examined. 
    
    ifelse(j == "Holotype", 
           print(glue("Material examined. {j}.")), 
           ifelse(j == "Paratype",
                  print(glue("{type_detail$type_to_print}. {type_detail$specimens_by_sex}.")), 
                  print(glue("{type_detail$type_to_print}.")))) #Non-type
    
    #create country list for species and type
    country_list <- species_data %>% 
      filter(type == j)  %>% 
      distinct(country) %>%
      arrange(country) %>% #alphabetical
      pull(country)
    
    for (k in country_list) {
      
    #k <- "United Arab Emirates"
    
    specimens_sex_country <- species_data %>%
      filter(species == i & 
               type == j & 
               country == k) %>% 
      group_by(species, 
               type, 
               country, 
               sex) %>% 
      count() %>% 
      mutate(sex = case_when(
        n > 1 ~ glue("{n} {sex}s"), 
        TRUE ~ glue("{n} {sex}"))) %>% 
      group_by(species, 
               type, 
               country) %>%
      summarize(specimens_by_sex = paste(sex, collapse = ", "))
    
    specimens_sex_country
  
      
    locality_specimens <- summary %>%
      filter(species == i & 
               type == j & 
               country == k) %>% 
      group_by(locality_id, date_label, collector_s, collecting_method) %>% 
      summarize(specimens = paste(specimens, collapse = "; ")) %>% 
      mutate(collecting_summary_by_date = case_when( #deal with missing collecting method
        is.na(collecting_method) ~ glue("{date_label}, {collector_s} ({specimens})"), 
        TRUE ~  glue("{date_label}, {collector_s}, {collecting_method} ({specimens})"))) %>% 
      group_by(locality_id) %>% 
      summarize(collecting_summary_by_locality = paste(collecting_summary_by_date, collapse = "; ")) %>% 
      mutate(locality_summary = glue("{locality_id}: {collecting_summary_by_locality}")) %>% 
      ungroup() %>% 
      summarize(locality_final = paste(locality_summary, collapse = ". ")) %>% 
      pull(locality_final) 
    
    locality_specimens
    
    #print sex tally only for non-type records
    #j <- "Material examined"
    
    ifelse(j %in% c("Holotype", "Paratype"), 
           print(glue("{k}: {locality_specimens}.")), 
           print(glue("{k}: {specimens_sex_country$specimens_by_sex}. {locality_specimens}.")))
           
    }}}
    












  
           


