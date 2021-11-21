#test with full Perilampus dataset

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)

?read_excel

data <- read_excel(here("r-material-examined", "data", 
                        "PlatigasterGroupWithHostData BH Nov 9 DCD reduce columns Nov 12 Emily test dobnos.xlsx"), 
                   range = "A2:AT2270", 
                   guess_max = 10000) %>% #set range because header on second row
  clean_names() 

data
glimpse(data)

#Aha, '\' hiding in ASCII -- remove later with find/replace in Word (until I have a better solution)

data <- data %>% 
  rename("species" = specific_epithet) %>% 
  filter(species == "dobnos") %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), #remove any leading/trailing whitespace
         sex = str_to_lower(sex)) 

glimpse(data)

data %>% 
  tabyl(species)

#use hymenoptera_on_line_locality
data %>% 
  select(contains("locality")) 

#find-replace backslashes to delete in Word 
data %>% 
  select(hol_locality) %>% 
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

#save for dad to check 
.Last.value %>% 
  write_csv(here("r-material-examined/outputs", "dobnos-by-country.csv"))


# specimen summary with dobnos-----------------------------------------------------
names(data)

# Select columns for use --------------------------------------------------
names(data)

#add state/province
data %>% 
  tabyl(state_province)

data %>% 
  tabyl(verbatim_date)  #oh boy

data %>% 
  select(species, 
         rome_number, 
         repository, 
         sex, 
         country,
         state_province, 
         hol_locality, 
         verbatim_date) %>% 
  glimpse()


# Make specimen summary ---------------------------------------------------
options(dplyr.summarise.inform = FALSE)

summary_specimens <- data %>% 
  mutate(id = glue("{rome_number}-{repository}")) %>% #add notes column if present 
  #id = case_when(is.na(notes) ~ glue("{object_number}-{repository}"), 
  #TRUE ~ glue("{object_number}-{repository}, {notes}")
  group_by(species, 
           country, 
           state_province, 
           hol_locality, 
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
           state_province, 
           hol_locality,
           #type, 
           verbatim_date) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = "; "))

glimpse(summary_specimens)
head(summary_specimens$specimens)

# full loop ---------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)

species_list <- unique(data$species)

#check dobnos/Costa Rica

data %>% 
  filter(species == "dobnos" & 
           country == "Costa Rica") %>% 
  tabyl(state_province)

data %>% 
  filter(species == "dobnos" & 
           country == "Costa Rica")  %>% 
  select(state_province, 
         hol_locality) %>% 
  distinct() %>% 
  arrange(state_province, 
          hol_locality) %>% 
  view()


#for (i in species_list) {

for (i in species_list) {
  print(glue("Perilampus {i}"))
  
  i <- "dobnos"
  
  species_data <- data %>%  
    filter(species == i)
  
  #create country list for species and type
  country_list <- species_data %>% 
    distinct(country) %>%
    arrange(country) %>% #alphabetical
    pull(country)
  
  for (j in country_list) {
      
      j <- "Costa Rica"
      
      specimens_sex_country <- species_data %>%
        filter(species == i &
                 country == j) %>% 
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
        summarize(specimens_by_sex = paste(sex, collapse = ", "))
      
      specimens_sex_country
      
      summary_specimens
      glimpse(summary_specimens)
      
      locality_specimens <- summary_specimens %>%
        filter(species == i & 
                 country == j) %>% 
        group_by(hol_locality,
                 verbatim_date) %>% 
        summarize(specimens = paste(specimens, collapse = "; ")) %>% 
        mutate(collecting_summary_by_date = glue("{verbatim_date} ({specimens})")) %>% 
        group_by(hol_locality) %>% 
        summarize(collecting_summary_by_locality = paste(collecting_summary_by_date, collapse = "; ")) %>% 
        mutate(locality_summary = glue("{hol_locality}: {collecting_summary_by_locality}")) %>% 
        ungroup() %>% 
        summarize(locality_final = paste(locality_summary, collapse = ". ")) %>% 
        pull(locality_final) 
      
      locality_specimens
      
      #print sex tally only for non-type records
      #j <- "Material examined"
      
      print(glue("{j}: {specimens_sex_country$specimens_by_sex}. {locality_specimens}."))
      
    }}

#copy output into a Word file (I know..)
















