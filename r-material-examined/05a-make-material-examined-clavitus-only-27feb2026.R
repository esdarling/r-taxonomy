#test with full Perilampus dataset

library(here)
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(glue)
library(parzer)

data <- read_excel(here("r-material-examined", "data", 
                        "Platigaster group export 2023-07-05 clavatus only test.xlsx"), 
                   #range = "A1:AX108", 
                   guess_max = 10000) %>% #set range because header on second row
  clean_names() 

data
names(data)
glimpse(data)

data <- data %>% 
  rename("species" = specific_epithet) %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(where(is.character), str_trim), #remove any leading/trailing whitespace
         sex = str_to_lower(sex)) %>% 
  mutate(type_ame = ifelse(is.na(type_ame), "Additional Material Examined", type_ame))
  

glimpse(data)
glimpse(data$locality)

data %>% 
  tabyl(species)

data %>% 
  tabyl(type_ame)

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
  # mutate(location_info = case_when(!is.na(park) ~ glue("{park}, {locality}"),
  #                                 TRUE ~ glue("{locality}"))) %>% 
  mutate(location_info = glue("{locality}")) %>%
  mutate(dcd_locality = case_when(!is.na(verbatim_latitude) ~ 
                                    glue("{location_info}, {verbatim_latitude}, {verbatim_longitude}"),
                                  TRUE ~ glue("{location_info}")))

data %>% 
  select(location_info) %>%
  distinct(location_info)

data %>% 
  select(dcd_locality) %>%
  distinct(dcd_locality)


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
 #  write_csv(here("r-material-examined/outputs", "P hyalinus-by-country.csv"))
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
         type_ame, 
         object_number, 
         repository, 
         #bin_number, 
         #host_id,
         sex, 
         country,
         state_province, 
         dcd_locality, 
         #verbatim_date, 
         #collector_s, 
         collector_s) %>%
  # select(object_number, 
  #        repository, 
  #        bin_number, 
  #        host_id) %>% 
  # distinct() %>% 
  # view()
  # mutate(id = case_when(!is.na(repository) & !is.na(host_id) & !is.na(bin_number) ~
  #                         glue("{object_number}-{repository}, {host_id}, {bin_number}"),
  #                       !is.na(repository) & !is.na(host_id) & is.na(bin_number) ~
  #                         glue("{object_number}-{repository}, {host_id}"),
  #                       !is.na(repository) & is.na(host_id) & !is.na(bin_number) ~
  #                         glue("{object_number}-{repository}, {bin_number}"),
  #                       !is.na(repository) & is.na(host_id) & is.na(bin_number) ~
  #                         glue("{object_number}-{repository}"),
  #                       TRUE ~ glue("{object_number}"))) %>%
  mutate(id = case_when(!is.na(repository) ~
                          glue("{object_number}-{repository}"),
                        TRUE ~ glue("{object_number}"))) %>%
  # select(id) %>%
  # distinct(id) %>%
  # view()
group_by(species, 
         type_ame, 
           country, 
           state_province, 
           dcd_locality, 
           #verbatim_date, 
           #collector_s,
           #collecting_method, 
         collector_s,
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
      glue("{n}{sex}-{specimen_id}")) %>% 
  group_by(species, 
           type_ame,
           country, 
           state_province, 
           dcd_locality,
           #type, 
           #verbatim_date,
           collector_s) %>% 
  summarize(specimens = paste(specimens_by_sex, collapse = ". "))

summary_specimens
glimpse(summary_specimens)
head(summary_specimens$specimens)

summary_specimens

summary_specimens %>% 
  tabyl(type_ame)

# full loop ---------------------------------------------------------------

species_list <- data %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) %>% 
  pull(species)

species_list


type_list <- data %>% 
  select(type_ame) %>% 
  distinct(type_ame) %>% 
  arrange(type_ame) %>% 
  pull(type_ame)

type_list <- c("Holotype", "Paratype", "Additional Material Examined")
type_list


# -----------------------------
# Helper functions (define once)
# -----------------------------

sex_symbol <- function(x) {
  dplyr::case_when(
    x == "male" ~ "\u2642",  # ♂
    x == "female" ~ "\u2640",  # ♀
    TRUE          ~ x
  )
}

replace_sex_words <- function(text) {
  stringr::str_replace_all(
    text,
    c(
      # Replace "female" even when attached to digits or codes
      "(?i)(female)(s)?" = "\u2640",
      
      # Replace "male" even when attached to digits or codes
      "(?i)(male)(s)?"   = "\u2642"
    )
  )
}

# -----------------------------
# Main loop
# -----------------------------

for (i in species_list) {
  
  suppressMessages({
    
    print(glue::glue("Perilampus {i}"))
    
    species_data <- data %>%
      dplyr::filter(species == i)
    
    species_data %>%
      janitor::tabyl(species)
    
    
  for (ii in type_list) {
      
      # Label for printing
      print(ii)
    
    
    # -----------------------------
    # Country level
    # -----------------------------
    
    country_list <- species_data %>%
      { 
        if (is.na(ii)) {
          dplyr::filter(., is.na(type_ame))
        } else {
          dplyr::filter(., type_ame == ii)
        }
      } %>%
      dplyr::distinct(country) %>%
      dplyr::arrange(country) %>%
      dplyr::pull(country)
    
    country_list
    
    for (j in country_list) {
      
      species_data %>% 
        tabyl(type_ame)
      
      specimens_sex_country <- species_data %>%
        dplyr::filter(species == i & type_ame == ii & country == j) %>%
        dplyr::group_by(species, country, sex) %>%
        dplyr::count() %>%
        dplyr::mutate(sex = sex_symbol(sex)) %>%
        dplyr::mutate(sex = glue::glue("{n}{sex}")) %>%
        dplyr::group_by(species, country) %>%
        dplyr::summarize(
          country_summary = paste(sex, collapse = ", "),
          .groups = "drop"
        )
      
      country_summary_clean <- replace_sex_words(specimens_sex_country$country_summary)
      
      print(glue::glue("{j}: {country_summary_clean}."))
      
      # -----------------------------
      # State level
      # -----------------------------
      
      state_list <- species_data %>%
        dplyr::filter(species == i & country == j) %>%
        dplyr::distinct(state_province) %>%
        dplyr::arrange(state_province) %>%
        dplyr::pull(state_province)
      
      for (k in state_list) {
        
        specimens_sex_state <- species_data %>%
          dplyr::filter(
            species == i & type_ame == ii &
              country == j &
              state_province == k
          ) %>%
          dplyr::group_by(species, type_ame, country, state_province, sex) %>%
          dplyr::count() %>%
          dplyr::mutate(sex = sex_symbol(sex)) %>%
          dplyr::mutate(sex = glue::glue("{n}{sex}")) %>%
          dplyr::group_by(species, country, state_province) %>%
          dplyr::summarize(
            state_summary = paste(sex, collapse = ", "),
            .groups = "drop"
          )
        
        state_summary_clean <- replace_sex_words(specimens_sex_state$state_summary)
        
        # -----------------------------
        # Locality + specimens text
        # -----------------------------
        
        locality_specimens <- summary_specimens %>%
          dplyr::filter(
            species == i & type_ame == ii & 
              country == j &
              state_province == k
          ) %>%
          dplyr::group_by(dcd_locality, collector_s) %>%
          dplyr::summarize(
            specimens = paste(specimens, collapse = "; "),
            .groups = "drop"
          ) %>%
          dplyr::mutate(specimens = replace_sex_words(specimens)) %>%
          dplyr::mutate(
            collecting_summary = glue::glue("{collector_s} ({specimens})")
          ) %>%
          dplyr::group_by(dcd_locality) %>%
          dplyr::summarize(
            collecting_summary_by_locality = paste(collecting_summary, collapse = "; "),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            locality_summary = glue::glue("{dcd_locality}: {collecting_summary_by_locality}")
          ) %>%
          dplyr::ungroup() %>%
          dplyr::summarize(
            locality_final = paste(locality_summary, collapse = ". ")
          ) %>%
          dplyr::pull(locality_final)
        
        locality_specimens_clean <- replace_sex_words(locality_specimens)
        
        # -----------------------------
        # Final print
        # -----------------------------
        
        print(
          glue::glue(
            "{k}: {state_summary_clean}. {locality_specimens_clean}."
          )
        )
        
      }
    }
  }})
}


#copy output into a Word file (I know..))
     