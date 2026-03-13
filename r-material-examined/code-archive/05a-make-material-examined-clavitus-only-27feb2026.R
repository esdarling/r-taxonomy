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

#test with new file
# data <- read_excel(here("r-material-examined", "data",
#                         "Platigaster group export 2023-07-05 clavatus SRNP.xlsx"),
#                    #range = "A1:AX108",
#                    guess_max = 10000) %>% #set range because header on second row
#   clean_names()


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

data %>% select(verbatim_latitude, verbatim_longitude) %>% 
  slice(25:30)

clean_coord <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("^-?[0-9]+\\.[0-9]+$", x) ~ as.character(round(suppressWarnings(as.numeric(x)), 6)),
    TRUE ~ x
  )
}

data <- data %>%
  mutate(location_info = glue("{locality}")) %>%
  mutate(
    verbatim_latitude  = clean_coord(verbatim_latitude),
    verbatim_longitude = clean_coord(verbatim_longitude),
    dcd_locality = case_when(
      !is.na(verbatim_latitude) ~ glue("{location_info}, {verbatim_latitude}, {verbatim_longitude}"),
      TRUE ~ glue("{location_info}")
    )
  )

data %>% select(verbatim_latitude, verbatim_longitude) %>% 
  slice(25:30)

data %>% 
  select(location_info) %>%
  distinct(location_info)

data %>% 
  select(dcd_locality) %>%
  distinct(dcd_locality)


data$dcd_locality

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

data %>% 
  select(object_number, 
         repository, 
         bin_number_coi) %>% 
  mutate(id = case_when(
    !is.na(repository) & !is.na(bin_number_coi) ~
      glue("{object_number}-{repository}-{bin_number_coi}"),
    !is.na(repository) ~
      glue("{object_number}-{repository}"),
    TRUE ~ glue("{object_number}")
  ))


summary_specimens <- data %>% 
  select(species, 
         type_ame, 
         object_number, 
         repository, 
         bin_number_coi, 
         #host_id,
         sex, 
         country,
         state_province, 
         dcd_locality, 
         #verbatim_date, 
         #collector_s, 
         collector_s) %>%
  mutate(id = case_when(
    !is.na(repository) & !is.na(bin_number_coi) ~
      glue("{object_number}-{repository}-{bin_number_coi}"),
    !is.na(repository) ~
      glue("{object_number}-{repository}"),
    TRUE ~ glue("{object_number}"))) %>% 
  group_by(species, 
           type_ame, 
           country, 
           state_province, 
           dcd_locality, 
           collector_s,
           sex) %>% 
  summarize(n = n(), 
            specimen_id = paste(id, collapse = "; ")) %>% 
  mutate(sex_label = case_when(
    sex == "male" & n > 1   ~ glue("\u2642\u2642"),
    sex == "male"           ~ "\u2642",
    sex == "female" & n > 1 ~ glue("\u2640\u2640"),
    sex == "female"         ~ "\u2640",
    TRUE                    ~ sex)) %>%
  mutate(specimens_by_sex = glue("{n}{sex_label}, {specimen_id}")) %>% 
  group_by(species, 
           type_ame,
           country, 
           state_province, 
           dcd_locality,
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
# Helper function (define once)
# -----------------------------

sex_symbol <- function(sex, n) {
  symbol <- dplyr::case_when(
    sex == "male"   ~ "\u2642",  # ♂
    sex == "female" ~ "\u2640",  # ♀
    TRUE            ~ sex
  )
  dplyr::case_when(
    n > 1  ~ glue::glue("{n}{symbol}{symbol}"),
    TRUE   ~ glue::glue("{n}{symbol}")
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
          dplyr::mutate(sex = sex_symbol(sex, n)) %>%
          dplyr::group_by(species, country) %>%
          dplyr::summarize(
            country_summary = paste(sex, collapse = ", "),
            .groups = "drop"
          )
        
        print(glue::glue("{j}: {specimens_sex_country$country_summary}."))
        
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
            dplyr::mutate(sex = sex_symbol(sex, n)) %>%
            dplyr::group_by(species, country, state_province) %>%
            dplyr::summarize(
              state_summary = paste(sex, collapse = ", "),
              .groups = "drop"
            )
          
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
          
          # -----------------------------
          # Final print
          # -----------------------------
          
          print(
            glue::glue(
              "{k}: {specimens_sex_state$state_summary}. {locality_specimens}."
            )
          )
          
        }
      }
    }})
}


#copy output into a Word file (I know..))