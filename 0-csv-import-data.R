#this code loads packages and imports a lucid CSV file stored on Google Drive

library(here)
library(googlesheets)
library(tidyverse)
library(data.table)

#-------------------
#import from csv file
# lucid <- read.csv(here("data-input", "UAE auratus test.csv")) %>% 
#   as_tibble()

lucid <- read.csv(here("data-input", "Auratus 1 and 2 Feb 27 2019.csv")) %>% 
  as_tibble()
lucid  

