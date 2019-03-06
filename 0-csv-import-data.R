#this code loads packages and imports a lucid CSV file stored on Google Drive

library(here)
library(googlesheets)
library(tidyverse)
library(data.table)
library(visdat)
library(skimr)

#install.packages("visdat")

#-------------------
#import from csv file
# lucid <- read.csv(here("data-input", "UAE auratus test.csv")) %>% 
#   as_tibble()

lucid <- read.csv(here("data-input", "March 6.csv")) %>% 
  as_tibble()
lucid  

