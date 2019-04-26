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
list.files(here("data-input"), pattern='*.csv')
file <- list.files(here("data-input"), pattern='*.csv')
file

lucid <- read.csv(here("data-input", file)) %>% 
  as_tibble()
lucid  

