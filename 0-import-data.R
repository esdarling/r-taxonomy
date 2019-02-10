#this code loads packages and imports a lucid CSV file stored on Google Drive

library(googlesheets)
library(tidyverse)
library(data.table)

#-------------------
mysheets <- gs_ls() #this signs into google docs and authenticates
mysheets %>% 
  glimpse()

?gs_title

lucid <- gs_title("Jan 31 2017 test export") #identifies google sheet
lucid <- gs_read(lucid)  #imports data

  
  

