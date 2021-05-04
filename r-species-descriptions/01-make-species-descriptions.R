#this code processes the data
#specifically for Auratus 1 and 2 test files from DCD

library(here)
library(tidyverse)
library(janitor)

lucid <- read_csv(here("r-species-descriptions", "UAE August 8, 2019 edit.csv")) %>% 
  clean_names() %>% 
  remove_empty()

lucid
names(lucid) 

#create new character variable
#keep only the text after dummy#:
pattern = "^dummy.*:"

dim(lucid)

lucid <- lucid %>% 
  #mutate(order = seq(from = 1, to = nrow(lucid), by = 1)) %>% 
  #gather("species", "trait.01", -1) %>%  #-X excludes X column from gather
  pivot_longer(-1, names_to = "species", values_to = "trait") %>% 
  mutate(character = str_remove(feature, pattern), 
         character = str_replace_all(character, ":", ": ")) %>% 
  filter(trait != 0)

lucid
unique(lucid$species)
unique(lucid$character)

# ----------------------------------------------------
#testing
# test.chars <- c("Length:2-4mm", "Body colour: black", "dummy8:")
# str_detect(test.chars, "dummy")
# str_detect(test.chars, "[:digit:]")
# 
# #pattern is "dummy" to ":"
# pattern = "^dummy.*:"
# str_detect(test.chars, pattern)
# str_remove(test.chars, pattern)
# ----------------------------------------------------

#export species description
spp_collapse <- lucid %>% 
  group_by(species) %>% 
  summarize(description = paste(character, collapse = " "))

spp_collapse
unique(spp_collapse$species)


#this outputs a CSV file that can be copied into a Word file for each species description and final formatting
write_csv(spp_collapse, 
          here("r-species-descriptions", 
               "output-species-description.csv"))

