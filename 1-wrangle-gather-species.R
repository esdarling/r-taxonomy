#this code processes the data
#specifically for Auratus 1 and 2 test files from DCD

library(here)
#source(here("0-google-docs-import-data.R"))
source(here("0-csv-import-data.R"))

lucid

lucid <- lucid %>% 
  clean_names()
names(lucid) 

unique(lucid$x)
#create new character variable
#keep only the text after dummy#:
pattern = "^dummy.*:"

lucid <- lucid %>% 
  #mutate(order = seq(from = 1, to = nrow(lucid), by = 1)) %>% 
  gather("species", "trait.01", -1) %>%  #-X excludes X column from gather
  mutate(character = str_remove(x, pattern), 
         character = str_replace_all(character, ":", ": ")) %>% 
  filter(trait.01 != 0)

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
spp.collapse <- lucid %>% 
  group_by(species) %>% 
  summarize(description = paste(character, collapse = " "))

spp.collapse

fwrite(spp.collapse, here("outputs", paste("output", file, sep = "-")))

