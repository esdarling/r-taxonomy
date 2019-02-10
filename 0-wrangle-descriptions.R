#this code processes the data

library(here)
source(here("0-import-data.R"))

lucid
names(lucid)
length(names(lucid))

#create new character variable
#keep only the text after dummy#:
pattern = "^dummy.*:"
lucid <- lucid %>% 
  mutate(character = str_remove(X1, pattern)) %>% 
  select(character, 2:length(names(lucid)))

lucid
lucid$character

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




#select a species (column) and see characters
test <- lucid %>% 
  select(1:2) %>% 
  filter(`Perilampus notdobnos (Bogus)` == 1)

test



