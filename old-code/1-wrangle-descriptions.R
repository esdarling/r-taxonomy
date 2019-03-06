#this code processes the data

library(here)
#source(here("0-google-docs-import-data.R"))
source(here("0-csv-import-data.R"))

lucid
names(lucid)
length(names(lucid))

#create new character variable
#keep only the text after dummy#:
pattern = "^dummy.*:"

lucid <- lucid %>% 
  mutate(character = str_remove(X, pattern)) %>% 
  select(X, character, 2:length(names(lucid)))

lucid
#lucid$character

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
lucid

test <- lucid %>% 
  select(1:2,4) %>% 
  filter(auratus.1 > 0) %>% 
  mutate(character = str_replace_all(character, ":", ": "))

test
names(test)

#export species description
test.output <- test %>% 
  summarize(description = paste(character, collapse = " "))
names(test.output)[1] <- names(test)[3]
test.output



fwrite(test.output, here("outputs", "test.csv"))
