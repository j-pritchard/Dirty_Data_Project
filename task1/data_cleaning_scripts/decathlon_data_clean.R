library(tidyverse)
library(here)
library(janitor)

decathlon_clean_data <- read_rds(here::here("raw_data/decathlon.rds")) %>% 
  rownames_to_column(var = "name") %>% # Change row names to their own column
  clean_names() %>% # Tidy column names
  mutate(name = str_to_sentence(name)) # Tidy competitor names

write.csv(decathlon_clean_data,
          here::here("clean_data/decathlon_clean_data.csv")) # save in folder

rm(decathlon_clean_data) #tidy environment