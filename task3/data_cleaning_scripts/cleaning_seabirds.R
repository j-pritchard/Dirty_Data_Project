library(readxl)
library(tidyverse)
library(here)

ship_data <- read_xls(
  here::here("raw_data/seabirds.xls"), sheet = "Ship data by record ID")
bird_data <- read_xls(
  here::here("raw_data/seabirds.xls"), sheet = "Bird data by record ID")
# Warning message about M appearing in "SEX" column (Boolean)
# This column is not needed for analysis so warning ignored.

# Select necessary bird data columns and manually rename
nesc_bird_data <- bird_data %>% 
  select("RECORD ID",
         "Species common name (taxon [AGE / SEX / PLUMAGE PHASE])", 
         "Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])",
         "Species abbreviation",
         "COUNT") %>% 
  rename(record_id = "RECORD ID",
         common_name = "Species common name (taxon [AGE / SEX / PLUMAGE PHASE])",
         sci_name = "Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])",
         abbreviation = "Species abbreviation",
         "count" = "COUNT")

# Select necessary ship data columns and manually rename
nesc_ship_data <- ship_data %>% 
  select("RECORD ID",
         LAT) %>%
  rename(record_id = "RECORD ID",
         lat = "LAT")

# Left join with bird observaions on the left
combined_bird_data <- left_join(nesc_bird_data, nesc_ship_data,
                                by = "record_id") %>% 
  select(-record_id) %>% 
  mutate(count = case_when(count > 500 ~ NA,
                           .default = count))
  
# save clean data
write.csv(combined_bird_data,
          here::here("clean_data/seabirds.csv"))

# tidy environment
rm(bird_data, nesc_bird_data, nesc_ship_data, ship_data, combined_bird_data)