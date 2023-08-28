# Packages tidyverse, here and janitor
library(tidyverse)
library(here)
library(janitor)

rwa_dirty <- read_csv(here::here("raw_data/rwa.csv"))

rwa_clean <- rwa_dirty %>% 
  
  # Clean column names
  clean_names() %>% 
  
  # Select necessary columns
  select(age, gender, urban, education, hand, familysize, testelapse, q3:q22) %>% 
  
  # Recode responses
  mutate(gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female",
                            gender == 3 ~ "other"),
         urban = case_when(urban == 1 ~ "rural",
                           urban == 2 ~ "suburban",
                           urban == 3 ~ "urban"),
         education = case_when(education == 1 ~ "less than high school",
                               education == 2 ~ "high school",
                               education == 3 ~ "university",
                               education == 4 ~ "postgrad"),
         hand = case_when(hand == 1 ~ "R",
                          hand == 2 ~ "L",
                          hand == 3 ~ "B")) %>% 
  rename(childhood = urban) %>% 
  
  # Invert scores for reverse-scored questions
  mutate(q4 = if_else(q4 == 0, 0, 10 - q4),
         q6 = if_else(q6 == 0, 0, 10 - q6),
         q8 = if_else(q8 == 0, 0, 10 - q8),
         q9 = if_else(q9 == 0, 0, 10 - q9),
         q11 = if_else(q11 == 0, 0, 10 - q11),
         q13 = if_else(q13 == 0, 0, 10 - q13),
         q15 = if_else(q15 == 0, 0, 10 - q15),
         q18 = if_else(q18 == 0, 0, 10 - q18),
         q20 = if_else(q20 == 0, 0, 10 - q20),
         q21 = if_else(q21 == 0, 0, 10 - q21)) %>%
  
  # Calculate RWA score
  rowwise() %>% 
  mutate(rwa_score = mean(c_across(q3:q22)), .before = q3) %>% 
  ungroup() %>% 
  
  # filter out extrema
  filter(rwa_score > 0) %>% # RWA score of zero means all questions left blank
  filter(testelapse >= 30) %>% # survey should take at least 30 seconds

  # Remove other extreme values
  mutate(age = if_else(age > 116, NA, age), # oldest living person in 2015 was 116
         familysize = if_else(familysize > 20, NA, familysize), # unlikely to have 20 kids
         familysize = if_else(familysize == 0, NA, familysize), # impossible to be zero (should include respondent)
         testelapse = if_else(testelapse > 7200, NA, testelapse)) # two hours is generous

write_csv(rwa_clean, here::here("clean_data/rwa_data")) # save clean data

# Clean environment
rm(rwa_clean, rwa_dirty)