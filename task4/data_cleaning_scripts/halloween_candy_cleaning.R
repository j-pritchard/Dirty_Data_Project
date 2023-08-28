# Packages readxl, tidyverse, here and janitor
library(readxl)
library(tidyverse)
library(here)
library(janitor)

candy_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))

# Remove unneeded columns. Indices used as this appeared to execute more quickly.
# Tidy column titles
# Standardise candy names that differ over years
# year_id column
cleaner_candy_2015 <- candy_2015 %>% 
  select(2:10, 12:15, 17, 19:22, 24, 25, 29:32, 35:37, 39, 40, 42:44, 46:55,
         58:62, 64:68, 70, 71, 73:81, 83, 84, 86, 87, 89, 91, 92, 96, 114, 115) %>% 
  clean_names() %>% 
  rename(age = how_old_are_you,
         going_out = are_you_going_actually_going_trick_or_treating_yourself,
         thing_in_black_and_orange_wrappers = 
           anonymous_brown_globs_that_come_in_black_and_orange_wrappers) %>% 
  mutate(year_id = str_c("2015_", row_number()), .before = age)


cleaner_candy_2016 <- candy_2016 %>% 
  select(2:5, 7:11, 13, 16:20, 23:25, 28:30, 33:37, 39:42, 44, 45, 47, 50:68,
         70:77, 80:89, 91:93, 95:100, 103, 106) %>% 
  clean_names() %>% 
  rename(age = how_old_are_you,
         going_out = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         country = which_country_do_you_live_in,
         bonkers = bonkers_the_candy,
         thing_in_black_and_orange_wrappers = 
           anonymous_brown_globs_that_come_in_black_and_orange_wrappers) %>% 
  mutate(year_id = str_c("2016_", row_number()), .before = going_out)


cleaner_candy_2017 <- candy_2017 %>% 
  select(2:5, 7:11, 13, 16:20, 23:25, 28:30, 33:37, 39:42, 44, 45, 47, 50:68,
         71:78, 80, 82:85, 87:91, 93:95, 97:103, 106, 109) %>% 
  clean_names() %>% 
  rename(age = q3_age,
         going_out = q1_going_out,
         gender = q2_gender,
         country = q4_country,
         bonkers = q6_bonkers_the_candy,
         thing_in_black_and_orange_wrappers = 
           q6_anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
         x100_grand_bar = q6_100_grand_bar
         ) %>% 
  mutate(year_id = str_c("2017_", row_number()), .before = going_out) %>% 
  rename_with(~str_remove(., 'q[0-9]+_'))

# Bind three years of candy
all_candy <- bind_rows(cleaner_candy_2017, cleaner_candy_2016, cleaner_candy_2015)

# Further column reordering: put candy columns in alphabetical order
identifiers <- all_candy %>% 
  select(1, 4, 3, 5, 2)
candies <- all_candy %>% 
  select(1,6:95)
alphabetical_candies <- candies[,order(colnames(candies))]
all_candy <- full_join(identifiers, alphabetical_candies, by = "year_id")

# Tidy ages: make ages numeric and remove unreasonable outliers
all_candy <- all_candy %>% 
  mutate(age = str_extract(age, "[0-9]+")) %>% # Remove anything non-numeric
  transform(age = as.numeric(age)) %>%   
  mutate(age = case_when(age > 117 ~ NA, # oldest person at the time was 117
                         age < 5 ~ NA, 
                         .default = age))
  # This leaves 441 ages as NA

# Tidy countries
all_candy <- all_candy %>% 
  
  # make everything lower case
  mutate(country = str_to_lower(country)) %>% 
  
  # deal with common cases
  mutate(country = case_when(country == "not the usa or canada" ~ "other",
                             country == "u.k." ~ "GBR",
                             country == "uk" ~ "GBR",
                             str_detect(country, "united k") ~ "GBR",
                             str_detect(country, "united s") ~ "USA",
                             str_detect(country, "u.s.") ~ "USA",
                             str_detect(country, "^us") ~ "USA",
                             str_detect(country, " states") ~ "USA",
                             country %in% str_to_lower(state.name) ~ "USA",
                             str_detect(country, "canada") ~ "CAN",
                             .default = country)) %>% 
  
  # remove numbers and symbols
  mutate(country = str_extract(country, "[(A-Za-z ]+"),
         country = str_replace_all(country, "[ ]+", "_")) %>%
  
  # deal with all other cases
  mutate(country = case_when(country %in% c("a",
                                            "atlantis",
                                            "denial",
                                            "earth",
                                            "fear_and_loathing",
                                            "god",
                                            "i_don",
                                            "insanity_lately",
                                            "narnia",
                                            "neverland",
                                            "one_of_the_best_ones",
                                            "see_above",
                                            "somewhere",
                                            "subscribe_to_dm",
                                            "there_isn",
                                            "this_one") ~ NA,
                             
                             country %in% c("can",
                                            "soviet_canuckistan") ~ "CAN",
                             
                             country %in% c("ahem",
                                            "america",
                                            "eua",
                                            "merica",
                                            "murica",
                                            "murrika",
                                            "pittsburgh",
                                            "sub",
                                            "the_best_one_",
                                            "the_yoo_ess_of_aaayyyyyy",
                                            "trumpistan",
                                            "u_s",
                                            "u_s_a",
                                            "ud") ~ "USA",
                             
                             country %in% c("endland",
                                            "england",
                                            "scotland") ~ "GBR",
                             
                             .default = country))

write.csv(all_candy,
          here::here("clean_data/all_candy.csv")) # save in folder




# The following script assigns points to candy ratings for use in some questions.
all_candy_points <- all_candy %>% 
  mutate(across(any_full_sized_candy_bar:york_peppermint_patties,
                ~case_when(. == "JOY" ~ 1,
                           . == "MEH" ~ 0, 
                           . == "DESPAIR" ~ -1,
                           .default = 0)))

write.csv(all_candy_points,
          here::here("clean_data/all_candy_points.csv")) # save in folder

# Tidy environment
rm(all_candy, all_candy_points, candies, alphabetical_candies, identifiers,
   candy_2015, candy_2016, candy_2017, cleaner_candy_2015, cleaner_candy_2016,
   cleaner_candy_2017)