library(tidyverse)
library(here)
library(janitor)

cake_ingredients <- read_csv(here::here("raw_data/cake-ingredients-1961.csv"))
ingredient_codes <- read_csv(here::here("raw_data/cake_ingredient_code.csv"))

# Make vertical list and drop NA values
long_cake_ingredients <- cake_ingredients %>% 
  pivot_longer(cols = AE:ZH, names_to = "code") %>% 
  drop_na()

# join two data sets together
joined_cake <- left_join(long_cake_ingredients, ingredient_codes, by = "code")

# Tidy names and rearrange columns
cake_ingredients_clean <- clean_names(joined_cake) %>% 
  select(cake, ingredient, value, measure) %>% 
  rename(amount = value) %>% 
  mutate(ingredient = case_when(ingredient == "Sour cream cup" ~ "Sour cream",
                                .default = ingredient)) %>% 
  mutate(measure = case_when(is.na(measure) == TRUE ~ "cup",
                             .default = measure))

# Save clean data
write.csv(cake_ingredients_clean,
          here::here("clean_data/cake_ingredients_clean.csv"))

# Tidy environment
rm(cake_ingredients, ingredient_codes, joined_cake, long_cake_ingredients,
   cake_ingredients_clean)