# Author: Ryan Bemowski (rrbemo@gmail.com)

# This file takes events and gets unique information about species from them. 
# The goal is to end up with a csv of unique species names and their details to 
# add to over time. This small table can be joined to the events table to give 
# additional details to the event.

library(tidyverse)

# read in the data
events_df <- read_csv("data/apis_data_2015_2023_descriptive.csv")

# filter only the fields of interest
species_details_df <- events_df %>%
  select(species, common_name, category, primary_eco_role, adult_size, winter_active, winter_inactive) %>%
  unique()

#view(species_details_df)
# Check to see all species are unique
print(count(species_details_df, species, sort=TRUE))

# It wouldn't surprise me if not all common names are unique... lets check
print(count(species_details_df, common_name, sort=TRUE))

# Are there any species that are identified as being winter active and inactive?
species_details_df %>%
  filter(winter_active & winter_inactive) %>%
  print()

# If not, we should drop winter_inactive (it is redundant)
species_details_df <- species_details_df %>%
  select(-winter_inactive)

# Now we can write out this file
write_csv(species_details_df, "data/species_details.csv")
