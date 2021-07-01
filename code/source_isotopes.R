# Isotope Dataset Cleaning
# Ana Miller-ter Kuile
# May 5, 2021

# this script takes in and cleans the isotope data creating
# isotope dataframe ready for SIBER and KIN analyses with
# isotopes, body sizes, and islet categories

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------
#spider isotope data
spider_iso <- read.csv(here("data", 
                            "isotopes", 
                            "2009-2015_Cane Spider Isotopes.csv"))
#plant isotope data
plant_iso <- read.csv(here("data", 
                           "isotopes", 
                           "2009-2012 Palmyra Plant Isotope Data.csv"))
#spider size data
spider_size <- read.csv(here("data", 
                             "size", 
                             "Spider sizes 2009-2015.csv"))
#island size and productivity
islands <- read.csv(here('data', 
                         "Island_data.csv"))

# Tidy Isotope Data -------------------------------------------------------

#make IDs make sense, rename islands for consistency
spider_iso <- spider_iso %>%
  separate(ID, 
           into = c("Island", "ID"), 
           sep = " ") %>%
  mutate(Island = case_when(Island == "Para" ~ "Paradise",
                            Island == "East" ~ "Eastern",
                            Island == "NF" ~ "N.Fighter",
                            Island == "SF" ~ "S.Figher",
                            Island == "Whip" ~ "Whipoorwill",
                            TRUE ~ Island))

#rename islands and get mean plant isotopic signature
plant_iso_2 <- plant_iso %>%
  mutate(Island = case_when(Island.name == "N. Fighter" ~ "N.Fighter",
                            Island.name == "S. Fighter" ~ "S.Figher",
                            Island.name == "NS Causeway" ~ "NSCauseway",
                            TRUE ~ Island.name)) %>%
  group_by(Island.name) %>%
  summarise(plant_d15N = mean(d15N))

#categorize islet size and productivity into high/low, big/small
islands <- islands %>%
  mutate(prod_level = ifelse(Island_prod > 0.008707850, 
                             "high", "low"),
         size_level = ifelse(Island_Area > 39629.5965, 
                             "big", "small"))

#rename stuff in the body size dataset for consistency
spider_size <- spider_size %>%
  mutate(Island = case_when(Island == "North Fighter" ~ "N.Fighter",
                            Island == "South Fighter" ~ "S.Fighter",
                            Island == "NS Causeway" ~ "NSCauseway",
                            Island == "Home NE" ~ "HomeNE",
                            Island == "Home SW" ~ "HomeSW",
                            Island == "Strawn " ~ "Strawn",
                            TRUE ~ Island))

# filter out years of interest, variables of interest
# clean up ID column, make naming and columns consistent
spider_size2 <- spider_size %>%
  filter(Year %in% c(2015, 2009, 2012)) %>%
  dplyr::select(Island, ID, Length_mm, Mass_g, Year) %>%
  separate(ID, 
           into = c("Island_2", "ID_2"), 
           sep = -2,
           remove = F) %>%
  mutate(ID_2 = str_squish(ID_2)) %>%
  dplyr::select(-ID, -Island_2) %>%
  rename("ID" = "ID_2")

#join spider and plant isotope data, correct spider d15 and 
# get rid of some weird or under-sampled sites
spider_iso <- spider_iso %>%
  dplyr::select(Island, ID, d15N, d13C, Year) %>%
  left_join(plant_iso_2, by = c("Island" = "Island.name")) %>%
  mutate(d15N_c = d15N - plant_d15N) %>%
  filter(!Island %in% c("Ainsley", "Whipoorwill", "Home")) %>%
  filter(!is.na(plant_d15N)) %>%
  left_join(islands, by = "Island") %>%
  left_join(spider_size2, by = c("Island", "ID", "Year"))






