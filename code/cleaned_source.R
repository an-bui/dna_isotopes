# Isotope and DNA Dataset Cleaning
# Ana Miller-ter Kuile
# May 5, 2021

# this script takes in and cleans the isotope data
# and also the DNA dataset, creating two separate outputs
# 1. isotope data ready for SIBER and KIN analyses with
# isotopes, body sizes, and islet categories
# 2. DNA data for spiders across islets with islet categories

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "SIBER", "glmmTMB",
                  "MuMIn", "emmeans",
                  "ggeffects", "DHARMa",
                  "effects",
                  "vegan", 
                  "rKIN")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------

spider_iso <- read.csv(here("data", 
                            "isotopes", 
                            "2009-2015_Cane Spider Isotopes.csv"))

plant_iso <- read.csv(here("data", 
                           "isotopes", 
                           "2009-2012 Palmyra Plant Isotope Data.csv"))

spider_size <- read.csv(here("data", 
                             "size", 
                             "Spider sizes 2009-2015.csv"))

islands <- read.csv(here('data', 
                         "Island_data.csv"))

DNA <- read.csv(here("data", 
                     "DNA", 
                     "all_prey_DNA.csv"))

DNA_meta <- read.csv(here("data",
                          "DNA",
                          "Sample_metadata.csv"))


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
plant_iso <- plant_iso %>%
  mutate(Island = case_when(Island.name == "N. Fighter" ~ "N.Fighter",
                            Island.name == "S. Fighter" ~ "S.Figher",
                            Island.name == "NS Causeway" ~ "NSCauseway",
                            TRUE ~ Island.name)) %>%
  group_by(Island.name) %>%
  summarise(plant_d15N = mean(d15N))

islands <- islands %>%
  mutate(prod_level = case_when(Island %in% c("Dudley",
                                              "Leslie",
                                              "Lost", 
                                              "Sand",
                                              "Eastern",
                                              "Frigate") ~ "high",
                                Island %in% c("Castor",
                                              "Fern",
                                              "Paradise",
                                              "Aviation",
                                              "Holei",
                                              "Kaula") ~ "low"),
         size_level = case_when(Island %in% c("Dudley",
                                              "Leslie",
                                              "Lost",
                                              "Frigate",
                                              "Castor",
                                              "Fern") ~ "small",
                                Island %in% c("Sand", 
                                              "Eastern",
                                              "Paradise",
                                              "Aviation",
                                              "Holei",
                                              "Kaula") ~ "big"))

spider_size <- spider_size %>%
  mutate(Island = case_when(Island == "North Fighter" ~ "N.Fighter",
                            Island == "South Fighter" ~ "S.Fighter",
                            Island == "NS Causeway" ~ "NSCauseway",
                            Island == "Home NE" ~ "HomeNE",
                            Island == "Home SW" ~ "HomeSW",
                            Island == "Strawn " ~ "Strawn",
                            TRUE ~ Island))

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
  left_join(plant_iso, by = c("Island" = "Island.name")) %>%
  mutate(d15N_c = d15N - plant_d15N) %>%
  filter(!Island %in% c("Ainsley", "Whipoorwill", "Home")) %>%
  filter(!is.na(plant_d15N)) %>%
  left_join(islands, by = "Island") %>%
  left_join(spider_size2, by = c("Island", "ID", "Year"))






