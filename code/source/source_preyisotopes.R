# Prey Isotope dataset prep
# Ana Miller-ter Kuile
# August 9, 2021

# This script cleans the prey isotope values for analyses, using 
# some of the same approaches as that used on the top predator isotope dataset.

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", "readxl", "lubridate")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load source -------------------------------------------------------------

# this script will use some datasets imported in the top predator isotope
# data

source(here("code", "source", "source_isotopes.R"))

#datasets needed from here islands, plant_iso 

# Load prey data ----------------------------------------------------------

files <- list.files(here("data", 
                         "isotopes",
                         "prey"),
                    pattern = "*.xlsx",
                    full.names = TRUE)


# import that list of dataframes, now a list of dataframes, into R
data_list <-  lapply(files, read_xlsx)

# Per Dataset Cleaning ----------------------------------------------------

bugs_09 <- data_list[[1]] %>%
  mutate(Year = 2009) %>%
  rename("Island" = "Island name") %>%
  dplyr::select(Organism, Year, Island,
                d15N, d13C) %>%
  filter(Organism %in% c("ant", "diptera", "moth"))

spiders_10 <- data_list[[2]] %>%
  mutate(Year = 2010) %>%
  rename("Island" = "ID#2",
         "Organism" = "Species")  %>%
  dplyr::select(Organism, Year, Island,
                d15N, d13C)

bugs_10 <- data_list[[3]] %>%
  mutate(Year = 2010) %>%
  rename("Island" = "ID#1") %>%
  dplyr::select(Organism, Year, Island,
                d15N, d13C)
         
spiders_12 <- data_list[[4]] %>%
  mutate(Year = 2012) %>%
  rename("Island" = "ID#1") %>%
  dplyr::select(Organism, Year, Island,
                d15N, d13C)

prey_iso <- bind_rows(bugs_09, bugs_10, spiders_10, spiders_12)
  
#Diptera - 2009_Palmyra_Insect_Amphipod_Isopod_Isotopes
#Hempitera - 2010_Palmyra_Insect_Isotopes
#Lepidoptera - 2009_Palmyra_Insect_Amphipod_Isopod_Isotopes
#Araneae - 2010_Palmyra_Day_Spider_Isotopes.xlsx & 2012_Palmyra_Day_Spider_Isotopes.xlsx
#Orthoptera - 2010_Palmyra_Insect_Isotopes

# All data cleaning -------------------------------------------------------

unique(prey_iso$Organism)

#give organisms an order level ID
prey_iso <- prey_iso %>%
  mutate(Order = case_when(Organism == "ant" ~ "Hymenoptera",
                           Organism == "diptera" ~ "Diptera",
                           Organism == "moth" ~ "Lepidoptera",
                           Organism == "Dysmicoccus sp." ~ "Hemiptera",
                           Organism %in% c("Phisis holdhausi", 
                                           "Gryllidae") ~ "Orthoptera",
                           Organism %in% c("Neoscona theisi", "Salticidae", 
                                           "Smeringopus pallidus", 
                                           "Scytodes sp.") ~ "Araneae",
                           TRUE ~ NA_character_))

# make island naming convention consistent with islands dataset
prey_iso <- prey_iso %>%
  mutate(Island = case_when(Island %in% c("Papala", "Papala PG/TA",
                                          "Papala TA",
                                          "Papala PS",
                                          "Papala PG") ~ "Papala",
                            Island %in% c("Sand TA", 
                                          "Sand PS",
                                          "Sand PG",
                                          "Sand") ~ "Sand",
                            Island %in% c("Fern TA", 
                                          "Fern") ~ "Fern",
                            Island %in% c("Home",           
                                          "Home PG",        
                                          "Home TA") ~ "Home NE",
                            Island =="Pollucks" ~ "Pollucks",
                            Island == "Castor PG" ~ "Castor",
                            Island %in% c("Lost PS", 
                                          "Lost PG", 
                                          "Lost") ~ "Lost",
                            Island %in% c("N.fighter PS",
                                          "N.fighter PG",
                                          "N.Fighter",
                                          "N.Fighter TA",
                                          "N. Fighter",
                                          "N. fighter") ~ "North Fighter",
                            Island %in% c("Ainsley",
                                          "Ainsley TA") ~ "Ainsley",
                            Island %in% c("Eastern", "New East",
                                          "Eastern TA",
                                          "Eastern PG",
                                          "Eastern PS",
                                          "East") ~ "Eastern",
                            Island %in% c("Paradise", 
                                          "Paradise PG",
                                          "Paradise TA",
                                          "Para") ~ "Paradise",
                            Island %in% c("Aviation",
                                          "Aviation TA") ~ "Aviation",
                            Island %in% c("Port", 
                                          "Portsmouth Scae",
                                          "Portsmouth",
                                          "Portsmouth PG",
                                          "Portsmouth TA") ~ "Portsmouth",
                            Island %in% c("Kaul",
                                          "Kaula",
                                          "Kaula TA",
                                          "Kaula PG") ~ "Kaula",
                            Island %in% c("Hole", "Holei",
                                          "Holei TA",
                                          "Holei PG") ~ "Holei",
                            Island %in% c("Frig", "Frigate",
                                          "Frigate PS",
                                          "Frigate PG",
                                          "Frigate TA") ~ "Frigate",
                            Island %in% c("Enfi",
                                          "Engi",
                                          "Engineer",
                                          "Enfineer",
                                          "Engineer PG",
                                          "Engineer TA") ~ "Engineer",
                            Island %in% c("Dudl",
                                          "Dudley TA",
                                          "Dudley PG",
                                          "Dudley") ~ "Dudley",
                            Island %in% c("Cast", "Castor") ~ "Castor",
                            Island %in% c("NS Causeway",
                                          "NSCauseway",
                                          "NSCauseway TA",
                                          "NSCauseway PG") ~ "NS Causeway",
                            Island %in% c("Whipoorwill",
                                          "Whip",
                                          "Whipoorwill PG",
                                          "Whipoorwill TA") ~ "Whipoorwill",
                            Island %in% c("S.fighter",
                                          "S.Fighter PG",
                                          "S. Fighter") ~ "South Fighter",
                            Island == "Sacia" ~ "Sacia",
                            Island %in% c("Leslie", 
                                          "Leslie Scae",
                                          "Leslie PS",
                                          "Leslie TA",
                                          "Leslie PG") ~ "Leslie",
                            TRUE ~ NA_character_))

#join with plant and island datasets
prey_iso <- prey_iso %>%
  left_join(plant_iso_2, by = c("Island" = "Island.name"))  %>%
  cbind(marine_iso) %>%
  cbind(guano_iso) %>%
  mutate(d15N_c = isotope_correction(d15_plant = plant_d15N,  
                                     d13_plant = plant_d13C,
                                     d15_marine = marine_d15N, 
                                     d13_marine = marine_d15N,
                                     d15_consumer = d15N, 
                                     d13_consumer = d13C)) %>%
  filter(!is.na(plant_d15N)) %>%
  left_join(islands, by = "Island") %>%
  filter(!is.na(prod_level))
 
