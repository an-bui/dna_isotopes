# Isotope Dataset Cleaning
# Ana Miller-ter Kuile
# May 5, 2021

# this script takes in and cleans the isotope data creating
# isotope dataframe ready for SIBER and KIN analyses with
# isotopes, body sizes, and islet categories

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", "readxl", "lubridate",
                  'patchwork')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#for function to get trophic position with correction function
source(here("code",
            "00_functions",
            "tidy_functions.R"))

theme_set(theme_bw())

# Load Data ---------------------------------------------------------------
#spider isotope data
spider_iso <- read.csv(here("data", 
                            "isotopes", 
                            "top",
                            "2009-2015_Cane Spider Isotopes.csv"))
#plant isotope data
plant_iso <- read.csv(here("data", 
                           "isotopes", 
                           "baseline",
                           "2009-2012 Palmyra Plant Isotope Data.csv"))
#spider size data
spider_size <- read.csv(here("data", 
                             "size", 
                             "Spider sizes 2009-2015.csv"))
#island size and productivity
islands <- read.csv(here('data', 
                         "environmental",
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
  summarise(plant_d15N = mean(d15N),
            plant_d13C = mean(d13C))

# using the plant isotopes (which includes the marine as well)
# get a baseline for marine wrack
marine_iso <- plant_iso %>%
  filter(Species %in% c("Algae", "homog-cladoph",
                        "turf algae-v", "turf algae-homo")) %>%
  summarise(marine_d15N = mean(d15N),
            marine_d13C = mean(d13C))

guano_iso <- plant_iso %>%
  filter(Species == "Guano") %>%
  summarise(guano_d15N = mean(d15N, na.rm = T),
            guano_d13C = mean(d13C, na.rm = T))
  

#categorize islet size and productivity into high/low, big/small
islands <- islands %>%
  mutate(prod_level = ifelse(Island_prod > 0.008707850, 
                             "high", "low"))

islands %>%
  filter(!is.na(Island_prod)) %>%
  group_by(prod_level) %>%
  summarise(mean = mean(Island_prod, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = "prod_level",
              values_from = "mean") %>%
  mutate(diff = high/low)

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
  cbind(marine_iso) %>%
  cbind(guano_iso) %>%
  mutate(d15N_c = isotope_correction(d15_plant = plant_d15N,  
                                     d13_plant = plant_d13C,
                                     d15_marine = marine_d15N, 
                                     d13_marine = marine_d15N,
                                     d15_consumer = d15N, 
                                     d13_consumer = d13C)) %>%
  filter(!Island %in% c("Ainsley", "Whipoorwill", "Home")) %>%
  filter(!is.na(plant_d15N)) %>%
  left_join(islands, by = "Island") %>%
  #guano d15N_C correction
  mutate(d15N_g = case_when(prod_level == "high" ~ isotope_correction(d15_plant = plant_d15N,  
                                                                      d13_plant = plant_d13C,
                                                                      d15_marine = guano_d15N, 
                                                                      d13_marine = guano_d13C,
                                                                      d15_consumer = d15N, 
                                                                      d13_consumer = d13C),
                            prod_level == "low" ~ d15N_c,
                            TRUE ~ NA_real_)) %>%
  mutate(d15N_g2 = isotope_correction(d15_plant = plant_d15N,  
                                      d13_plant = plant_d13C,
                                      d15_marine = guano_d15N, 
                                      d13_marine = guano_d13C,
                                      d15_consumer = d15N, 
                                      d13_consumer = d13C)) %>%
  left_join(spider_size2, by = c("Island", "ID", "Year")) 


spider_iso <- spider_iso %>%
  mutate(Habitat = case_when(Island %in% c("Castor", "Fern", "Paradise",
                                           "Holei", "Kaula") ~ "CN",
                             Island %in% c("Dudley", "Leslie", "Lost",
                                           "Eastern", "Sand") ~ "PG")) %>%
  filter(!Island %in% c("Aviation", "Frigate"))

spider_iso %>%
  group_by(Island) %>%
  tally()
spider_iso %>%
  group_by(Habitat) %>%
  tally()
spider_iso %>%
  tally()

spider_iso %>%
  group_by(prod_level) %>%
  summarise(mean_plant = mean(plant_d15N),
            mean_d15 = mean(d15N),
            mean_d15c = mean(d15N_c),
            mean_d15g = mean(d15N_g))

a <- ggplot(spider_iso, aes(x= prod_level, y = plant_d15N)) +
  geom_boxplot()

b <- ggplot(spider_iso, aes(x = prod_level, y = d15N)) +
  geom_boxplot()

c <- ggplot(spider_iso, aes(x = prod_level, y = d15N_c)) +
  geom_boxplot()


d <- spider_iso %>%
  mutate(d15N_p = d15N - plant_d15N) %>%
  ggplot(aes(x = prod_level, y = d15N_p)) +
  geom_boxplot()

a + b+ c +d

e <- ggplot(spider_iso, aes(x = d15N_c, y = d15N_g)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point() +
  labs(x = "Trophic position-marine wrack",
       y = "Trophic position-guano for high-productivity")


f <- ggplot(spider_iso, aes(x = d15N_c, y = d15N_g2)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point()+
  labs(x = "Trophic position-marine wrack",
       y = "Trophic position-guano")

baseline <- e + f +
  plot_annotation(tag_levels = "A")

ggsave(plot = baseline,
       filename = 'baseline_supp.png',
       path = here("pictures", "R"),
       width = 8, height = 5,
       units = "in")
