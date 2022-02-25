# Prey of Prey DNA Dataset Cleaning
# Ana Miller-ter Kuile
# September 30, 2021

# this script takes in and cleans the  DNA dataset, 
# creating an output of prey diet DNA data for spider prey items across 
# different canopy types, not a completely perfect proxy
# for productivity, but at least an exploratory approach?

# Load packages and source ------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load DNA Data -----------------------------------------------------------
#DNA interaction data
DNA <- read.csv(here("data", 
                     "DNA", 
                     "all_prey_DNA.csv"))

#metadata associated with these samples, including sample ID
DNA_meta <- read.csv(here("data",
                          "DNA",
                          "Sample_metadata.csv"))

# Tidy DNA Data -----------------------------------------------------------
#filter out just the intermediate predators of interest
# variables of interest, change ID name and make a presence column
# remove all zero interactions
DNA_int <- DNA %>%
  filter(sample_str %in% c("NEO", "SCY", "LRS", 
                           "SME")) %>%
  filter(Class != "Mammalia") %>%
  filter(ID_level %in% c("Order", "Family", "Genus", "Species")) %>%
  filter(reads > 0) %>%
  ungroup() %>%
  mutate(Class = case_when(Order == "Entomobryomorpha" ~ "Collembola",
                           TRUE ~ Class)) %>%
  dplyr::select(sample, 
                Class, 
                Order,
                reads) %>%
  mutate(sample = str_sub(sample,1,nchar(sample)-1)) %>%
  group_by(sample,Class, Order) %>%
  summarise(reads = sum(reads)) %>%
  mutate(presence = 1)

# get only intermediate predator data and get distinct and consistent
# naming of samples
DNA_intmeta <- DNA_meta %>%
  filter(ID %in% c("Scytodes longipes",
                   "Smeringopus pallidus",
                   "Neoscona theisi",
                   "Keijia mneon")) %>% #& Year == 2015) %>%
  group_by(Island, Habitat, Extraction.ID, ID) %>%
  summarise(Length_mm = mean(Length_mm, na.rm = TRUE)) %>%
  mutate(category = case_when(Habitat %in% c("PF", "PG", "TA") ~ "high",
                            TRUE ~ "low"))

#combine DNA data with metadata
DNA_intfull <- DNA_int %>%
  full_join(DNA_intmeta, by = c("sample" = "Extraction.ID")) %>%
  mutate(category = case_when(Habitat %in% c("PF", "PG", "TA") ~ "high",
                              TRUE ~ "low")) %>%
  filter(!is.na(Order)) %>%
  filter(Habitat %in% c("PG", "CN")) 

#get stats
DNA_intfull %>%
  ungroup() %>%
  distinct(sample) %>%
  tally()

DNA_intfull %>%
  distinct(sample, Habitat) %>%
  group_by(Habitat) %>%
  tally()

DNA_intfull %>%
  group_by(Habitat) %>%
  tally()

# Barplot visualization DFs -----------------------------------------------

#get frequency of different kinds of prey of prey by habitat cateogry
habitat_int <- DNA_intfull %>%
  group_by(Class, Order, Habitat) %>%
  summarise(Frequency = n())

#figure out sample size
sample_size_int <- DNA_intfull %>%
  ungroup() %>%
  distinct(sample,  Habitat) %>%
  group_by(Habitat) %>%
  tally(name = "sample_sz") 

#get % of pop that eats that thing
habitat_int2 <- habitat_int %>%
  left_join(sample_size_int, by = c("Habitat")) %>%
  mutate(percent = Frequency/sample_sz)


# Matrix and metadata for community analyses ------------------------------

#matrix of prey items by samples (predators)
intDNA_matrix <- DNA_intfull %>%
  ungroup() %>%
  dplyr::select(sample, Order, presence) %>%
  pivot_wider(names_from = Order,
              values_from = presence,
              values_fill = 0) %>%
  column_to_rownames(var = "sample")

#metadata associated with these samples for adonis and betapart
DNA_intmetadata <- DNA_intfull %>%
  ungroup() %>%
  distinct(sample, category, Habitat, ID) %>%
  column_to_rownames(var = "sample")

