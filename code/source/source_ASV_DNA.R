# DNA Dataset Cleaning at ASV level
# Ana Miller-ter Kuile
# June 29, 2021

# this script takes in and cleans the  DNA dataset at the ASV level, 
# creating an output of DNA data for spiders across 
# islets with islet categories and associated 
# isotope data for those spiders that ahve isotopes
#attached to them

# Load packages and source ------------------------------------------------

library(here)

source(here("code", "source_isotopes.R"))

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

ASV <- DNA %>%
  filter(reads > 0) %>%
  filter(sample_str == "HEV") %>%
  filter(Order != "Primates") %>%
  dplyr::select(ASV,
                sample, 
                Class, 
                Order) %>%
  mutate(sample = str_sub(sample,1,nchar(sample)-1)) %>%
  mutate(presence = 1) 

DNA_meta <- DNA_meta %>%
  filter(ID == "Heteropoda venatoria") %>% #& Year == 2015) %>%
  mutate(Isotope_ID = word(Isotope_ID,2)) %>%
  dplyr::select(Island, Isotope_ID, Extraction.ID) %>%
  distinct(Island, Isotope_ID, Extraction.ID)

#combine ASV data with the isotope data 
ASV_iso <- spider_iso %>%
  filter(Year == 2015) %>%
  full_join(DNA_meta, by = c("Island", c("ID" = "Isotope_ID"))) %>%
  filter(!is.na(Extraction.ID)) %>%
  left_join(ASV, by = c("Extraction.ID" = "sample")) %>%
  filter(!Island %in% c("Cooper", "North Fighter")) %>%
  filter(!is.na(Order)) %>%
  dplyr::select(-Island_Area, -Island_prod, -prod_level,
                -size_level) %>%
  left_join(islands, by = "Island")

ASV_iso %>%
  group_by(Extraction.ID) %>%
  tally() %>%
  summarise(mean = mean(n))
  
