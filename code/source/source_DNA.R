# DNA Dataset Cleaning
# Ana Miller-ter Kuile
# May 5, 2021

# this script takes in and cleans the  DNA dataset, 
# creating an output of DNA data for spiders across 
# islets with islet categories and associated 
# isotope data for those spiders that ahve isotopes
#attached to them

# Load packages and source ------------------------------------------------

library(here)

source(here("code", 
            "source", 
            "source_isotopes.R"))

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
#filter out just the cane spiders and
# variables of interest, change ID name and make a presence column
# remove all zero interactions
DNA <- DNA %>%
  filter(sample_str == "HEV") %>%
  filter(Order != "Primates") %>%
  filter(ID_level %in% c("Order", "Family", "Genus", "Species")) %>%
  filter(reads > 0) %>%
  dplyr::select(sample, 
                Class, 
                Order,
                reads) %>%
  mutate(sample = str_sub(sample,1,nchar(sample)-1)) %>%
  group_by(sample, Class, Order) %>%
  summarise(reads = sum(reads)) %>%
  mutate(presence = 1)

# get only cane spider data and get distinct and consistent
# naming of samples
DNA_meta <- DNA_meta %>%
  filter(ID == "Heteropoda venatoria") %>% #& Year == 2015) %>%
  mutate(Isotope_ID = word(Isotope_ID,2)) %>%
  dplyr::select(Island, Isotope_ID, Extraction.ID) %>%
  distinct(Island, Isotope_ID, Extraction.ID) 

#combine DNA data with the isotope data 
DNA_iso <- spider_iso %>%
  filter(Year == 2015) %>%
  full_join(DNA_meta, by = c("Island", c("ID" = "Isotope_ID"))) %>%
  filter(!is.na(Extraction.ID)) %>%
  left_join(DNA, by = c("Extraction.ID" = "sample")) %>%
  filter(!Island %in% c("Cooper", "North Fighter")) %>%
  filter(!is.na(Order)) %>%
  dplyr::select(-Island_Area, -Island_prod, -prod_level,
                -size_level) %>%
  left_join(islands, by = "Island")

#get frequency of different kinds of prey by islet populations
islet_prey <- DNA_iso %>%
  group_by(Island, prod_level, size_level, Class, Order) %>%
  summarise(Frequency = n())

sample_size <- DNA_iso %>%
  distinct(Island, Extraction.ID) %>%
  group_by(Island) %>%
  tally(name = "sample_sz") 

islet_prey2 <- islet_prey %>%
  left_join(sample_size, by = "Island") %>%
  mutate(percent = Frequency/sample_sz)

DNA_iso %>%
  distinct(Extraction.ID) %>%
  tally()

DNA_matrix <- DNA_iso %>%
  ungroup() %>%
  dplyr::select(Extraction.ID, Order, presence) %>%
  pivot_wider(names_from = Order,
              values_from = presence,
              values_fill = 0) %>%
  column_to_rownames(var = "Extraction.ID")

DNA_metadata <- DNA_iso %>%
  ungroup() %>%
  dplyr::select(Extraction.ID, prod_level, Island) %>%
  distinct(prod_level, Island, Extraction.ID) %>%
  column_to_rownames(var = "Extraction.ID")

adonis(DNA_matrix ~ prod_level, data = DNA_metadata,
       method = "jaccard", nperm = 999)
