# Prey of Prey DNA Dataset Cleaning
# Ana Miller-ter Kuile
# September 30, 2021

# this script takes in and cleans the  DNA dataset, 
# creating an output of prey diet DNA data for spider prey items across 
# different canopy types, not a completely perfect proxy
# for productivity, but at least an exploratory approach?

# Load packages and source ------------------------------------------------

library(here)
library(tidyverse)

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
DNA_prey <- DNA %>%
  filter(sample_str %in% c("NEO", "LRS", "SCY")) %>%
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
DNA_preymeta <- DNA_meta %>%
  filter(ID %in% c("Keijia mneon",
                   "Scytodes longipes",
                   "Neoscona theisi")) %>% #& Year == 2015) %>%
  group_by(Island, Habitat, Extraction.ID, ID) %>%
  summarise(Length_mm = mean(Length_mm, na.rm = TRUE)) %>%
  mutate(category = case_when(Habitat %in% c("PF", "PG", "TA") ~ "high",
                            TRUE ~ "low"))

#combine DNA data with metadata
DNA_preyfull <- DNA_prey %>%
  full_join(DNA_preymeta, by = c("sample" = "Extraction.ID")) %>%
  mutate(category = case_when(Habitat %in% c("PF", "PG", "TA") ~ "high",
                              TRUE ~ "low")) %>%
  filter(!is.na(Order))

#get frequency of different kinds of prey of prey by habitat cateogry
habitat_prey <- DNA_preyfull %>%
  group_by(Class, Order, category) %>%
  summarise(Frequency = n())

sample_size <- DNA_preyfull %>%
  ungroup() %>%
  distinct(sample,  category) %>%
  group_by(category) %>%
  tally(name = "sample_sz") 

habitat_prey2 <- habitat_prey %>%
  left_join(sample_size, by = c("category")) %>%
  mutate(percent = Frequency/sample_sz)

habitat_prey2 %>%
  mutate(trophic = case_when(Order %in% c("Araneae") ~ "Predators",
                             Order %in% c("Sarcoptiformes",
                                          "Entomobryomorpha",
                                          "Blattodea",
                                          "Coleoptera",
                                          "Diptera",
                                          "Hymenoptera",
                                          "Lepidoptera",     
                                          "Orthoptera",
                                          "Psocoptera",
                                          "Thysanoptera") ~ "Omnivores",
                             TRUE ~ "Herbivores")) %>%
ggplot(aes(x = Order, y = percent, fill = category)) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.9, preserve = "single")) +
  theme_bw() +
  facet_grid(~ trophic, scales = "free_x", space = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust= 1))


preyDNA_matrix <- DNA_preyfull %>%
  ungroup() %>%
  dplyr::select(sample, Order, presence) %>%
  pivot_wider(names_from = Order,
              values_from = presence,
              values_fill = 0) %>%
  column_to_rownames(var = "sample")

DNA_metadata <- DNA_preyfull %>%
  ungroup() %>%
  distinct(sample, category, ID) %>%
  column_to_rownames(var = "sample")
library(vegan)

adonis(preyDNA_matrix ~ category, data = DNA_metadata,
       method = "jaccard", nperm = 999)

  
  
  
  