# Dissimilarities explorations
# Ana Miller-ter Kuile
# June 29, 2021

#this code creates a per-individual dissimilarity matrix 
# for both DNA diet as well as isotope values. This may or may not 
# work for the DNA data given that these data contain a lot of 
# zero-pairs

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "vegan")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load datasets -----------------------------------------------------------

#we'll be extracting information for the dissimiliarity
# matrices from the DNA_iso dataset derived from this source:
source(here("code", "source_ASV_DNA.R"))

# Create DNA matrix -------------------------------------------------------

#extract only the DNA order ID and sample ID and convert it to a matrix
# of presence-absence values
dna_mat <- ASV_iso %>%
  dplyr::select(Extraction.ID, ASV, presence) %>% #select columns of interest
  pivot_wider(names_from = ASV, #make DNA diet column names
              values_from = presence, #fill each column with presence value
              values_fill = 0) %>% #any missing values get zeros
  column_to_rownames(var = "Extraction.ID") #make extraction ID the row names

#calculate Jaccard dissimilarity, where 0 is completely similar, and 
# 1 is completely dissimilar
dna_dist <- as.matrix(vegdist(dna_mat, "jaccard")) 
dna_dist <- as.data.frame(dna_dist) #convert to a dataframe

#pivot this longer so that each pair is a row in the dataframe
dna_pairs <- dna_dist %>%
  rownames_to_column(var = "ID1") %>% #make the ID a column again
  pivot_longer(HEV67:HEV94, #collect all the sample columns
               names_to = "ID2", #make them the second ID row
               values_to = "ASV_Jaccard_diss") %>% #give the dissimilarity values a column
  filter(ID1 != ID2) #remove any self-comparisons in this dataset

# Create Isotope matrices -------------------------------------------------

iso_mat <- ASV_iso %>%
  dplyr::select(Extraction.ID, d15N_c, d13C) %>%
  distinct(Extraction.ID, d15N_c, d13C) %>%
  column_to_rownames(var = "Extraction.ID") %>%
  filter(!is.na(d15N_c))

#calculate euclidian dissimilarity, where 0 is completely similar, and 
# 1 is completely dissimilar
iso_dist <- as.matrix(vegdist(iso_mat, "euclidean")) 
iso_dist <- as.data.frame(iso_dist) #convert to a dataframe

#pivot this longer so that each pair is a row in the dataframe
iso_pairs <- iso_dist %>%
  rownames_to_column(var = "ID1") %>% #make the ID a column again
  pivot_longer(HEV67:HEV108, #collect all the sample columns
               names_to = "ID2", #make them the second ID row
               values_to = "ISO_Euclid_diss") %>% #give the dissimilarity values a column
  filter(ID1 != ID2) #remove any self-comparisons in this dataset

# Compare DNA and isotope dissimilarity -----------------------------------

diss <- dna_pairs %>%
  left_join(iso_pairs, by = c("ID1" = "ID1", "ID2" = "ID2"))

# Metadata for Islet attributes -------------------------------------

meta <- DNA_iso %>%
  distinct(Island,
           Length_mm, 
           Mass_g, 
           Extraction.ID,
           Island_Area,
           Island_prod,
           prod_level,
           size_level) 

meta1 <- meta %>%
  dplyr::select(Island, 
                Extraction.ID,
                prod_level,
                size_level) %>%
  rename("Island1" = "Island",
         "ID1" = "Extraction.ID",
         "prod1" = "prod_level",
         "size1" = "size_level")

meta2 <- meta %>%
  dplyr::select(Island, 
                Extraction.ID,
                prod_level,
                size_level) %>%
  rename("Island2" = "Island",
         "ID2" = "Extraction.ID",
         "prod2" = "prod_level",
         "size2" = "size_level")

diss_is <- diss %>%
  left_join(meta1, by = "ID1") %>%
  left_join(meta2, by = "ID2")

# Dissimilarity graphs ----------------------------------------------------

#DNA versus isotope distances:
diss_is %>%
  unite(prod_pair, c("prod1", "prod2"), sep = "_") %>%
  filter(prod_pair != "high_low") %>%
  ggplot(aes(x = ISO_Euclid_diss, 
             y = ASV_Jaccard_diss, 
             color = prod_pair)) +
  geom_point() +
  theme_bw()

#DNA dissimilarity by islet productivity
diss_is %>%
  unite(prod_pair, c("prod1", "prod2"), sep = "_") %>%
  unite(size_pair, c("size1", "size2"), sep = "_") %>%
  filter(prod_pair != "high_low") %>%
  mutate(size_pair = case_when(size_pair == "big_small" ~ "small_big",
                               TRUE ~ size_pair)) %>%
  #filter(ASV_Jaccard_diss < 1) %>%
  ggplot(aes(x = prod_pair, 
             y = ASV_Jaccard_diss,
             fill= size_pair)) +
  geom_boxplot() +
  theme_bw() 

#isotope dissimilarity by islet productivity
diss_is %>%
  unite(prod_pair, c("prod1", "prod2"), sep = "_") %>%
  unite(size_pair, c("size1", "size2"), sep = "_") %>%
  filter(prod_pair != "high_low") %>%
  mutate(size_pair = case_when(size_pair == "big_small" ~ "small_big",
                               TRUE ~ size_pair)) %>%
  ggplot(aes(x = prod_pair, 
             y = ISO_Euclid_diss, 
             fill = size_pair)) +
  geom_boxplot() +
  theme_bw() 

#how many per group of comparisons?
diss_is %>%
  unite(prod_pair, c("prod1", "prod2"), sep = "_") %>%
  group_by(prod_pair) %>%
  filter(prod_pair != "high_low") %>%
  tally()



