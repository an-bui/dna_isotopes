#ASV NMDS
#Ana Miller-ter Kuile
#June 30, 2021

# I'm going to try an NMDS with prey ASVs - see what happens

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
source(here("code", "source_DNA.R"))
# Create DNA matrix -------------------------------------------------------

#extract only the DNA order ID and sample ID and convert it to a matrix
# of presence-absence values
dna_mat <- ASV_iso %>%
  dplyr::select(Extraction.ID, ASV, presence) %>% #select columns of interest
  pivot_wider(names_from = ASV, #make DNA diet column names
              values_from = presence, #fill each column with presence value
              values_fill = 0) %>% #any missing values get zeros
  column_to_rownames(var = "Extraction.ID") #make extraction ID the row names

dna_mat2 <- DNA_iso %>%
  dplyr::select(Extraction.ID, Order, presence) %>% #select columns of interest
  pivot_wider(names_from = Order, #make DNA diet column names
              values_from = presence, #fill each column with presence value
              values_fill = 0) %>% #any missing values get zeros
  column_to_rownames(var = "Extraction.ID") #make extraction ID the row names


ord.nmds1 <- metaMDS(dna_mat2, distance="jaccard")
stressplot(ord.nmds1)
plot(ord.nmds1)

ASV_iso %>%
  group_by(Extraction.ID) %>%
  summarise(total = n()) %>%
  summarise(mean = mean(total),
            sd = sd(total),
            n = n(),
            se = sd/sqrt(n),
            median = median(total))


# First step is to calculate a distance matrix. 
# Here we use Jaccard distance metric
dist <- vegdist(dna_mat,  method = "jaccard")
dist2 <- vegdist(dna_mat2, method = "jaccard")
# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)
PCOA2 <- pcoa(dist2)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
barplot(PCOA2$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")
PCOA2 <- pcoa(dist2, correction = "cailliez")
# Plot your results
biplot.pcoa(PCOA2)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA2, dna_mat2)  

env <- ASV_iso %>%
  distinct(Extraction.ID, 
                Island, 
                d15N_c, 
                d13C, 
                Mass_g, 
                prod_level,
                size_level)
  

vectors <- as.data.frame(PCOA$vectors)
vectors2 <- as.data.frame(PCOA2$vectors)
vectors <- vectors %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(env, by = "Extraction.ID")

vectors2 <- vectors2 %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(env, by = "Extraction.ID")
  
# Basic plot with individuals
ggplot(vectors, aes(Axis.1, Axis.2, 
                    color= Island)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_bw()

ggplot(vectors2, aes(Axis.1, Axis.2, 
                    color= Island)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_bw()



