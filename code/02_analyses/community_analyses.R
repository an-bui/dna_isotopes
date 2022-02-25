# Community analyses
# Ana Miller-ter Kuile
# February 23, 2022

# this script compiles the community analyses for both intermediate 
# and top predators


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", "betapart", "vegan",
                  "patchwork")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

source(here("code",
            "01_cleaning",
            "top_DNA.R"))

source(here("code",
            "01_cleaning",
            "int_DNA.R"))


# Top predator PERMANOVA and betapart -------------------------------------

dist<-beta.pair(DNA_matrix, index.family="jaccard")

#overall beta diversity
anova(capscale(dist[[3]] ~ DNA_metadata$Habitat, 
               dist = "jaccard",
               comm = DNA_matrix))
#don't do turnover or nestedness b/c beta isn't different

# Intermediate predator PERMANOVA and betapart ----------------------------

disti<-beta.pair(intDNA_matrix, index.family="jaccard")

#overall beta diversity
anova(capscale(disti[[3]] ~ DNA_intmetadata$Habitat, 
               dist = "jaccard",
               comm = intDNA_matrix))
#is it turnover, nestedness, or both?
#turnover
anova(capscale(disti[[1]] ~ DNA_intmetadata$Habitat, 
         dist = "jaccard",
         comm = intDNA_matrix))
#nestedness
anova(capscale(disti[[2]] ~ DNA_intmetadata$Habitat, 
               dist = "jaccard",
               comm = intDNA_matrix))

# Visualizations ----------------------------------------------------------


# Barplot visuals ---------------------------------------------------------

#Barplot of most to least consumed items
#TOp predator bars

top_id <- ggplot(islet_prey, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme_bw() +
  labs(y = "Relative proportion of diet",
       title = "Top predators")

#int predator bars
int_id <- ggplot(habitat_int, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme_bw() +
  labs(y = "Relative proportion of diet",
       title = "intermediate predators") 


# Biplots -----------------------------------------------------------------
#Biplot with circles for each

capt <- capscale(DNA_matrix ~ DNA_metadata$Habitat, 
                 dist = "jaccard")

xt <- as.data.frame(scores(capt, display = "sites"))

top_comm <- xt %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(DNA_meta, by = "Extraction.ID") %>%
  ggplot(aes(x = CAP1, y = MDS1, color = Habitat)) +
  geom_point(size = 2) +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  scale_color_manual(values = c("#bf812d",
                                "#80cdc1")) +
  stat_ellipse(size = 1) +
  theme_bw() +
  labs(title = "Top predators")

capi <- capscale(intDNA_matrix ~ DNA_intmetadata$Habitat, 
                dist = "jaccard")

xi <- as.data.frame(scores(capi, display = "sites"))

int_comm <- xi %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(DNA_intmeta, by = "Extraction.ID") %>%
ggplot(aes(x = CAP1, y = MDS1, color = Habitat)) +
  geom_point(size = 2) +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  scale_color_manual(values = c("#bf812d",
                                "#80cdc1")) +
  stat_ellipse(size = 1) +
  theme_bw() +
  labs(title= "Intermediate predators")

biplot <- int_comm + top_comm 

ggsave(plot = biplot,
       filename = 'biplot_graphs.png',
       path = here("pictures", "R"),
       width = 6, height = 4,
       units = "in")

comp <- int_id + top_id

ggsave(plot = comp,
       filename = 'comp_graphs.png',
       path = here("pictures", "R"),
       width = 6, height = 4,
       units = "in")
