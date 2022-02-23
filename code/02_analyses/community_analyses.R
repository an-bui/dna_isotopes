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

adonis(DNA_matrix ~ prod_level, data = DNA_metadata,
       method = "jaccard", nperm = 999)

beta.multi(DNA_matrix, index.family = "jaccard")  
dist<-beta.pair(DNA_matrix, index.family="jaccard")

#turnover
p <- betadisper(dist[[1]], DNA_metadata$prod_level)
plot(p)
#nestedness
p2 <- betadisper(dist[[2]], DNA_metadata$prod_level)
plot(p2)
#overall beta diversity
p3 <- betadisper(dist[[3]], DNA_metadata$prod_level)
plot(p3)

# Intermediate predator PERMANOVA and betapart ----------------------------

adonis(intDNA_matrix ~ category, data = DNA_intmetadata,
       method = "jaccard", nperm = 999)

dist<-beta.pair(intDNA_matrix, index.family="jaccard")

#turnover
p <- betadisper(dist[[1]], DNA_intmetadata$Habitat)
plot(p)
anova(p)
#nestedness
p2 <- betadisper(dist[[2]], DNA_intmetadata$Habitat)
plot(p2)
anova(p2)
#overall beta diversity
p3 <- betadisper(dist[[3]], DNA_intmetadata$Habitat)
boxplot(p3)
anova(p3)
plot(p3)

# Visualizations ----------------------------------------------------------

#Biplot with circles for each
#Barplot of most to least consumed items

#TOp predator bars
islet_prey2 %>%
  group_by(prod_level, Order) %>%
  summarise(mean = mean(percent, na.rm = T),
            sd = sd(percent, na.rm = T),
            total = n(),
            se = sd/sqrt(total)) %>%
  ggplot(aes(x = Order, y = mean, fill = prod_level)) +
  geom_bar(color = "black", stat = "identity", position = position_dodge(preserve = "single")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(width = .9, preserve = "single")) +
  theme_bw() + 
  labs(y = "Frequency (percentage of samples)", x = "Prey Order") +
  scale_fill_manual(values = c("#80cdc1",
                               "#bf812d")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#int predator bars


habitat_int2 %>%
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

