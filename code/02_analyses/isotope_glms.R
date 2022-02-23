# Trophic data glmm
# Ana Miller-ter Kuile
# February 23, 2022

# this script runs glmm for the trophic (isotope) data for top and intermediate predators



# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", "glmmTMB",
                  "effects", "MuMIn", "DHARMa",
                  "patchwork")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

#top preds:
source(here("code", 
            "01_cleaning",
            "top_isotopes.R"))
#df = spider_iso

#int preds
source(here("code", 
            "01_cleaning",
            "int_isotopes.R"))
#df = int_iso


# Combine DFs -------------------------------------------------------------

colnames(spider_iso)
spider_iso <- spider_iso %>%
  dplyr::select(Island, d13C, Year, d15N_c, prod_level) %>%
  mutate(Organism = "Heteropoda venatoria",
         group = "top")

colnames(int_iso)
int_iso <- int_iso %>%
  dplyr::select(Organism, Island, d13C, Year, d15N_c, prod_level) %>%
  mutate(group = "intermediate")

all_iso <- rbind(spider_iso, int_iso)
# Models ------------------------------------------------------------------

#d15 N by productivity level and predator group
m1 <- glmmTMB(d15N_c ~ prod_level*group + (1|Year) + (1|Island),
              data = all_iso)

summary(m1)
plot(allEffects(m1))
simulateResiduals(m1, plot = T)

#d14 C by productivity level and predator group
m2 <- glmmTMB(d13C ~ prod_level*group + (1|Year) + (1|Island),
              data = all_iso)

summary(m2)
plot(allEffects(m2))
simulateResiduals(m2, plot = T)

# Visualizations ----------------------------------------------------------

d15 <- ggplot(all_iso, aes(x = prod_level, y = d15N_c, fill = prod_level)) +
  geom_boxplot(size =1, alpha = 0.6) +
  geom_point(aes(color = prod_level), 
             position=position_jitterdodge()) +
  theme_bw() +  
  labs(y = expression({delta}^15*N~ ('\u2030')), 
       x = "Islet productivity",
       fill = "Islet productivity") +
  scale_fill_manual(values = c("#80cdc1",
                               "#bf812d")) +
  scale_color_manual(values = c("#80cdc1",
                                "#bf812d")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  facet_grid(~group)


d13 <- ggplot(all_iso, aes(x = prod_level, y = d13C, fill = prod_level)) +
  geom_boxplot(size =1, alpha = 0.6) +
  geom_point(aes(color = prod_level), 
             position=position_jitterdodge()) +
  theme_bw() +  
  labs(y = expression({delta}^13*C~ ('\u2030')), 
       x = "Islet productivity",
       fill = "Islet productivity") +
  scale_fill_manual(values = c("#80cdc1",
                               "#bf812d")) +
  scale_color_manual(values = c("#80cdc1",
                                "#bf812d")) +
  theme(legend.position = "none") +
  facet_grid(~group)

iso_graphs <- d15/d13

ggsave(plot = iso_graphs,
       filename = 'iso_graphs.png',
       path = here("pictures", "R"),
       width = 4, height = 5,
       units = "in")
