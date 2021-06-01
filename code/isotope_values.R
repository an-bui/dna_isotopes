# Raw Isotope values
# Ana Miller-ter Kuile
# May 12, 2021

# this code looks at average d15N and d13C for spiders
# and sees if these values vary by islet size or productivity

# Load source ------------------------------------------------
library(here)
source(here("code", "source_isotopes.R"))

# Visualize ----------------------------------------------------------

ggplot(spider_iso, aes(x = prod_level, y = d15N_c, fill = size_level)) +
  geom_boxplot() +
  geom_point(aes(color = size_level), 
             position=position_jitterdodge(), alpha = 0.6) +
  theme_bw()

ggplot(spider_iso, aes(x = prod_level, y = d13C, fill = size_level)) +
  geom_boxplot() +
  geom_point(aes(color = size_level), 
             position=position_jitterdodge(), alpha = 0.6) +
  theme_bw()

plant_iso %>%
  filter(!Species %in% c("Asplenium", "Grass")) %>%
  mutate(type = case_when(Species %in% c("Algae", "homog-cladoph",
                                            "turf algae-homo",
                                            "turf algae-v") ~ "Algae",
                          Species %in% c("Cocos", "Phymatosorus",
                                         "Pisonia", "Scaevola",
                                         "Tournefortia") ~ "Terrestrial",
                             TRUE ~ Species)) %>%
ggplot(aes(x = d13C, y = d15N, color = type)) +
  geom_point(size = 3) +
  theme_bw() 
