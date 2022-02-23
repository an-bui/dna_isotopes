# Load Data ---------------------------------------------------------------
library(here)
library(rKIN)
source(here("code", "source_isotopes.R"))

# Tidy --------------------------------------------------------------------

#remove islets with low resolution
spider_iso_1 <- spider_iso %>%
  filter(!Island %in% c("Aviation", "Frigate"))

# Split by islet ----------------------------------------------------------

islets <- split(spider_iso_1, paste(spider_iso_1$Island))

# calculate kernel by islet
kud_plot <- function(df) {

  kin <- estKIN(df,
                x = "d13C",
                y = "d15N_c", 
                group = "Island")
  
  plot <- plotKIN(kin)
  
  return(plot)
  
}

kud_plot(islets$Castor)
kud_plot(islets$Dudley)

plots <- lapply(islets, kud_plot)
library(patchwork)

wrap_plots(plots)

# Big vs. Small -----------------------------------------------------------
big <- spider_iso_1 %>%
  filter(Island %in% c("Sand", "Eastern", "Kaula", "Holei", "Paradise"))

big_plots <- kud_plot(big)

small <- spider_iso_1 %>%
  filter(Island %in% c("Castor", "Fern", "Lost", "Dudley", "Leslie"))

small_plots <- kud_plot(small)

big_plots + small_plots

kud_plot(spider_iso_1)

spider_iso_1 <- spider_iso_1 %>%
  mutate(Island = factor(Island, levels = c("Castor", "Fern", 
                                            "Paradise", "Holei",
                                            "Kaula", "Dudley",
                                            "Leslie", "Lost",
                                            "Sand", "Eastern")))

kin <- estKIN(spider_iso_1,
              x = "d13C",
              y = "d15N_c", 
              group = "Island",
              levels = c(95))

plotKIN(kin)
