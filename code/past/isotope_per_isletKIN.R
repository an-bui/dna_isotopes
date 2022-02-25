# Load Data ---------------------------------------------------------------
library(here)
library(rKIN)

source(here("code",  
            "01_cleaning",
            "top_isotopes.R"))

# Split by islet ----------------------------------------------------------

islets <- split(spider_iso, paste(spider_iso$Island))

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


spider_iso <- spider_iso %>%
  mutate(Island = factor(Island, levels = c("Castor", "Fern",
                                            "Paradise", "Holei",
                                            "Dudley","Leslie",
                                            "Lost", "Sand", 
                                            "Eastern")))

kin <- estKIN(spider_iso,
              x = "d13C",
              y = "d15N_c", 
              group = "Island",
              levels = c(95),
              smallSamp = TRUE)

plotKIN(kin)
