# Load Data ---------------------------------------------------------------

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
