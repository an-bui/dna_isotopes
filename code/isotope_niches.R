#rKIN explorations
#Ana Miller-ter Kuile
#May 5, 2021


# Load Packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "SIBER", "rKIN")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------

source(here("code", "source_isotopes.R"))

# Tidy --------------------------------------------------------------------

#remove islets with low resolution
spider_iso_1 <- spider_iso %>%
  filter(!Island %in% c("Aviation", "Frigate"))

# KUD ---------------------------------------------------------------------

# calculate kernel by islet
kin_is <- estKIN(spider_iso_1, 
              x = "d13C", 
              y = "d15N_c",
              group = "Island")

#get the areas of those kernels
area_kin <- getArea(kin_is)

# sea ---------------------------------------------------------------------

sea_is <- estEllipse(spider_iso_1,
                     x = "d13C",
                     y = "d15N_c",
                     group= "Island")

area_sea <- getArea(sea_is)


# mcp ---------------------------------------------------------------------

mcp_is <- estMCP(spider_iso_1,
                 x = "d13C",
                 y = "d15N_c",
                 group= "Island")

area_mcp <- getArea(mcp_is)

# join all the areas ------------------------------------------------------

area_all <- area_kin %>%
  bind_rows(area_sea) %>%
  bind_rows(area_mcp) %>%
  left_join(islands, by = c("Group" = "Island")) %>%
  mutate(ConfInt = as.factor(ConfInt))


# Visualize ---------------------------------------------------------------

ggplot(area_all, aes(x = prod_level, y = ShapeArea, fill = size_level)) +
  geom_boxplot() +
  facet_wrap(Method~ConfInt) +
  theme_bw()













