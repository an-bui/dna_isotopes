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
#removed because this method was less comparable based on the 
# comparison paper
#mcp_is <- estMCP(spider_iso_1,
#                 x = "d13C",
#                 y = "d15N_c",
#                 group= "Island")

#area_mcp <- getArea(mcp_is)

# join all the areas ------------------------------------------------------

area_all <- area_kin %>%
  bind_rows(area_sea) %>%
  #bind_rows(area_mcp) %>%
  left_join(islands, by = c("Group" = "Island")) %>%
  mutate(ConfInt = as.factor(ConfInt))

# Stats -------------------------------------------------------------------

ker95 <- area_all %>%
  filter(ConfInt == "95" & Method == "Kernel")

m1 <- glm(ShapeArea ~ prod_level, 
              data = ker95)

summary(m1)


ell95 <- area_all %>%
  filter(ConfInt == "95" & Method == "Ellipse")

m2 <- glm(ShapeArea ~ prod_level, 
              data = ell95)

summary(m2)

# Visualize ---------------------------------------------------------------

kernel_ellipse_95 <- area_all %>%
  filter(ConfInt == "95" & Method %in% c("Ellipse", "Kernel"))

ggplot(kernel_ellipse_95, aes(x = Method, y = ShapeArea, fill = prod_level)) +
  geom_boxplot() +
  labs(y = "95% isotopic niche area", 
       x = "Isotopic niche method",
       fill = "Islet productivity") +
  scale_fill_manual(values = c("#80cdc1",
                               "#bf812d")) +
  theme_bw()





