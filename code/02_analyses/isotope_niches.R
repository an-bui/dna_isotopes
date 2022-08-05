# Isotopic niche of top predators
# Ana Miller-ter Kuile
# February 25, 2022

# this script looks at isotopic niche of H venatoria


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "SIBER", "rKIN",
                  "DHARMa", "effects",
                  "glmmTMB", "emmeans")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

source(here("code",
            "01_cleaning",
            "top_isotopes.R"))


# Calculate niches --------------------------------------------------------


# KUD ---------------------------------------------------------------------

# calculate kernel by islet
kin_is <- estKIN(spider_iso, 
                 x = "d13C", 
                 y = "d15N_c",
                 group = "Island",
                 smallSamp = TRUE)

#get the areas of those kernels
area_kin <- getArea(kin_is)

# sea ---------------------------------------------------------------------

sea_is <- estEllipse(spider_iso,
                     x = "d13C",
                     y = "d15N_c",
                     group= "Island",
                     smallSamp = TRUE)

area_sea <- getArea(sea_is)


# Combine -----------------------------------------------------------------

meta <- spider_iso %>%
  distinct(Island, prod_level, Habitat)

all_niche <- rbind(area_kin, area_sea) %>%
  left_join(meta, by = c('Group' = "Island")) %>%
  filter(ConfInt == 95)

# Statistics --------------------------------------------------------------

m1 <- glm(ShapeArea ~ Habitat*Method, data = all_niche)
summary(m1)

simulateResiduals(m1, plot = T)
plot(allEffects(m1))

summary(m1)

# Visualizations ----------------------------------------------------------


