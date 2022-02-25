# Isotopic niche of top predators
# Ana Miller-ter Kuile
# February 25, 2022

# this script looks at isotopic niche of H venatoria


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "SIBER", "rKIN",
                  "DHARMa", "effects",
                  "glmmTMB")

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


# Visualizations ----------------------------------------------------------

iso_niche <- spider_iso %>%
  mutate(Island = factor(Island, 
                         levels = c("Castor", "Fern",
                                    "Holei","Kaula", "Paradise", 
                                    "Dudley", "Eastern",
                                    "Leslie", "Lost", "Sand"))) %>%
ggplot(aes(x = d13C, y = d15N_c, color = Island)) +
  geom_point() +
  labs(x = expression({delta}^13*C~ ('\u2030')), 
       y = expression({delta}^15*N~ ('\u2030'))) +
  scale_color_manual(values = c("#2D1A03", "#543005", 
                                "#8c510a","#bf812d",
                                "#dfc27d", "#c7eae5",
                                "#80cdc1", "#35978f", "#01665e", 
                                "#003c30", "#001511")) +
  stat_ellipse(aes(linetype = Habitat)) +
  theme_bw()

niche_box <- ggplot(all_niche, aes(x = Method, y = ShapeArea, fill = Habitat)) +
  geom_boxplot() +
  labs(y = "95% isotopic niche area", 
       x = "Isotopic niche method",
       fill = "Habitat") +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  theme_bw()
