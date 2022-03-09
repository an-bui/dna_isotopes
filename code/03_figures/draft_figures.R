
# Load libraries and dependent scripts ------------------------------------


library(here)

#Capscale DNA analyses
source(here("code",
            "02_analyses",
            "community_analyses.R"))

#isotope niche analyses
source(here("code",
            "02_analyses",
            "isotope_niches.R"))



# Figure 1: Top pred niche stuff ------------------------------------------


#Fig1 for top preds - A isotope niche, b DNA niche, 
##### c. niche size boxplot, D. diet proportion

# Figure 1a: Isotopic Niche ------------------------------------------------

#this is a biplot of c and N isotopes color and linetype 
#of ellipses categorized by habitat type
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


# Figure1b: DNA niche -----------------------------------------------------

# this is the DNA niche graph bi-plot color coded
# by habitat with some elipses
top_comm <- xt %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(DNA_meta, by = "Extraction.ID") %>%
  ggplot(aes(x = CAP1, y = MDS1, color = Habitat)) +
  geom_point(size = 2) +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  scale_color_manual(values = c("#bf812d",
                                "#80cdc1")) +
  stat_ellipse(size = 1) +
  theme_bw() +
  labs(title = "Top predators")

# Fig1c: isotopic niche box plot -------------------------------------------

#this is a boxplot showing the 95% area of the niche
# for two different methods for isotopes
niche_box <- ggplot(all_niche, aes(x = Method, y = ShapeArea, fill = Habitat)) +
  geom_boxplot() +
  labs(y = "95% isotopic niche area", 
       x = "Isotopic niche method",
       fill = "Habitat") +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  theme_bw()


# Figure 1D: Diet composition ---------------------------------------------

#this is the proportional composition graph of what the
# top preds are eating

# this is the one I feel like - I would like to order by
# most-least abundant for visual aid, in this case like 75-95% of
# prey base is the same between CN and PG habitat, i'd like this
#somewhat complex-looking graph to quickly convey that better
# than it does 
top_id <- ggplot(islet_prey, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme_bw() +
  labs(y = "Relative proportion of diet",
       title = "Top predators")


# Fig1: Put together and save ---------------------------------------------


fig1 <- (iso_niche + top_comm)/(niche_box + top_id)
fig1

ggsave(plot = fig1,
       filename = 'top_niche.png',
       path = here("pictures", "R"),
       width = 7, height = 7,
       units = "in")



# Figure 2: Intermediate/Secondary Preds ----------------------------------


# Fig2: A. DNA niche, B. diet proportion


# Fig2a: Secondary DNA niche ----------------------------------------------

#this is the biplot of DNA communities showing that intermediate
#secondary predators shift  their diet across habitats
int_comm <- xi %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(DNA_intmeta, by = "Extraction.ID") %>%
  ggplot(aes(x = CAP1, y = MDS1, color = Habitat)) +
  geom_point(size = 2) +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  scale_color_manual(values = c("#bf812d",
                                "#80cdc1")) +
  stat_ellipse(size = 1) +
  theme_bw() +
  labs(title= "Intermediate predators")


# Fig2b: Secondary Pred prey composition ---------------------------------------

#int predator proportional bar graph
# again, I think ordering this by shared prsence and/or relative
#abundance would help visualize that the diet has shifted a fair bit
int_id <- ggplot(habitat_int, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme_bw() +
  labs(y = "Relative proportion of diet",
       title = "intermediate predators") 


# Figure2: Put it together and output -------------------------------------


fig2 <- int_comm + int_id
fig2
ggsave(plot = fig2,
       filename = 'int_niche.png',
       path = here("pictures", "R"),
       width = 7, height = 4,
       units = "in")
