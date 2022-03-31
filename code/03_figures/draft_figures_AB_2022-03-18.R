
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

# Question 1: Does top predator isotopic trophic niche shift across environmental context (i.e. native-dominant, invasive-dominant)?
# invasive = dotted, native = solid

# just a thought - I don't think you need the islands here because you're basically using them
# as "replicates" within the "habitat type", which could cut down on the colors you're using?
# you could also put the island figure into the supplement if you wanted to present that data?

iso_niche_opt2 <- spider_iso %>%
  ggplot(aes(x = d13C, y = d15N_c, col = Habitat, shape = Habitat)) +
  geom_point(size = 2) +
  labs(x = expression({delta}^13*C~ ('\u2030')), 
       y = expression({delta}^15*N~ ('\u2030'))) +
  scale_color_manual(values = c("#bf812d",
                                "#80cdc1")) +
  stat_ellipse(size = 1) +
  # in plot annotations of CN and PG
  annotate("text", x = -26, y = 7.2, label = "CN", size = 12, col = "#80cdc1") +
  annotate("text", x = -20, y = 3.5, label = "PG", size = 12, col = "#bf812d") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none", 
        plot.margin = margin(1, 1, 0.5, 0.5, unit = "cm"))

iso_niche_opt2

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


# Question 2: Do DNA diet trophic niches of top predators shift across environmental contexts?
# the difference in number of dots here is kinda tripping me up - is there any way to filter the 
# isotope data to only include the ones with DNA data to match? I think the most recent draft that
# I have doesn't include the isotope methods so I'm sorry if this is totally unreasonable!!

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

# just another option - I think it's worth seeing the distribution of data somehow, whether it's
# a boxplot with jittered points on top, or a violin?

niche_box_opt2 <- ggplot(all_niche, aes(x = Method, y = ShapeArea, fill = Habitat)) +
  geom_violin() +
  labs(y = "95% isotopic niche area", 
       x = "Isotopic niche method",
       fill = "Habitat") +
  scale_fill_manual(values = c("#bf812d",
                               "#80cdc1")) +
  theme_bw()

niche_box_opt2

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

# Very hacky solution...

# create a new data frame
df_prey <- islet_prey %>% 
  pivot_wider(names_from = Habitat, values_from = Frequency) %>% 
  # shades of blue for shared orders
  mutate(color_col = case_when(
    Order == "Araneae" ~ "#88cfe3",
    Order == "Blattodea" ~ "#43c2e6",
    Order == "Dermaptera" ~ "#0facd9",
    Order == "Diptera" ~ "#468294",
    Order == "Orthoptera" ~ "#0d5f78",
    # use grey for the orders that are not shared
    TRUE ~ "#8e9191"
  )) %>% 
  pivot_longer(CN:PG, names_to = "Habitat", values_to = "Frequency") %>% 
  ungroup() %>% 
  fill(Order)

levels_prey <- c(
  # five shared species in alphabetical order
  "Araneae", "Blattodea", "Dermaptera", "Diptera", "Orthoptera",
  # unique species ordered by frequency of occurrence
  # > 1
  "Hemiptera", "Lepidoptera", 
  # 1
  "Geophilomorpha", "Hymenoptera","Odonata", 
  "Psocoptera", "Sarcoptiformes", "Scorpiones"
  )

# CN data frame
cn_prey <- df_prey %>% 
  filter(Habitat == "CN") %>% 
  mutate(Order = fct_relevel(Order, levels_prey)) %>% 
  arrange(Order) 

cn_prey_col <- pull(cn_prey, color_col)

cn_prey_plot <- ggplot(cn_prey, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = cn_prey_col) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 16)
        ) +
  facet_wrap(~Habitat, strip.position = "bottom")

pg_prey <- df_prey %>% 
  filter(Habitat == "PG") %>% 
  mutate(Order = fct_relevel(Order, levels_prey)) %>% 
  arrange(Order)

pg_prey_col <- pull(pg_prey, color_col)

pg_prey_plot <- ggplot(pg_prey, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = pg_prey_col, drop = FALSE) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 16)
        ) +
  facet_wrap(~Habitat, strip.position = "bottom")

library(patchwork) # putting things together

together <- cn_prey_plot + pg_prey_plot

together

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

# again, super hacky solution...

# create a new data frame
df_int <- habitat_int %>% 
  pivot_wider(names_from = Habitat, values_from = Frequency) %>% 
  # shades of blue for shared orders
  mutate(color_col = case_when(
    # yellowish browns for shared orders
    Order == "Araneae" ~ "#deb962",
    Order == "Diptera" ~ "#d4bf8a",
    Order == "Hymenoptera" ~ "#bf8c11",
    Order == "Lepidoptera" ~ "#967f45",
    Order == "Orthoptera" ~ "#876d2a", 
    Order == "Psocoptera" ~ "#6b5210", 
    # use grey for the orders that are not shared
    TRUE ~ "#8e9191"
  )) %>% 
  pivot_longer(CN:PG, names_to = "Habitat", values_to = "Frequency") %>% 
  ungroup() %>% 
  fill(Order)

levels_pred <- c(
  # six shared species in alphabetical order
  "Araneae", "Diptera", "Hymenoptera", 
  "Lepidoptera", "Orthoptera", "Psocoptera",
  
  # unique species
  "Blattodea", "Coleoptera", "Thysanoptera"
)

# CN data frame
cn_int <- df_int %>% 
  filter(Habitat == "CN") %>% 
  arrange(-Frequency) %>% 
  mutate(Order = fct_relevel(Order, levels_pred)) %>% 
  arrange(Order) 

cn_int_col <- pull(cn_int, color_col)

cn_int_plot <- ggplot(cn_int, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = cn_int_col, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 16)
  ) +
  facet_wrap(~Habitat, strip.position = "bottom")

pg_int <- df_int %>% 
  filter(Habitat == "PG") %>% 
  mutate(Order = fct_relevel(Order, levels_pred)) %>% 
  arrange(Order)

pg_int_col <- pull(pg_int, color_col)

pg_int_plot <- ggplot(pg_int, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = pg_int_col, drop = FALSE) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 16)
  ) +
  facet_wrap(~Habitat, strip.position = "bottom")

together <- cn_int_plot + pg_int_plot

together


# Figure2: Put it together and output -------------------------------------


fig2 <- int_comm + int_id
fig2
ggsave(plot = fig2,
       filename = 'int_niche.png',
       path = here("pictures", "R"),
       width = 7, height = 4,
       units = "in")
