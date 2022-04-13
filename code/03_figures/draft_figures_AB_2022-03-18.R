
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

# store colors
cn_col <- "#bf812d"
pg_col <- "#80cdc1"

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
  scale_color_manual(values = c("CN" = cn_col, "PG" = pg_col)) +
  stat_ellipse(size = 1) +
  # in plot annotations of CN and PG
  annotate("text", x = -26, y = 7.2, label = "Pisonia", size = 11, col = pg_col) +
  annotate("text", x = -20.1, y = 3.4, label = "Cocos", size = 11, col = cn_col) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none", 
        plot.margin = margin(1, 1, 0.5, 0.5, unit = "cm"))

iso_niche_opt2

iso_niche_opt3 <- spider_iso %>%
  mutate(Island = factor(Island, 
                         levels = c("Castor", "Fern",
                                    "Holei","Kaula", "Paradise", 
                                    "Dudley", "Eastern",
                                    "Leslie", "Lost", "Sand"))) %>%
  ggplot(aes(x = d13C, y = d15N_c, color = Island)) +
  geom_point() +
  labs(x = expression({delta}^13*C~ ('\u2030')), 
       y = expression({delta}^15*N~ ('\u2030'))) +
  scale_color_manual(values = c("#2D1A03", # Castor
                                "#543005", # Fern
                                "#8c510a", # Holei
                                "#bf812d", # Kaula
                                "#dfc27d", # Paradise
                                "#c7eae5", # Dudley
                                "#80cdc1", # Eastern
                                "#35978f", # Leslie
                                "#01665e", # Lost
                                "#003c30" # Sand
                                )) +
  stat_ellipse(aes(linetype = Habitat)) +
  scale_linetype_manual(values = c(1, 2), guide = "none") +
  # in plot annotations of CN and PG
  annotate("text", x = -26.7, y = 7.4, label = "Pisonia", size = 10.5, col = pg_col) +
  annotate("text", x = -20.05, y = 3.4, label = "Cocos", size = 10.5, col = cn_col) +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 23),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25)
  ) 

iso_niche_opt3

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

top_comm_opt2 <- xt %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(DNA_meta, by = "Extraction.ID") %>%
  ggplot(aes(x = CAP1, y = MDS1, color = Habitat)) +
  geom_point(size = 2) +
  scale_fill_manual(values = c(cn_col, pg_col)) +
  scale_color_manual(values = c(cn_col, pg_col)) +
  stat_ellipse(size = 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 23),
        legend.position = "none"
  ) +
  # in plot annotations of CN and PG
  annotate("text", x = -4, y = 1.7, label = "Cocos", size = 11, col = cn_col) +
  annotate("text", x = 2.8, y = -1.8, label = "Pisonia", size = 11, col = pg_col) 

top_comm_opt2

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

niche_box_opt2 <- all_niche %>% 
  mutate(Habitat = recode(Habitat, CN = "Cocos", PG = "Pisonia")) %>% 
  ggplot(aes(x = Method, y = ShapeArea, fill = Habitat)) +
  geom_violin() +
  labs(y = "95% isotopic niche area", 
       x = "Isotopic niche method",
       fill = "Habitat") +
  scale_fill_manual(values = c(cn_col,
                               pg_col)) +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 23),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25)
  ) 

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

# old colors
# Order == "Araneae" ~ "#88cfe3",
# Order == "Blattodea" ~ "#43c2e6",
# Order == "Dermaptera" ~ "#0facd9",
# Order == "Diptera" ~ "#468294",
# Order == "Orthoptera" ~ "#0d5f78"

# create a new data frame
df_prey <- islet_prey %>% 
  pivot_wider(names_from = Habitat, values_from = Frequency) %>% 
  # shades of blue for shared orders
  mutate(color_col = case_when(
    Order == "Araneae" ~ "#A1CAF6",
    Order == "Blattodea" ~ "#6592D6",
    Order == "Dermaptera" ~ "#4C6FA1",
    Order == "Diptera" ~ "#375377",
    Order == "Orthoptera" ~ "#1E2F46",
    # use grey for the orders that are not shared
    Order == "Hemiptera" ~ "#E0E0E0",
    Order == "Lepidoptera" ~ "#CBCBCB",
    Order == "Geophilomorpha" ~ "#B7B7B7",
    Order == "Hymenoptera" ~ "#A2A2A2",
    Order == "Odonata" ~ "#8D8D8D",
    Order == "Psocoptera" ~ "#787878",
    Order == "Sarcoptiformes" ~ "#646464",
    Order == "Scorpiones" ~ "#4F4F4F"
  )) %>% 
  pivot_longer(CN:PG, names_to = "Habitat", values_to = "Frequency") %>% 
  ungroup() %>% 
  fill(Order) %>% 
  mutate(Habitat = recode(Habitat, CN = "Cocos", PG = "Pisonia"))

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
  filter(Habitat == "Cocos") %>% 
  mutate(Order = fct_relevel(Order, levels_prey)) %>% 
  arrange(Order) 

cn_prey_col <- pull(cn_prey, color_col)

cn_prey_plot <- ggplot(cn_prey, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = cn_prey_col, drop = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 23),
        axis.title.y = element_text(size = 23), 
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 25)
        ) +
  facet_wrap(~Habitat, strip.position = "top")

pg_prey <- df_prey %>% 
  filter(Habitat == "Pisonia") %>% 
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
        strip.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20)
        ) +
  facet_wrap(~Habitat, strip.position = "top")

top_id_opt2 <- cn_prey_plot + pg_prey_plot

top_id_opt2

# Fig1: Put together and save ---------------------------------------------


fig1 <- (iso_niche + top_comm)/(niche_box + top_id)
fig1

ggsave(plot = fig1,
       filename = 'top_niche.png',
       path = here("pictures", "R"),
       width = 7, height = 7,
       units = "in")

fig1_opt2 <- (iso_niche_opt3 + top_comm_opt2)/(niche_box_opt2 + top_id_opt2) +
  plot_layout(tag_level = "new") &
  plot_annotation(tag_levels = list(c("A", "B", "C", "D", ""))) &
  theme(plot.tag = element_text(size = 40))
fig1_opt2

ggsave(plot = fig1_opt2,
       filename = 'top_niche_opt2.png',
       path = here("pictures", "R"),
       width = 18, height = 14,
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

int_comm_opt2 <- xi %>%
  rownames_to_column(var = "Extraction.ID") %>%
  left_join(DNA_intmeta, by = "Extraction.ID") %>%
  ggplot(aes(x = CAP1, y = MDS1, color = Habitat)) +
  geom_point(size = 5) +
  scale_fill_manual(values = c(cn_col, pg_col)) +
  scale_color_manual(values = c(cn_col, pg_col)) +
  stat_ellipse(size = 2) +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 23),
        legend.position = "none"
  ) +
  # in plot annotations of CN and PG
  annotate("text", x = -2, y = -1.5, label = "Cocos", size = 11, col = cn_col) +
  annotate("text", x = 1, y = 2, label = "Pisonia", size = 11, col = pg_col)

int_comm_opt2


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

# old colors: 
# Order == "Araneae" ~ "#deb962",
# Order == "Diptera" ~ "#d4bf8a",
# Order == "Hymenoptera" ~ "#bf8c11",
# Order == "Lepidoptera" ~ "#967f45",
# Order == "Orthoptera" ~ "#876d2a", 
# Order == "Psocoptera" ~ "#6b5210"

# create a new data frame
df_int <- habitat_int %>% 
  pivot_wider(names_from = Habitat, values_from = Frequency) %>% 
  # shades of blue for shared orders
  mutate(color_col = case_when(
    # yellowish oranges for shared orders
    Order == "Araneae" ~ "#F2B705",
    Order == "Diptera" ~ "#F29F05",
    Order == "Hymenoptera" ~ "#F28705",
    Order == "Lepidoptera" ~ "#D95204",
    Order == "Orthoptera" ~ "#A62F03", 
    Order == "Psocoptera" ~ "#701e00", 
    # use grey for the orders that are not shared
    Order == "Blattodea" ~ "#E0E0E0",
    Order == "Coleoptera" ~ "#787878",
    Order == "Thysanoptera" ~ "#B7B7B7"
  )) %>% 
  pivot_longer(CN:PG, names_to = "Habitat", values_to = "Frequency") %>% 
  ungroup() %>% 
  fill(Order) %>% 
  mutate(Habitat = recode(Habitat, CN = "Cocos", PG = "Pisonia"))

levels_pred <- c(
  # six shared species in alphabetical order
  "Araneae", "Diptera", "Hymenoptera", 
  "Lepidoptera", "Orthoptera", "Psocoptera",
  
  # unique species
  "Blattodea", "Thysanoptera", "Coleoptera"
)

# CN data frame
cn_int <- df_int %>% 
  filter(Habitat == "Cocos") %>% 
  arrange(-Frequency) %>% 
  mutate(Order = fct_relevel(Order, levels_pred)) %>% 
  arrange(Order) 

cn_int_col <- pull(cn_int, color_col)

cn_int_plot <- ggplot(cn_int, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = cn_int_col, drop = FALSE) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 23),
        axis.title.y = element_text(size = 23), 
        panel.grid = element_line(color = "white"),
        panel.border = element_rect(color = "white"),
        strip.background = element_rect(color = "white", fill = "white"),
        strip.text = element_text(size = 25)
  ) +
  facet_wrap(~Habitat, strip.position = "top")

pg_int <- df_int %>% 
  filter(Habitat == "Pisonia") %>% 
  mutate(Order = fct_relevel(Order, levels_pred)) %>% 
  arrange(Order)

pg_int_col <- pull(pg_int, color_col)

pg_int_plot <- ggplot(pg_int, aes(x = Habitat, y = Frequency, fill = Order)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = pg_int_col, drop = FALSE) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
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
        strip.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20)
  ) +
  facet_wrap(~Habitat, strip.position = "top")

int_id_opt2 <- cn_int_plot + pg_int_plot

int_id_opt2 


# Figure2: Put it together and output -------------------------------------


fig2 <- int_comm + int_id
fig2

# ggsave(plot = fig2,
#        filename = 'int_niche.png',
#        path = here("pictures", "R"),
#        width = 7, height = 4,
#        units = "in")

fig2_opt2 <- (int_comm_opt2 + int_id_opt2) +
  plot_layout(widths = c(5, 3), tag_level = "new") &
  plot_annotation(tag_levels = list(c("A", "B", ""))) &
  theme(plot.tag = element_text(size = 40))
fig2_opt2

ggsave(plot = fig2_opt2,
       filename = 'int_niche_opt2.png',
       path = here("pictures", "R"),
       width = 16, height = 10,
       units = "in")
