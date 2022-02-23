
source(here("code", "source_DNA.R"))
source(here("code", "source_ASV_DNA.R"))

freq <- DNA_iso %>%
  group_by(Order) %>%
  summarise(frequency = n()) %>%
  add_row(frequency = 34, .before = 1) %>%
  ungroup() %>%
  dplyr::select(frequency) %>%
  as_vector()

library(iNEXT)

out <- iNEXT(freq, q = 0, datatype = "incidence_freq")

out$AsyEst

ggiNEXT(out, type=1, color.var="site") + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25)) +
  scale_color_manual(values = c("#AF6E78")) +
  scale_fill_manual(values = c("#AF6E78")) +
  geom_ribbon(aes(ymin=28-13, ymax=28+13), color = NA, alpha=0.1) +
  geom_hline(yintercept = 28, linetype = "dashed", size =1) +
  labs(y = "Order Richness")

freq2 <- ASV_iso %>%
  group_by(ASV) %>%
  summarise(frequency = n()) %>%
  add_row(frequency = 34, .before = 1) %>%
  ungroup() %>%
  dplyr::select(frequency) %>%
  as_vector()

out2 <- iNEXT(freq2, q = 0, datatype = "incidence_freq")

out2$AsyEst

ggiNEXT(out2, type=1, color.var="site") + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25)) +
  scale_color_manual(values = c("#AF6E78")) +
  scale_fill_manual(values = c("#AF6E78")) +
  geom_ribbon(aes(ymin=142-46, ymax=142+46), color = NA, alpha=0.1) +
  geom_hline(yintercept = 142, linetype = "dashed", size =1) +
  labs(y = "ASV Richness")

