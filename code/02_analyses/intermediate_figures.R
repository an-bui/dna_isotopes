#Fig1 - A isotope niche, b DNA niche, 
##### c. niche size boxplot, D. diet proportion
fig1 <- (iso_niche + top_comm)/(niche_box + top_id)
fig1

ggsave(plot = fig1,
       filename = 'top_niche.png',
       path = here("pictures", "R"),
       width = 7, height = 7,
       units = "in")


# Fig2: A. DNA niche, B. diet proportion
fig2 <- int_comm + int_id
fig2
ggsave(plot = fig2,
       filename = 'int_niche.png',
       path = here("pictures", "R"),
       width = 7, height = 4,
       units = "in")
