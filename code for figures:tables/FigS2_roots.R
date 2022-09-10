# adventitious roots
# Fig. S2

library(ggplot2)
library(cowplot)
library(dplyr)
library(here)
theme_set(theme_cowplot())

# adventitous roots
adven_root <- read.csv(here("data/adventaguous_roots.csv"))

adven_root_plot <- ggplot(adven_root, 
                          aes(x = factor(TREAT),
                              y = DEPTH, 
                              fill = SITE)) + 
  geom_boxplot() + 
  labs(x = "Number of Fires", y = "Depth (cm)",
       title = "Adventitious Roots") + 
  scale_fill_manual(values = c("#d9d9d9", "#ffffff"), name = "Site Type", labels = c("Upland", "Lowland")) + 
  theme(plot.title = element_text(hjust = 0)) + xlim("1", "2")

adven_root_plot

save_plot("adven_root.png", adven_root_plot, base_aspect_ratio = 1.5)

# getting numbers
## adven root
upland <- adven_root[adven_root$SITE == "DALTON",]

summary(upland$DEPTH[upland$TREAT == 1])

summary(upland$DEPTH[upland$TREAT == 2])

lowland <- adven_root[adven_root$SITE == "STEESE",]

summary(lowland$DEPTH[lowland$TREAT == 1])
low1 <- lowland$DEPTH[lowland$TREAT == 1]
low2 <- lowland$DEPTH[lowland$TREAT == 2]
