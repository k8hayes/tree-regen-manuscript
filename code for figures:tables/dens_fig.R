# density figure
# fig 
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

species_dens <- read.csv(here("species_dens.csv"), stringsAsFactors = F)

species_dens$SITE[species_dens$SITE == "DALTON"] <- "Upland"
species_dens$SITE[species_dens$SITE == "STEESE"] <- "Lowland"

species_dens %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = COUNT_HA, fill = DIV)) + 
  geom_boxplot() + facet_wrap(~SITE)  + ylim(0, 200000) + 
  labs(x = "Number of Fires", y = "Stems per Hectare", 
       title = "Density of Regeneration in Burned Plots") + 
  scale_fill_manual(values = c("#f0f0f0", "#bdbdbd"),
                    name = "Division",
                    labels = c("Conifer", "Deciduous"))
# export manually with 650 x 350, no aspect ratio
# name = dens_fig.png

willow_plot <- species_dens %>%
  filter(TREAT != 0) %>%
  filter(SPP == "SALIX") %>%
  ggplot(aes(x = as.factor(TREAT), y = COUNT_HA, fill = SITE)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#a1d99b", "#31a354"),
                    name = "Division",
                    labels = c("Upland", "Lowland")) + 
  labs(x = "Number of Fires", y = "Density (Stems/ha)",
       title = "Willow Regeneration")

spruce_plot <- species_dens %>%
  filter(TREAT != 0) %>%
  filter(SPP == "PIME") %>%
  ggplot(aes(x = as.factor(TREAT), y = COUNT_HA, fill = SITE)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#feb24c", "#f03b20"),
                                     name = "Division",
                                     labels = c("Upland", "Lowland")) +
  labs(x = " ", y = "Density (Stems/ha)",
       title = "Black Spruce Regeneration")

birch_plot <- species_dens %>%
  filter(TREAT != 0) %>%
  filter(SPP == "BENE") %>%
  ggplot(aes(x = as.factor(TREAT), y = COUNT_HA, fill = SITE)) +
  geom_boxplot() + ylim(0, 25000) + 
  scale_fill_manual(values = c("#9ecae1", "#3182bd"),
                    name = "Division",
                    labels = c("Upland", "Lowland")) + 
  labs(x = " ", y = "Density (Stems/ha)",
       title = "Birch Regeneration")
plot_grid(spruce_plot, birch_plot, willow_plot, nrow = 3, ncol = 1,
          labels = c("A.", "B.", "C."))
# export

