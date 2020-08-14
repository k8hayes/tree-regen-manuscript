# basal area figure
# fig 
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

species_ba <- read.csv(here("species_ba.csv"), stringsAsFactors = F)

species_ba$SITE[species_ba$SITE == "DALTON"] <- "Upland"
species_ba$SITE[species_ba$SITE == "STEESE"] <- "Lowland"

species_ba %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = BA_PLOT, fill = DIV)) + 
  geom_boxplot() + facet_wrap(~SITE) + ylim(0,6) +
  labs(x = "Number of Fires", y = "Basal Area (m2/ha)", 
       title = "Total Basal Area in Burned Plots") + 
  scale_fill_manual(values = c("#f0f0f0", "#bdbdbd"),
                     name = "Division",
                     labels = c("Conifer", "Deciduous"))
# export manually with 650 x 350, no aspect ratio
# name = ba_fig.png


