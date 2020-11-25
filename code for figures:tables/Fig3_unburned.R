# Figure 3
# unburned density and basal area
# conifer and deciduous
# both plots 

library(tidyverse)
library(ggplot2)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

ba <- read.csv(here("data/ba.csv"))
dens <- read.csv(here("data/density.csv"))

plot_dens <- dens %>%
  group_by(SITE, TREAT, PLOT, DIV) %>%
  summarise(COUNT_HA = mean(COUNT_HA))

    # density plot
    conif_dens <- plot_dens %>%
      filter(TREAT == 0) %>%
      filter(DIV == "c") %>%
      ggplot(aes(x = as.factor(SITE), y = COUNT_HA)) + geom_boxplot() +
      labs(x = " ",y = "Density (stem/ha)", title = "Conifer Density") +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland")) + 
      theme(plot.title = element_text(size = 14))
    
    # DECID density
    decid_dens <- plot_dens %>%
      filter(TREAT == 0) %>%
      filter(DIV == "d") %>%
      ggplot(aes(x = as.factor(SITE), y = COUNT_HA)) + geom_boxplot() + 
      labs(x = "",y = "Density (stem/ha)", title = "Deciduous Density")  +
      scale_x_discrete(labels = c("Upland", "Lowland")) + 
      theme(plot.title = element_text(size = 14))
    density <- cowplot::plot_grid(conif_dens, decid_dens)
    density
    # rm(conif_dens, decid_dens)
  
# basal area
    plot_ba <- ba %>%
      group_by(SITE, TREAT, PLOT, DIV) %>%
      summarise(BA_ha = mean(BA_ha))
    
  # CONIF basal area  
    conif_ba <- plot_ba %>%
      filter(TREAT == 0) %>%
      filter(DIV == "c") %>%
      ggplot(aes(x = as.factor(SITE), y = BA_ha)) + geom_boxplot() + 
      labs(x = " ",y = "Basal Area (m2/ha)", title = "Conifer Basal Area")   +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland")) + 
      theme(plot.title = element_text(size = 14))
    # DECID basal area
    decid_ba <-plot_ba %>%
      filter(TREAT == 0) %>%
      filter(DIV == "d") %>%
      ggplot(aes(x = as.factor(SITE), y = BA_ha)) + geom_boxplot() + 
      labs(x = "",y = "Basal Area (m2/ha)", title = "Deciduous Basal Area")   +
      scale_x_discrete(labels = c("Upland", "Lowland")) + 
      theme(plot.title = element_text(size = 14))
    ba <- cowplot:: plot_grid(conif_ba, decid_ba, ncol = 2)
    # rm(conif_ba, decid_ba)
    ba
    # save_plot("ba_boxplot.png", ba)
    
    lrow <- cowplot:: plot_grid(conif_dens, conif_ba, align = "v", axis = 'l', ncol = 1)
    rrow <- cowplot:: plot_grid(decid_dens, decid_ba, align = "v", axis = "l", ncol = 1)
    
   fig3 <- cowplot:: plot_grid(lrow, rrow, ncol = 2)
   
   fig3
   # title <- ggdraw() +
   #   draw_label(
   #     "Unburned Stand Structure between an Upland and Lowland Site",
   #     fontface = 'bold',
   #     x = 0, size = 17,
   #     hjust = 0) +
   #   theme(
   #     # add margin on the left of the drawing canvas,
   #     # so title is aligned with left edge of first plot
   #     plot.margin = margin(0, 0, 0, 20))
   # 
   # fig3_title <- plot_grid(title, fig3,
   #   ncol = 1,
   #   rel_heights = c(0.1, 1))
   # 
   # fig3_title
   
   # for earlier drafts:
    # export as "unburned.png" # 650 x 450, no aspect ratio
  
    # for ecosphere:
    # export as "unburned.pdf # 6 x 8 inc 
        # since ecosphere req max 15.6 cm by 20 cm
    