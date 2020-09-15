# unburned graphs
library(tidyverse)
library(ggplot2)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

ba <- read.csv(here("ba.csv"))
dens <- read.csv(here("data/density.csv"))

plot_dens <- dens %>%
  group_by(SITE, TREAT, PLOT, DIV) %>%
  summarise(COUNT_HA = mean(COUNT_HA))

    # density plot
    conif_dens <- plot_dens %>%
      filter(TREAT == 0) %>%
      filter(DIV == "c") %>%
      ggplot(aes(x = as.factor(SITE), y = COUNT_HA)) + geom_boxplot() +
      labs(x = " ",y = "Conifer Density", title = "Unburned Density (stem/ha)") +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    
    # DECID density
    decid_dens <- plot_dens %>%
      filter(TREAT == 0) %>%
      filter(DIV == "d") %>%
      ggplot(aes(x = as.factor(SITE), y = COUNT_HA)) + geom_boxplot() + 
      labs(x = "",y = "Decid. Density", title = "")  +
      scale_x_discrete(labels = c("Upland", "Lowland"))
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
      labs(x = " ",y = "Conifer Basal Area", title = "Unburned Basal Area (m2/ha)")   +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    # DECID basal area
    decid_ba <-plot_ba %>%
      filter(TREAT == 0) %>%
      filter(DIV == "d") %>%
      ggplot(aes(x = as.factor(SITE), y = BA_ha)) + geom_boxplot() + 
      labs(x = "",y = "Decid. Basal Area", title = "")   +
      scale_x_discrete(labels = c("Upland", "Lowland"))
    ba <- cowplot:: plot_grid(conif_ba, decid_ba, ncol = 2)
    # rm(conif_ba, decid_ba)
    ba
    # save_plot("ba_boxplot.png", ba)
    
    lrow <- cowplot:: plot_grid(conif_dens, conif_ba, align = "v", axis = 'l', ncol = 1)
    rrow <- cowplot:: plot_grid(decid_dens, decid_ba, align = "v", axis = "l", ncol = 1)
    
    plot <- cowplot:: plot_grid(lrow, rrow, ncol = 2)
    plot
    # export as "unburned.png" # 650 x 450, no aspect ratio
    