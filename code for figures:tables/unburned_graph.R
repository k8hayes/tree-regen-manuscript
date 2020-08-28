# unburned graphs
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

ba <- read.csv(here("ba.csv"))
dens <- read.csv(here("species_dens.csv"))

    # density plot
    conif_dens <- dens %>%
      filter(TREAT ==0) %>%
      filter(DIV == "c") %>%
      ggplot(aes(x = as.factor(SITE), y = COUNT_HA)) + geom_boxplot() +
      labs(x = " ",y = "Conifer Density", title = "Unburned Density (stem/ha)") +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    
    # DECID density
    decid_dens <- dens %>%
      filter(TREAT ==0) %>%
      filter(DIV == "d") %>%
      ggplot(aes(x = as.factor(SITE), y = COUNT_HA)) + geom_boxplot() + 
      labs(x = "",y = "Decid. Density", title = "")  + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    density <- plot_grid(conif_dens, decid_dens, nrow = 2)
    density
    rm(conif_dens, decid_dens)
  
# basal area
  # CONIF basal area  
    conif_ba <- ba %>%
      filter(TREAT == 0) %>%
      filter(DIV == "c") %>%
      ggplot(aes(x = as.factor(SITE), y = BA_ha)) + geom_boxplot() + 
      labs(x = " ",y = "Conifer Basal Area", title = "Unburned Basal Area (m2/ha)")   +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    # DECID basal area
    decid_ba <- ba %>%
      filter(TREAT == 0) %>%
      filter(DIV == "d") %>%
      ggplot(aes(x = as.factor(SITE), y = BA_ha)) + geom_boxplot() + 
      labs(x = "",y = "Decid. Basal Area", title = "")  + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    ba <- plot_grid(conif_ba, decid_ba, nrow = 2)
    rm(conif_ba, decid_ba)
    ba
    # save_plot("ba_boxplot.png", ba)

    plot <- plot_grid(ba, density)
    plot
    save_plot("unburned.png", plot)
    