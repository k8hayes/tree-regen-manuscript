# unburned graphs
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)
here :: here()

tree <- read.csv(here("tree regen manuscript", "tree.csv"), stringsAsFactors = F)
sapling <- read.csv(here("tree regen manuscript", "sapling.csv"), stringsAsFactors = F)
both <- rbind(tree, sapling) ; rm(tree, sapling)
both$TREAT <- as.factor(both$TREAT)

# dunn's test
# basal area
# conifers
both %>%
  filter(TREAT == 0) %>%
  group_by(SITE) %>%
  shapiro_test(CONIF_BASAL_HA)
  # dunn's test
    both %>%
      filter(TREAT == 0)  %>%
      dunn_test(CONIF_BASAL_HA ~ SITE, p.adjust.method = "holm")
    
# conifers
  both %>%
      filter(TREAT == 0) %>%
      group_by(SITE) %>%
      shapiro_test(CONIF_COUNT_HA)
    # dunn's test
    both %>%
      filter(TREAT == 0)  %>%
      dunn_test(CONIF_COUNT_HA ~ SITE, p.adjust.method = "holm")

    # density plot
    conif_dens <- both %>%
      filter(TREAT ==0) %>%
      ggplot( aes(x = as.factor(SITE), y = CONIF_COUNT_HA)) + geom_boxplot() +
      labs(x = " ",y = "Conifer Density", title = "Unburned Density (stem/ha)") +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    # DECID density
    decid_dens <- both %>%
      filter(TREAT ==0) %>%
      ggplot(aes(x = as.factor(SITE), y = DECID_COUNT_HA)) + geom_boxplot() + 
      labs(x = "",y = "Decid. Density", title = "") + ylim(0,350) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    density <- plot_grid(conif_dens, decid_dens, nrow = 2)
    density
    rm(conif_dens, decid_dens)
  
# basal area
  ba <- read.csv(here("tree regen manuscript/ba.csv"), stringsAsFactors = F)
  ba <- ba[ba$TREAT == 0,]
  # CONIF basal area  
    con_ba <- ba[ba$DIV == "c",]  
    conif_ba <-  con_ba %>%
      ggplot(aes(x = as.factor(SITE), y = BA_DIV)) + geom_boxplot() + 
      labs(x = " ",y = "Conifer Basal Area", title = "Unburned Basal Area (m/ha)")   +
      theme(plot.title = element_text(hjust = 0)) + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    # DECID basal area
    dec_ba <- ba[ba$DIV == "d",]
    decid_ba <- dec_ba %>%
      ggplot(aes(x = as.factor(SITE), y = BA_DIV)) + geom_boxplot() + 
      labs(x = "",y = "Decid. Basal Area", title = "")  + 
      scale_x_discrete(labels = c("Upland", "Lowland"))
    ba <- plot_grid(conif_ba, decid_ba, nrow = 2)
    rm(conif_ba, decid_ba)
    ba
    # save_plot("ba_boxplot.png", ba)

    plot <- plot_grid(ba, density)
    save_plot("unburned.png", plot)
    