# kruskall wallis density tests and graphs
# figure 5
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

# density
tree <- read.csv(here("tree regen manuscript", "tree.csv"), stringsAsFactors = F)
sapling <- read.csv(here("tree regen manuscript", "sapling.csv"), stringsAsFactors = F)
both <- rbind(tree, sapling); rm(tree,sapling)
both$TREAT <- as.factor(both$TREAT)

# CONIFER DENSITY
  both %>%
    filter(TREAT != 0) %>%
    group_by(SITE) %>%
    shapiro_test(CONIF_COUNT_HA) # not normally distributed
  # kruskal-wallis with Dunn's test
  # DALTON
    both %>%
      filter(TREAT !=0) %>%
      filter(SITE == "DALTON") %>%
      dunn_test(CONIF_COUNT_HA ~ TREAT, p.adjust.method = "holm")
  # STEESE
    both %>%
      filter(TREAT !=0 ) %>%
      filter(SITE == "STEESE") %>%
      dunn_test(CONIF_COUNT_HA ~ TREAT, p.adjust.method = "holm")

# DECIDUOUS density
    both %>%
      filter(TREAT !=0) %>%
      group_by(SITE) %>%
      shapiro_test(DECID_COUNT_HA) 
    # DALTON
    both %>%
      filter(TREAT !=0) %>%
      filter(SITE == "DALTON") %>%
      dunn_test(DECID_COUNT_HA ~ TREAT, p.adjust.method = "holm")
    # STEESE
    both %>%
      filter(TREAT !=0) %>%
      filter(SITE == "STEESE") %>%
      dunn_test(DECID_COUNT_HA ~ TREAT, p.adjust.method = "holm")
    
# DENSITY BOXPLOTS - FIGURE 5
    # CONIFER density
    # dalton
    dalt_conif_dens <- both %>%
      filter(TREAT !=0) %>%
      filter(SITE == "DALTON") %>%
      ggplot( aes(x = as.factor(TREAT), y = CONIF_COUNT_HA)) + geom_boxplot() +
      labs(x = " ",y = "Conifer Density", title = "Upland Plots") +
      theme(plot.title = element_text(hjust = 0)) + ylim(0,30)
    # steese
    stee_conif_dens <- both %>%
      filter(TREAT !=0) %>%
      filter(SITE == "STEESE") %>%
      ggplot(aes(x = as.factor(TREAT), y = CONIF_COUNT_HA)) + geom_boxplot() + 
      labs(x = "",y = "", title = "Lowland Plots") + 
      theme(plot.title = element_text(hjust = 0)) + ylim(0,30)
    conif_dens <- plot_grid(dalt_conif_dens,stee_conif_dens)
    rm(dalt_conif_dens, stee_conif_dens)
    # DECID density
    # dalton
    dalt_decid_dens <- both %>%
      filter(TREAT !=0) %>%
      filter(SITE == "DALTON") %>%
      ggplot(aes(x = as.factor(TREAT), y = DECID_COUNT_HA)) + geom_boxplot() + 
      labs(x = "Number of Fires",y = "Decid. Density", title = "") + ylim(0,3750)
    # steese
    stee_decid_dens <- both%>%
      filter(TREAT !=0) %>%
      filter(SITE == "STEESE") %>%
      ggplot(aes(x = as.factor(TREAT), y = DECID_COUNT_HA)) + geom_boxplot() + 
      labs(x = "Number of Fires",y = "", title = "") + ylim(0,3750)
    decid_dens <- plot_grid(dalt_decid_dens, stee_decid_dens)
    rm(dalt_decid_dens, stee_decid_dens)
    density <- plot_grid(conif_dens, decid_dens, nrow = 2)
    rm(conif_dens, decid_dens)
    save_plot("dens_boxplot.png", density)
