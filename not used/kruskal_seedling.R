# kruskal-wallis tests for seedling density
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
theme_set(theme_cowplot())

setwd("Google Drive/UC Denver/Project/R/")
all <- read.csv("seedling.csv", stringsAsFactors = F)
all$TREAT <- as.factor(all$TREAT)
all %>%
  group_by(SITE) %>%
  shapiro_test(CONIF_COUNT_HA)

# total stem density
all$TOTAL_COUNT_HA <- all$DECID_COUNT_HA + all$CONIF_COUNT_HA
  # dalton
    all <- all[all$SITE == "Upland",]
    all %>%
       dunn_test(TOTAL_COUNT_HA ~ TREAT, p.adjust.method = "bonferroni")
  # steese
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all$TOTAL_COUNT_HA <- all$DECID_COUNT_HA + all$CONIF_COUNT_HA
    all <- all[all$SITE == "Lowland",]
    all %>%
      dunn_test(TOTAL_COUNT_HA ~ TREAT, p.adjust.method = "bonferroni")

# conifer density
  # DALTON
    all <- all[all$SITE == "Upland",]
    all %>%
      dunn_test(CONIF_COUNT_HA ~ TREAT, p.adjust.method = "bonferroni")
  # STEESE
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all <- all[all$SITE == "Lowland",]
    all %>%
      dunn_test(CONIF_COUNT_HA ~ TREAT, p.adjust.method = "bonferroni")

# decid density
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all %>%
      group_by(SITE) %>%
      shapiro_test(DECID_COUNT_HA)
    # DALTON
    all <- all[all$SITE == "Upland",]
    all %>%
      dunn_test(DECID_COUNT_HA ~ TREAT, p.adjust.method = "bonferroni")
    # STEESE
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all <- all[all$SITE == "Lowland",]
    all %>%
      dunn_test(DECID_COUNT_HA ~ TREAT, p.adjust.method = "bonferroni")

# boxplots  
    # dalton
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all <- all[all$SITE == "Upland",]
    dalt_conif_dens <- ggplot(all, aes(x = TREAT, y = CONIF_COUNT_HA)) + geom_boxplot() + 
      labs(x = " ",y = "Conifer Density", title = "Upland Plots") + 
      theme(plot.title = element_text(hjust = 0.5))
    # steese
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all <- all[all$SITE == "Lowland",]
    stee_conif_dens <- ggplot(all, aes(x = TREAT, y = CONIF_COUNT_HA)) + geom_boxplot() + 
      labs(x = "",y = "", title = "Lowland Plots") + 
      theme(plot.title = element_text(hjust = 0.5))
    conif_dens <- plot_grid(dalt_conif_dens,stee_conif_dens)
    # DECID density
    # dalton
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all <- all[all$SITE == "Upland",]
    dalt_decid_dens <- ggplot(all, aes(x = TREAT, y = DECID_COUNT_HA)) + geom_boxplot() + 
      labs(x = "Number of Fires",y = "Decid. Density", title = "")
    # steese
    all <- read.csv("seedling.csv", stringsAsFactors = F)
    all$TREAT <- as.factor(all$TREAT)
    all <- all[all$SITE == "Lowland",]
    stee_decid_dens <- ggplot(all, aes(x = TREAT, y = DECID_COUNT_HA)) + geom_boxplot() + 
      labs(x = "Number of Fires",y = "", title = "")
    decid_dens <- plot_grid(dalt_decid_dens, stee_decid_dens)
    density <- plot_grid(conif_dens, decid_dens, nrow = 2)
    save_plot("dens_boxplot_Seed.png", density)
