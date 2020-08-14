# kruskal-wallis tests for tree and seedling density and basal area
# also Figure X 
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# so many tidyverse links...
   
# basal area
  # CONIFER BASAL AREA
      ba <- read.csv(here("tree regen manuscript/ba.csv"))
      ba$TREAT <- as.factor(ba$TREAT)
      ba %>%
        filter(TREAT != 0) %>%
        group_by(SITE) %>%
        shapiro_test(CONIF_BA) # not normally distributed
    # kruskal-wallis with Dunn's test
    # DALTON
      ba %>%
        filter(TREAT !=0) %>%
        filter(SITE == "DALTON") %>%
        dunn_test(CONIF_BA ~ TREAT, p.adjust.method = "holm")
    # STEESE
      ba %>%
        filter(TREAT !=0 ) %>%
        filter(SITE == "STEESE") %>%
        dunn_test(CONIF_BA ~ TREAT, p.adjust.method = "holm")
      
  # DECIDUOUS BASAL AREA
      ba %>%
        filter(TREAT !=0)  %>%
        group_by(SITE) %>%
        shapiro_test(DECID_BA)
      # DALTON
      ba %>%
        filter(TREAT !=0) %>%
        filter(SITE == "DALTON") %>%
        dunn_test(DECID_BA ~ TREAT, p.adjust.method = "holm")
      # STEESE
      ba %>%
        filter(TREAT !=0) %>%
        filter(SITE == "STEESE") %>%
        dunn_test(DECID_BA ~ TREAT, p.adjust.method = "holm")

# BOXPLOTS - FIGURE 7      
  ba <- read.csv(here("ba.csv"))
  ba$TREAT <- as.factor(ba$TREAT)
  # CONIFER basal area
    # dalton
    dalt_conif_ba <- ba %>%
      filter(TREAT !=0) %>%
      filter(SITE == "DALTON") %>%
      ggplot(aes(x = as.factor(TREAT), y = CONIF_BA)) + geom_boxplot() + 
      labs(x = " ",y = "Conifer BA", title = "Upland Plots")  + ylim(0,0.03) + 
      theme(plot.title = element_text(hjust = 0)) 
    # steese
    stee_conif_ba <- ba %>%
      filter(TREAT !=0) %>%
      filter(SITE == "STEESE") %>%
      ggplot(aes(x = as.factor(TREAT), y = CONIF_BA)) + geom_boxplot() + 
      labs(x = "",y = "", title = "Lowland Plots")  + ylim(0,0.03) + 
      theme(plot.title = element_text(hjust = 0))
    conif_ba <- plot_grid(dalt_conif_ba,stee_conif_ba)
    conif_ba
    rm(dalt_conif_ba, stee_conif_ba)
# DECID basal area
    # dalton
    dalt_decid_ba <- ba %>%
      filter(TREAT !=0) %>%
      filter(SITE == "DALTON") %>%
      ggplot(aes(x = as.factor(TREAT), y = DECID_BA)) + geom_boxplot() + 
      labs(x = "Number of Fires",y = "Decid. BA", title = "") + ylim(0,7.5)
    # steese
    stee_decid_ba <- ba %>%
      filter(TREAT !=0) %>%
      filter(SITE == "STEESE") %>%
      ggplot(aes(x = as.factor(TREAT), y = DECID_BA)) + geom_boxplot() + 
      labs(x = "Number of Fires",y = "", title = "") + ylim(0,7.5)
    decid_ba <- plot_grid(dalt_decid_ba, stee_decid_ba)
    rm(dalt_decid_ba, stee_decid_ba)
    ba_plot <- plot_grid(conif_ba, decid_ba, nrow = 2, align = "v")
    rm(conif_ba, decid_ba)
    ba_plot
    save_plot("ba_boxplot.png", ba_plot) # watch out, right now saves into R folder, not tree regen manuscript



# graph for jannike
    ggplot(both, aes(x = TREAT, y = COUNT_HA, fill = DIVISION)) +
      geom_boxplot() + facet_wrap(vars(SITE)) + ylim(c(0, 5000)) + 
      labs(x = "Number of Fires", y = "Stem count per Ha") + 
      scale_fill_manual(name = "Division",
                        labels = c("Conifer", "Deciduous"),
                        values = c("#737373", "#ffffff")) + 
      panel_border() + background_grid()
    