# proportion dataset
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)
se <- function(x) sqrt(var(x)/length(x))

# code for figure 6 
regen <- read.csv(here("data/regen.csv"))        
  regen <- regen[regen$SPP != "POBA",]
  regen <- regen[regen$SPP != "PIGL",]
  regen <- regen[regen$SPP != "ARCTO",]
  regen <- regen[regen$SPP != "ALCR",]

up_plot <- regen %>%
  filter(SITE == "Upland")  %>%
  filter(SPP_PROP > 0 ) %>%
  ggplot(aes(x = as.factor(TREAT), y = SPP_PROP, 
             fill = SPP)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Proportion present per plot", title = "Upland Site") +
  theme(legend.position = "none") + ylim(0,1) + 
  scale_fill_manual(values = c( "#fdae61", "#d7191c", "#92c5de", "#0571b0"),
                    name = "Species", labels = c( "Birch", "Black Spruce", "Aspen", "Willow")) 

low_plot <- regen %>%
  filter(SITE == "Lowland") %>%
  filter(SPP_PROP > 0 ) %>%
  ggplot(aes(x = as.factor(TREAT), y = SPP_PROP, 
             fill = SPP)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Proportion present per plot", title = "Lowland Site") +
  scale_fill_manual(values = c( "#fdae61", "#d7191c", "#92c5de", "#0571b0"),
                    name = "Species", labels = c( "Birch", "Black Spruce", "Aspen", "Willow")) 

up_low <- plot_grid(up_plot, low_plot, nrow = 1, ncol = 2,
                    rel_widths = c(0.9,1.3), rel_heights = c(1, 1.5),
                    labels = c("A.", "B."))
up_low
 save_plot("Fig6.pdf", up_low, nrow = 1, ncol = 2)


# data for table s4 - proportion of regeneraation
  regen <- read.csv(here("data/regen.csv"))
  
  MeanRegen <- regen %>% 
    group_by(regen$TREAT, regen$SITE, regen$SPP) %>% 
    summarise(MEAP_PROP = mean(SPP_PROP), SE = se(SPP_PROP))  %>%
    complete(regen$SPP, fill = list(MEAN_PROP = 0, SD = 0))
  MeanRegen <- as.data.frame(MeanRegen)
  colnames(MeanRegen) <- c( "TREAT", "SITE", "SPP", "MEAN_PROP", "SE")
  
  MeanRegen$SE <- round(MeanRegen$SE, digits = 3)
  MeanRegen$MEAN_PROP <- round(MeanRegen$MEAN_PROP, digits = 3)
  
  #regen <- regen[regen$SPP != "POBA",]
  #regen <- regen[regen$SPP != "PIGL",]
  #regen <- regen[regen$SPP != "ARCTO",]
  #regen <- regen[regen$SPP != "ALCR",]
  
  MeanRegen$SE <- MeanRegen$SE * 100
  MeanRegen$MEAN_PROP <- MeanRegen$MEAN_PROP * 100
  # removing values of zero for clarity of reading
    test <- MeanRegen %>%
      filter(MEAN_PROP >0 )

# code used for line graph
 regeggplot(MeanRegen, aes(x = as.factor(TREAT), y = MEAN_PROP,
                                     color = SPP, group = SPP)) + facet_wrap(~SITE) +
    geom_line(size = 0.75) + geom_point(aes(shape = SPP))  +
    labs(x = "Number of Fires", y = "Mean proportion of species", title = "Proportion of Regeneration by Species") +
    scale_shape_manual(values = c(16,17,18,19),
                       name = "Species", labels = c( "Birch", "Black Spruce", "Aspen", "Willow")) +
    scale_color_manual(values = c( "#08306b", "#d7191c", "#9ecae1", "#0571b0"),
                       name = "Species", labels = c( "Birch", "Black Spruce", "Aspen", "Willow"))  +
    geom_errorbar(aes(ymin = MEAN_PROP - SE,
                      ymax = MEAN_PROP + SE),
                  width = .65, position = position_dodge(0.15))
  regenPlot
  # save_plot("regenPlot.png", regenPlot, base_aspect_ratio = 1.5 )

