# organic layer and exposed mineral soil
# Figure 7

library(ggplot2)
library(cowplot)
library(dplyr)
library(here)
theme_set(theme_cowplot())

# organic layer depth
  org_depth <- read.csv(here("data/org_depth.csv"))
  
  org_depth_plot <- ggplot(org_depth, 
                         aes(x = factor(TREATMENT), y = ORG_DEPTH, 
                             fill = SITE)) + 
    geom_boxplot()   + 
    scale_fill_manual(name = "Site Type",
                    values = c("#d9d9d9", "#ffffff"), 
                    labels = c("Upland", "Lowland")) + 
    labs(title = "Organic Layer Depth", 
       x = "Number of Fires", y = "Organic layer depth (cm)") + 
    theme(plot.title = element_text(hjust = 0), legend.position =  "none") 
  org_depth_plot
# save_plot("org_depth.png", org_depth_plot, base_aspect_ratio = 1.5)
  
# Exposed mineral soil
  exposed_min <- read.csv(here("data/exposed_min.csv"))
    
  expose_min_plot <- ggplot(exposed_min, 
                            aes(x = factor(TREATMENT),
                                y = MINERAL,
                                fill = SITE)) + 
    geom_boxplot() + 
    scale_fill_manual(name = "Site Type",
                      values = c("#d9d9d9", "#ffffff"), 
                      labels = c("Upland", "Lowland")) + 
    labs(title = "Exposed Mineral Soil", 
         x = "Number of Fires", y = "% of Mineral Soil Exposed") + 
    theme(plot.title = element_text(hjust = 0))
  expose_min_plot
  
# combining org and exp_min (fig. 7)
  substrate <- plot_grid( org_depth_plot, expose_min_plot,
                          nrow = 1, ncol = 2,
                          rel_widths = c(0.95,1.25),
                          labels = c("A.", "B."))
  
  save_plot("substrate.png", substrate, nrow = 1, ncol = 2, base_aspect_ratio = 1.5)
  
# getting numbers
  
  ## organic layer
  summary(org_depth$ORG_DEPTH[org_depth$TREATMENT == 0])
  
  # UPLANDS
  upland <- org_depth[org_depth$SITE == "DALTON",]
  summary(upland$ORG_DEPTH[upland$TREATMENT == 0])
  sd(upland$ORG_DEPTH[upland$TREATMENT == 0])
  
  summary(upland$ORG_DEPTH[upland$TREATMENT == 1])
  sd(upland$ORG_DEPTH[upland$TREATMENT == 1])
  
  summary(upland$ORG_DEPTH[upland$TREATMENT == 2])
  sd(upland$ORG_DEPTH[upland$TREATMENT == 2])
  
  summary(upland$ORG_DEPTH[upland$TREATMENT == 3])
  sd(upland$ORG_DEPTH[upland$TREATMENT == 3])
  
  # LOWLANDS
  lowland <- org_depth[org_depth$SITE == "STEESE",]
  summary(lowland$ORG_DEPTH[lowland$TREATMENT == 0])
  sd(lowland$ORG_DEPTH[lowland$TREATMENT == 0])
  
  summary(lowland$ORG_DEPTH[lowland$TREATMENT == 1])
  sd(lowland$ORG_DEPTH[lowland$TREATMENT == 1])
  
  summary(lowland$ORG_DEPTH[lowland$TREATMENT == 2])
  sd(lowland$ORG_DEPTH[lowland$TREATMENT == 2])
  
  summary(lowland$ORG_DEPTH[lowland$TREATMENT == 3])
  sd(lowland$ORG_DEPTH[lowland$TREATMENT == 3])
  
  
# factors
  mean(lowland$ORG_DEPTH[lowland$TREATMENT == 1]) / mean(upland$ORG_DEPTH[upland$TREATMENT == 1]) # 1.889
  
  mean(lowland$ORG_DEPTH[lowland$TREATMENT == 2]) / mean(upland$ORG_DEPTH[upland$TREATMENT == 2]) # 3.169
  
  mean(lowland$ORG_DEPTH[lowland$TREATMENT == 3]) / mean(upland$ORG_DEPTH[upland$TREATMENT == 3]) # 1.57
  
  mean(upland$ORG_DEPTH[upland$TREATMENT == 00]) / mean(upland$ORG_DEPTH[upland$TREATMENT == 1])
  mean(lowland$ORG_DEPTH[lowland$TREATMENT == 00]) / mean(lowland$ORG_DEPTH[lowland$TREATMENT == 1])
 