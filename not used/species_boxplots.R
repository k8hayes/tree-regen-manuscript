# species boxplots
library(ggplot2)
library(cowplot)

# total_regen <- read.csv("data/total_regen.csv")
#org_depth <- read.csv("data/Org_Depth.csv")
#colnames(org_depth) <- c("SITE", "SITECODE", "SITENUM", "TREATMENT", "SUBPLOT", "ORG_DEPTH")

# test <- merge(total_regen, org_depth, by = c("SITENUM", "SITE", "TREATMENT", "SITECODE"))
# names(test)
# test <- test[-5]

# names(test) <- c("SITENUM", "DIVISION", "SPECIES", "STEM_COUNT", "UNBROWS_COUNT", "BROWSE_COUNT", "QUAD", "REGEN_TYPE","stem_count_ha", "SITE", "SITECODE", "TREATMENT", "CORNER", "ORG_DEPTH")
# total_regen <- test
# write.csv(total_regen, "regen_orgdepth.csv")
total_regen <- read.csv("regen_orgdepth.csv")

# changing primary / secondary
total_regen$REGEN_TYPE <- as.character(total_regen$REGEN_TYPE)
total_regen$REGEN_TYPE[total_regen$REGEN_TYPE == "PRIMARY"] <- "COLONIZATION"
total_regen$REGEN_TYPE[total_regen$REGEN_TYPE == "SECONDARY"] <- "ESTABLISHMENT"
total_regen$REGEN_TYPE <- as.factor(total_regen$REGEN_TYPE)

# just deciduous
decid <- total_regen[total_regen$DIVISON == "d",]
ggplot(decid, aes(x = factor(decid$TREATMENT), y = log(decid$stem_count_ha))) + 
  geom_boxplot(aes(fill = decid$SITE)) + 
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Deciduous Regeneration") + 
  scale_fill_manual(values = c( "#d95f0e", "#fec44f"), name = "Site type",  labels = c("Upland", "Lowland")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0)) + 
  coord_cartesian(ylim = c(3, 12.5)) + panel_border() + background_grid()


pime <- total_regen[total_regen$SPECIES == "PIME",]
pime1 <- pime[pime$REGEN_TYPE == "COLONIZATION",]
pime2 <- pime[pime$REGEN_TYPE == "ESTABLISHMENT",]
pime1_ggplot <- ggplot(pime1, aes(x = factor(pime1$TREATMENT), y = log(pime1$stem_count_ha))) + 
  geom_boxplot(aes(fill = pime1$SITE)) + 
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Black Spruce Colonization") + 
  scale_fill_manual(values = c( "#d95f0e", "#fec44f"), name = "Site type",  labels = c("Upland", "Lowland")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0)) + 
  coord_cartesian(ylim = c(3, 12.5)) + panel_border() + background_grid()

pime2_gplot <- ggplot(pime2, aes(x = factor(pime2$TREATMENT), y = log(pime2$stem_count_ha))) +
  geom_boxplot(aes(fill = pime2$SITE)) + 
  labs(x = "Number of Fires", y = "", title = "Black Spruce Establishment") + 
  scale_fill_manual(values = c( "#d95f0e", "#fec44f"), name = "Site type",  labels = c("Upland", "Lowland")) + 
  theme( plot.title = element_text(hjust = 0)) + 
  coord_cartesian(ylim = c(3, 12.5)) + panel_border() + background_grid()
pime12 <- plot_grid(pime1_ggplot, pime2_gplot, rel_widths = c(0.9,1.3), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("pime12.png", pime12, nrow = 1, ncol = 2, base_aspect_ratio = 1.5)
pime12

ggplot(pime, aes(x = factor(pime$TREATMENT), y = log(pime$stem_count_ha))) + 
  geom_boxplot(aes(fill = pime$SITE))+ facet_grid(pime$REGEN_TYPE) + 
  background_grid() + panel_border() +
  labs(x = "Treatment", y = "Log-Transformed Stem count per Ha", title = "Black Spruce Establishment") + 
  scale_fill_manual(values = c( "#d95f0e", "#fec44f"), name = "Site / Severity",  
                    labels = c("Dalton / High", "Steese / Moderate")) 


ggplot(pime, aes(x = pime$ORG_DEPTH, y = pime$stem_count_ha)) + geom_point()

salix <- total_regen[total_regen$SPECIES == "SALIX",]
salix1 <- salix[salix$REGEN_TYPE == "COLONIZATION",]
salix2 <- salix[salix$REGEN_TYPE == "ESTABLISHMENT",]
salix1_plot <- ggplot(salix1, aes(x = factor(salix1$TREATMENT), y = log(salix1$stem_count_ha))) + 
  geom_boxplot(aes(fill = salix1$SITE)) +
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Salix Colonization") + 
  scale_fill_manual(values = c( "#31a354","#addd8e"), name = "Site type",
                    labels = c("Upland", "Lowland")) + 
  coord_cartesian(ylim = c(4, 12) ) + panel_border( ) + background_grid() + 
  theme( plot.title = element_text(hjust = 0), legend.position = "none")
salix2_plot <- ggplot(salix2, aes(x = factor(salix2$TREATMENT), y = log(salix2$stem_count_ha))) + 
  geom_boxplot(aes(fill = salix2$SITE)) +
  labs(x = "Number of Fires", y = "", title = "Salix Establishment") + 
  scale_fill_manual(values = c("#31a354", "#addd8e"), name = "Site type",
                    labels = c("Upland", "Lowland")) + 
  coord_cartesian(ylim = c(4, 12) ) + panel_border( ) + background_grid() + 
  theme( plot.title = element_text(hjust = 0))
salix12<- plot_grid(salix1_plot, salix2_plot, rel_widths = c(0.9,1.3), rel_heights = c(1,1), labels = c("C.", "D."))
save_plot("salix12.png", salix12, nrow = 1, ncol = 2, base_aspect_ratio = 1.5)

salix2 <- salix2[salix2$TREATMENT > 0,]
salix_estab_plot <- ggplot(salix2, aes(x = factor(salix2$TREATMENT), y = log(salix2$stem_count_ha))) + 
  geom_boxplot(aes(fill = salix2$SITE)) +
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Salix Establishment") + 
  scale_fill_manual(values = c("#31a354", "#addd8e"), name = "Site type",
                    labels = c("Upland", "Lowland"))  + panel_border( ) + background_grid() + 
  theme( plot.title = element_text(hjust = 0))
save_plot("salix_estab.png", salix_estab_plot, base_aspect_ratio = 1.5)

salix <- total_regen[total_regen$SPECIES == "SALIX",]
salix1 <- salix[salix$REGEN_TYPE == "PRIMARY",]
salix2 <- salix[salix$REGEN_TYPE == "SECONDARY",]
  
  ggplot(salix, aes(x = factor(salix$TREATMENT), y = log(salix$stem_count_ha))) + 
    geom_boxplot(aes(fill = salix$SITE))+ facet_grid(salix$REGEN_TYPE) + 
    background_grid() + panel_border() +
    labs(x = "Treatment", y = "Log-Transformed Stem count per Ha", title = "Salix") + 
    scale_fill_manual(values = c( "#31a354", "#addd8e"), name = "Site / Severity",  
                      labels = c("Dalton / High", "Steese / Moderate")) 
  

bene <- total_regen[total_regen$SPECIES == "BENE",]  
bene1 <- bene[bene$REGEN_TYPE == "COLONIZATION",]
bene2 <- bene[bene$REGEN_TYPE == "ESTABLISHMENT",]
bene1_plot <- ggplot(bene1, aes(x = factor(bene1$TREATMENT), y = log(bene1$stem_count_ha))) + 
  geom_boxplot(aes(fill = bene1$SITE)) + 
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Birch Colonization") + 
  scale_fill_manual(values = c("#2b8cbe", "#a6bddb"), name = "Site type",
                    labels = c("Upland", "Lowland")) + 
  panel_border() + background_grid() +
  coord_cartesian(ylim = c(2, 13)) + theme( plot.title = element_text(hjust = 0), legend.position =  "none")
bene2_plot <- ggplot(bene2, aes(x = factor(bene2$TREATMENT), y = log(bene2$stem_count_ha))) + 
  geom_boxplot(aes(fill = bene2$SITE)) + 
  labs(x = "Number of Fires", y = "", title = "Birch Establishment") + 
  scale_fill_manual(values = c("#2b8cbe", "#a6bddb"), name = "Site type",
                    labels = c("Upland", "Lowland")) +  
  panel_border() + background_grid() + theme( plot.title = element_text(hjust = 0)) + 
  coord_cartesian(ylim = c(2, 13))
bene12<- plot_grid(bene1_plot, bene2_plot, rel_widths = c(0.9,1.3), rel_heights = c(1,1), labels = c("A.", "B."))
save_plot("bene12.png", bene12, nrow = 1, ncol = 2, base_aspect_ratio = 1.5)

bene2 <- bene2[bene2$TREATMENT > 0,]
bene_estab_plot <- ggplot(bene2, aes(x = factor(bene2$TREATMENT), y = log(bene2$stem_count_ha))) + 
  geom_boxplot(aes(fill = bene2$SITE)) + 
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Birch Establishment") + 
  scale_fill_manual(values = c("#2b8cbe", "#a6bddb"), name = "Site type",
                    labels = c("Upland", "Lowland")) +  
  panel_border() + background_grid() + theme( plot.title = element_text(hjust = 0)) 
save_plot("bene_estab.png", bene_estab_plot, base_aspect_ratio = 1.5)

ggplot(bene, aes(x = factor(bene$TREATMENT), y = log(bene$stem_count_ha))) + 
  geom_boxplot(aes(fill = bene$SITE))+ facet_grid(bene$REGEN_TYPE) + 
  background_grid() + panel_border() +
  labs(x = "Treatment", y = "Log-Transformed Stem count per Ha") + 
  scale_fill_manual(values = c( "#2b8cbe", "#a6bddb"), name = "Site / Severity",  
                    labels = c("Dalton / High", "Steese / Moderate")) 

alcr <- total_regen[total_regen$SPECIES == "ALCR",]
alcr1 <- alcr[alcr$REGEN_TYPE == "COLONIZATION",]
alcr2 <- alcr[alcr$REGEN_TYPE == "ESTABLISHMENT",]
alcr1_plot <- ggplot(alcr1, aes(x = factor(alcr1$TREATMENT), y = log(alcr1$stem_count_ha), fill = alcr1$SITE)) + 
  geom_boxplot() +
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Alder Colonization") + 
  scale_fill_manual(values = c("#e34a33", "#fdbb84"), name = "Site Type",
                    labels = c("Upland", "Lowland")) + panel_border() + background_grid()  + 
  theme( plot.title = element_text(hjust = 0), legend.position =  "none") +
  coord_cartesian(ylim = c(3,12))
alcr2_plot <- ggplot(alcr2, aes(x = factor(alcr2$TREATMENT), y = log(alcr2$stem_count_ha))) + 
  geom_boxplot(aes(fill = alcr2$SITE)) +
  labs(x = "Number of Fires", y = "", title = "Alder Establishment") + 
  scale_fill_manual(values = c("#e34a33", "#fdbb84"), name = "Site Type",
                    labels = c("Upland", "Lowland")) +  panel_border() + background_grid()  + 
  theme( plot.title = element_text(hjust = 0)) + 
  coord_cartesian(ylim = c(3, 12))
alcr12 <- plot_grid(alcr1_plot, alcr2_plot, rel_widths = c(0.9,1.3), rel_heights = c(1,1), labels = c("E.", "F."))
save_plot("alcr12.png", alcr12, nrow = 1, ncol = 2, base_aspect_ratio = 1.5)
alcr2_plot

potr <- total_regen[total_regen$SPECIES == "POTR",]
potr1 <- potr[potr$REGEN_TYPE == "COLONIZATION",]
potr2 <- potr[potr$REGEN_TYPE == "ESTABLISHMENT",]
potr1_plot <- ggplot(potr1, aes(x = factor(potr1$TREATMENT), y = log(potr1$stem_count_ha), fill = potr1$SITE)) + 
  geom_boxplot() +
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Aspen Colonization") + 
  scale_fill_manual(values = c("#e7e1ef", "#c994c7"), name = "Site Type",
                    labels = c("Upland", "Lowland")) + panel_border() + background_grid()  + 
  theme( plot.title = element_text(hjust = 0), legend.position =  "none") +
  coord_cartesian(ylim = c(4,11))
potr2_plot <- ggplot(potr2, aes(x = factor(potr2$TREATMENT), y = log(potr2$stem_count_ha))) + 
  geom_boxplot(aes(fill = potr2$SITE)) +
  labs(x = "Number of Fires", y = "", title = "Aspen Establishment") + 
  scale_fill_manual(values = c("#e7e1ef", "#c994c7"), name = "Site Type",
                    labels = c("Upland", "Lowland")) +  panel_border() + background_grid()  + 
  theme( plot.title = element_text(hjust = 0)) +
  coord_cartesian(ylim = c(4,11))
potr12<- plot_grid(potr1_plot, potr2_plot, rel_widths = c(0.9,1.3), rel_heights = c(1,1), labels = c("G.", "H."))
save_plot("potr12.png", potr12, nrow = 1, ncol = 2, base_aspect_ratio = 1.5)

## just establishment
alcr2 <- alcr2[alcr2$TREATMENT >0,]
alcr_estab_plot <- ggplot(alcr2, aes(x = factor(alcr2$TREATMENT), y = log(alcr2$stem_count_ha))) + 
  geom_boxplot(aes(fill = alcr2$SITE)) +
  labs(x = "Number of Fires", y = "Log-Transformed Stem count per Ha", title = "Alder Establishment") + 
  scale_fill_manual(values = c("#e34a33", "#fdbb84"), name = "Site Type",
                    labels = c("Upland", "Lowland")) +  panel_border() + background_grid()  + 
  theme( plot.title = element_text(hjust = 0)) 
 save_plot("alcr_estab.png", alcr_estab_plot, base_aspect_ratio = 1.5)

 alcr <- total_regen[total_regen$SPECIES == "ALCR",]
 alcr1 <- alcr[alcr$REGEN_TYPE == "PRIMARY",]
 alcr2 <- alcr[alcr$REGEN_TYPE == "SECONDARY",]
 

ggplot(alcr, aes(x = factor(alcr$TREATMENT), y = log(alcr$stem_count_ha))) + 
  geom_boxplot(aes(fill = alcr$SITE))+ facet_grid(alcr$REGEN_TYPE) + 
  background_grid() + panel_border() +
  labs(x = "Treatment", y = "Log-Transformed Stem count per Ha") + 
  scale_fill_manual(values = c( "#e34a33", "#fdbb84"), name = "Site Type",  
                    labels = c("Upland", "Lowland")) 

##########
# incorporating species data with organic depth and exposed mineral soil
exposed_min <- read.csv("exposed_min.csv")
test <- merge(total_regen, exposed_min, by = c("SITENUM", "SITECODE", "SITE", "TREATMENT", "SUBPLOT"))
# test <- test[-5] ; test <- test[-15]
species_incorp <- test
rm(test)

## testing with PIME
pime_incorp <- species_incorp[species_incorp$SPECIES == "PIME",]
ggplot(pime_incorp, aes(x = pime_incorp$ORG_DEPTH, y = log(pime_incorp$stem_count_ha), col = pime_incorp$REGEN_TYPE)) + 
  geom_point()


## BENE
bene_incorp <- species_incorp[species_incorp$SPECIES == "BENE",]
ggplot(bene_incorp, aes(x = bene_incorp$ORG_DEPTH, y = log(bene_incorp$stem_count_ha), col = bene_incorp$REGEN_TYPE)) + 
  geom_point()

plot(log(total_regen$stem_count_ha)~total_regen$ORG_DEPTH)

test <- lm(total_regen$stem_count_ha ~ total_regen$SPECIES + total_regen$TREATMENT + total_regen$SITE + total_regen$TREATMENT:total_regen$SITE)
summary(test)
 anova(test)

pime_test <-  lm(pime$stem_count_ha ~ pime$TREATMENT * pime$SITE)
summary(pime_test)
stripchart(log(stem_count_ha) ~ TREATMENT * SITE, data = pime, method = "jitter",
           pch = 16, vertical = T)

pime_test2 <- lm(pime$stem_count_ha ~ pime$ORG_DEPTH + pime$TREATMENT*pime$SITE)
summary(pime_test2)

## testing pime linear model

