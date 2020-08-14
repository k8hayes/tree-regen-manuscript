# density plot
library(dplyr)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
options(scipen=999)
setwd("Google Drive/UC Denver/Project/R/tree regen manuscript/")
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)

# dalton tree / saplings
all <- rbind(tree,sapling)
all <- all[all$SITE == "DALTON",]
meanAll <- all %>%
  group_by(all$TREAT, all$DIVISON) %>%
  summarise(MEAN = mean(COUNT_HA), SD = sd(COUNT_HA))
colnames(meanAll) <- c("TREAT","DIVISION", "MEAN", "SD")

dalt_tree_sap <- ggplot(meanAll, aes(x = as.factor(TREAT), y = MEAN, col = DIVISION)) + 
  geom_point() + geom_line(aes(group = DIVISION))  +
  labs(x = "",y = "Av. Stems/Ha", 
       title = "Upland Tree Density") + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
                     name = "Growth Form", labels = c("Conifer", "Deciduous")) + 
  geom_errorbar(aes(ymin = MEAN - SD, 
                    ymax = MEAN + SD), 
                width = .5, position = position_dodge(0.1)) + 
  background_grid() + panel_border() +
  theme(legend.position = "none")
dalt_tree_sap
# dalton seedlings
seedling <- read.csv("seedling.csv", stringsAsFactors = F)
seedling <- seedling[seedling$SITE == "Upland",]
meanSeedD <- seedling %>%
  group_by(seedling$TREAT) %>%
  summarise(MEAN = mean(DECID_COUNT_HA), SD = sd(DECID_COUNT_HA))
meanSeedC <- seedling %>%
  group_by(seedling$TREAT) %>%
  summarise(MEAN = mean(CONIF_COUNT_HA), SD = sd(CONIF_COUNT_HA))
meanSeedC$DIVISION <- "c"
meanSeedD$DIVISION <- "d"
meanSeed <- rbind(meanSeedC, meanSeedD)
colnames(meanSeed) <- c("TREAT","MEAN", "SD", "DIVISION")

dalt_seed <- ggplot(meanSeed, aes(x = as.factor(TREAT), y = MEAN, group = DIVISION)) + 
  geom_point(aes(col = DIVISION)) + geom_line(aes(col = DIVISION))  +
  labs(x = "",y = "", 
       title = "Upland Seedling Density") + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
                     name = "Growth Form", labels = c("Conifer", "Deciduous")) + 
  geom_errorbar(aes(ymin = MEAN - SD, 
                    ymax = MEAN + SD, col = DIVISION), 
                width = .5, position = position_dodge(0.15)) + 
  background_grid() + panel_border()
dalt_seed
dalton <- plot_grid(dalt_tree_sap, dalt_seed, rel_widths = c(1.5,2.25))
dalton

# steese tree / saplings
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)
all <- rbind(tree,sapling)
all <- all[all$SITE == "STEESE",]
meanAll <- all %>%
  group_by(all$TREAT, all$DIVISON) %>%
  summarise(MEAN = mean(COUNT_HA), SD = sd(COUNT_HA))
colnames(meanAll) <- c("TREAT","DIVISION", "MEAN", "SD")

stee_tree_sap <- ggplot(meanAll, aes(x = as.factor(TREAT), y = MEAN, col = DIVISION)) + 
  geom_point() + geom_line(aes(group = DIVISION))  +
  labs(x = "Number of Fires",y = "Av. Stems/Ha", 
       title = "Lowland Tree Density") + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
                     name = "Growth Form", labels = c("Conifer", "Deciduous")) + 
  geom_errorbar(aes(ymin = MEAN - SD, 
                    ymax = MEAN + SD), 
                width = .5, position = position_dodge(0.1)) + 
  background_grid() + panel_border() +
  theme(legend.position = "none")
stee_tree_sap
# steese seedlings
seedling <- read.csv("seedling.csv", stringsAsFactors = F)
seedling <- seedling[seedling$SITE == "Lowland",]
meanSeedD <- seedling %>%
  group_by(seedling$TREAT) %>%
  summarise(MEAN = mean(DECID_COUNT_HA), SD = sd(DECID_COUNT_HA))
meanSeedC <- seedling %>%
  group_by(seedling$TREAT) %>%
  summarise(MEAN = mean(CONIF_COUNT_HA), SD = sd(CONIF_COUNT_HA))
meanSeedC$DIVISION <- "c"
meanSeedD$DIVISION <- "d"
meanSeed <- rbind(meanSeedC, meanSeedD)
colnames(meanSeed) <- c("TREAT","MEAN", "SD", "DIVISION")

stee_seed <- ggplot(meanSeed, aes(x = as.factor(TREAT), y = MEAN, group = DIVISION)) + 
  geom_point(aes(col = DIVISION)) + geom_line(aes(col = DIVISION))  +
  labs(x = "Number of Fires",y = "", 
       title = "Lowland Seedling Density") + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
                     name = "Growth Form", labels = c("Conifer", "Deciduous")) + 
  geom_errorbar(aes(ymin = MEAN - SD, 
                    ymax = MEAN + SD, col = DIVISION), 
                width = .5, position = position_dodge(0.15)) + 
  background_grid() + panel_border() 
stee_seed
steese <- plot_grid(stee_tree_sap, stee_seed, rel_widths = c(1.5,2.25))

density <- plot_grid(dalton, steese, nrow = 2)
density
save_plot("density.png", density, base_aspect_ratio = 2.25) 


# ALSO trying to get slope
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)
all <- rbind(tree,sapling)
all <- all[all$DIVISON == "c",]
lm(COUNT_HA ~ TREAT, data = all)
plot(COUNT_HA ~ as.factor(TREAT), data = all)
