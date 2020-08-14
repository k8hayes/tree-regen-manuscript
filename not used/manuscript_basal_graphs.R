# basal plot
library(dplyr)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
options(scipen=999)


# dalton tree / saplings
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)
all <- rbind(tree,sapling)
all <- all[all$SITE == "DALTON",]
all <- all[all$TREAT !=0,]
meanAll <- all %>%
  group_by(all$TREAT,all$SITE, all$DIVISON) %>%
    summarise(MEAN = mean(BASAL_HA), SD = sd(BASAL_HA))
colnames(meanAll) <- c("TREAT","SITE", "DIVISION", "MEAN", "SD")

dalt<- ggplot(meanAll, aes(x = as.factor(TREAT), y = MEAN, col = DIVISION)) + 
  geom_point() + geom_line(aes(group = DIVISION), size = 1)  +
  labs(x = "Number of Fires",y = "", 
       title = "Upland BA") + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
                     name = "Division", labels = c("Conifer", "Deciduous")) + 
  geom_errorbar(aes(ymin = MEAN - SD, 
                    ymax = MEAN + SD), 
                width = .75, position = position_dodge(0.1)) + 
  background_grid() + panel_border() +
  theme(legend.position = "none")+ ylim(-80,140)

# steese tree / saplings
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)
all <- rbind(tree,sapling)
all <- all[all$SITE == "STEESE",]
all <- all[all$TREAT != 0,]
meanAll <- all %>%
  group_by(all$TREAT, all$DIVISON) %>%
  summarise(MEAN = mean(BASAL_HA), SD = sd(BASAL_HA))
colnames(meanAll) <- c("TREAT","DIVISION", "MEAN", "SD")

steese <- ggplot(meanAll, aes(x = as.factor(TREAT), y = MEAN, col = DIVISION)) + 
  geom_point() + geom_line(aes(group = DIVISION), size = 1)  +
  labs(x = "Number of Fires",y = " ", 
       title = "Lowland BA") + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
                     name = "Division", labels = c("Conifer", "Deciduous")) + 
  geom_errorbar(aes(ymin = MEAN - SD, 
                    ymax = MEAN + SD), 
                width = .75, position = position_dodge(0.1)) + 
  background_grid() + panel_border() + ylim(-80,140)

# unburned sites
tree <- read.csv("tree.csv", stringsAsFactors = F)
sapling <- read.csv("sapling.csv", stringsAsFactors = F)
all <- rbind(tree,sapling)
all <- all[all$TREAT ==0,]
all$SITE[all$SITE == "DALTON"] <- "Upland"
all$SITE[all$SITE == "STEESE"] <- "Lowland"
control <- ggplot(all, aes(x = SITE, y = BASAL_HA, col = DIVISON))  + 
  geom_boxplot()  +
  labs(x = "Site",y = "Average Basal Area (cm2/Ha)", 
       title = "Unburned BA")  + 
  scale_color_manual(values = c("#fe9929", "#3182bd"),
         name = "DIVISON", labels = c("Conifer", "Deciduous")) + 
  background_grid() + panel_border() + theme(legend.position = "none") + 
  scale_x_discrete(limits = c("Upland", "Lowland"))


basal <- plot_grid(control, dalt, steese, ncol = 3, rel_widths = c(1.15,1.15,1.5))
save_plot("basal.png", basal, base_asp = 2.618, base_height = 3.71)
