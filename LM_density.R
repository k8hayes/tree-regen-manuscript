# LMs for density
library(sjPlot) 
library(here)
library(parameters)
library(ggplot2) 
library(cowplot); theme_set(theme_cowplot()) 
options(scipen = 9999)

tree <- read.csv(here("tree.csv"))
sapling <- read.csv(here("sapling.csv"))
both <- rbind(tree, sapling) ; rm(tree, sapling)

# removing unburned sites
both <- both[both$TREAT !=0,]

# MODEL - conifer density
mCD <- lm(CONIF_COUNT_HA ~ TREAT+ SITE, data = both) 
summary(mCD)
model_parameters(mCD)

# visualizing results
plot_model(mCD, show.values = T, show.p = T, sort.est = T)
plot_model(mCD, type = "pred", terms = c("TREAT", "SITE")) # like as a visual # but 1.5 fires is not a real thing

# MODEL - deciduous density
mDD <- lm(DECID_COUNT_HA ~ TREAT + SITE, data = both)
summary(mDD)
model_parameters(mDD)

plot_model(mDD, show.values = T, show.p = T, sort.est = T) 
plot_model(mDD, type = "pred", terms = c("TREAT", "SITE")) # like as a visual # but 1.5 fires is not a real thing

