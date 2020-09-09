# spatial autocorrelation - basal area
# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
library(ape)
library(spdep)
library(here)
library(tidyverse)
options(scipen = 9999)

ba <- read.csv(here("ba.csv"))

plot_ba <- ba %>%
  group_by(SITE,TREAT, PLOT) %>%
  summarise(BA = sum(BA_ha))

    # bringing in latitude and longitude
    # pulling in site attribute variables
    attrib <- read.csv(here("data/site_attrib.csv"))
    
    # getting order of sites in ba
    order <- as.vector(unique(ba$PLOT)) 
    attrib <- attrib %>%
      group_by(SITE, TREAT) %>%
      arrange(match(PLOT,order)) %>% # matches the same order of plots as baity file
      slice(rep(1:n(), each = 1)) # replicates each value by 8, since there's 8 rows per plot (8 species)
    
    # adding in attributes to ba file
    plot_ba$LAT <- attrib$LAT
    plot_ba$LONG <- attrib$LONG
    
# DALTON
  # unburned
    dalt0 <- plot_ba %>%
      filter(SITE == "DALTON") %>%
      filter(TREAT == 0)
    
    regen.dist <- as.matrix(dist(cbind(dalt0$LONG, dalt0$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(dalt0$BA, regen.dist)

  # once-burned
    dalt1 <- plot_ba %>%
      filter(SITE == "DALTON") %>%
      filter(TREAT == 1)
    
    regen.dist <- as.matrix(dist(cbind(dalt1$LONG, dalt1$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(dalt1$BA, regen.dist)
    
    # twice-burned
    dalt2 <- plot_ba %>%
      filter(SITE == "DALTON") %>%
      filter(TREAT == 2)
    
    regen.dist <- as.matrix(dist(cbind(dalt2$LONG, dalt2$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(dalt2$BA, regen.dist)
    
    # thrice-burned
    dalt3 <- plot_ba %>%
      filter(SITE == "DALTON") %>%
      filter(TREAT == 3)
    
    regen.dist <- as.matrix(dist(cbind(dalt3$LONG, dalt3$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(dalt3$BA, regen.dist)

# STEESE
    # unburned
    stee0 <- plot_ba %>%
      filter(SITE == "STEESE") %>%
      filter(TREAT == 0)
    
    regen.dist <- as.matrix(dist(cbind(stee0$LONG, stee0$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(stee0$BA, regen.dist)
    
    # once-burned
    stee1 <- plot_ba %>%
      filter(SITE == "STEESE") %>%
      filter(TREAT == 1)
    
    regen.dist <- as.matrix(dist(cbind(stee1$LONG, stee1$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(stee1$BA, regen.dist)
    
    # twice-burned
    stee2 <- plot_ba %>%
      filter(SITE == "STEESE") %>%
      filter(TREAT == 2)
    
    regen.dist <- as.matrix(dist(cbind(stee2$LONG, stee2$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(stee2$BA, regen.dist)
    
    # thrice-burned
    stee3 <- plot_ba %>%
      filter(SITE == "STEESE") %>%
      filter(TREAT == 3)
    
    regen.dist <- as.matrix(dist(cbind(stee3$LONG, stee3$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(stee3$BA, regen.dist)
    