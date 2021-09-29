# spatial autocorrelation - density
# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
library(ape)
library(spdep)
library(here)
library(tidyverse)
options(scipen = 9999)
session <- sessionInfo()

dens <- read.csv(here("data/density.csv"))

# bringing in latitude and longitude
  # pulling in site attribute variables
  attrib <- read.csv(here("data/site_attrib.csv"))
  
  # getting order of sites in dens
    order <- as.vector(unique(dens$PLOT)) 
    attrib <- attrib %>%
      group_by(SITE, TREAT) %>%
      arrange(match(PLOT,order)) %>% # matches the same order of plots as density file
      slice(rep(1:n(), each = 8)) # replicates each value by 8, since there's 8 rows per plot (8 species)

  # adding in attributes to density file
  dens$LAT <- attrib$LAT
  dens$LONG <- attrib$LONG

# DALTON
# unburned

dalt_unb <- dens %>%
  filter(SITE == "DALTON") %>%
  filter(TREAT == 0)

  regen.dist <- as.matrix(dist(cbind(dalt_unb$LONG, dalt_unb$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(dalt_unb$COUNT_HA, regen.dist)

# once-burned
  dalt1 <- dens %>%
    filter(SITE == "DALTON") %>%
    filter(TREAT == 1)
  
  regen.dist <- as.matrix(dist(cbind(dalt1$LONG, dalt1$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(dalt1$COUNT_HA, regen.dist)

# twice-burned
  dalt2 <- dens %>%
    filter(SITE == "DALTON") %>%
    filter(TREAT == 2)
  
  regen.dist <- as.matrix(dist(cbind(dalt2$LONG, dalt2$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(dalt2$COUNT_HA, regen.dist)

# thrice-burned
  dalt3 <- dens %>%
    filter(SITE == "DALTON") %>%
    filter(TREAT == 3)
  
  regen.dist <- as.matrix(dist(cbind(dalt3$LONG, dalt3$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(dalt3$COUNT_HA, regen.dist)

# STEESE
  # unburned
  stee0 <- dens %>%
    filter(SITE == "STEESE") %>%
    filter(TREAT == 0)
  
  regen.dist <- as.matrix(dist(cbind(stee0$LONG, stee0$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(stee0$COUNT_HA, regen.dist)
  
  # once-burned
  stee1 <- dens %>%
    filter(SITE == "STEESE") %>%
    filter(TREAT == 1)
  
  regen.dist <- as.matrix(dist(cbind(stee1$LONG, stee1$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(stee1$COUNT_HA, regen.dist)
  
  # twice-burned
  stee2 <- dens %>%
    filter(SITE == "STEESE") %>%
    filter(TREAT == 2)
  
  regen.dist <- as.matrix(dist(cbind(stee2$LONG, stee2$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(stee2$COUNT_HA, regen.dist)
  
  # thrice-burned
  stee3 <- dens %>%
    filter(SITE == "STEESE") %>%
    filter(TREAT == 3)
  
  regen.dist <- as.matrix(dist(cbind(stee3$LONG, stee3$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(stee3$COUNT_HA, regen.dist)