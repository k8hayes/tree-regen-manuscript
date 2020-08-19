# spatial autocorrelation - density
# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
library(ape)
library(spdep)
library(here)
library(tidyverse)
options(scipen = 9999)

# DALTON
# unburned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "DALTON",]
  both <- both[both$TREAT == "0",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)

# once-burned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "DALTON",]
  both <- both[both$TREAT == "1",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)

# twice-burned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "DALTON",]
  both <- both[both$TREAT == "2",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)

# thrice-burned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "DALTON",]
  both <- both[both$TREAT == "3",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)

# STEESE
  # unburned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "STEESE",]
  both <- both[both$TREAT == "0",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)
  
  # once-burned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "STEESE",]
  both <- both[both$TREAT == "1",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)
  
  # twice-burned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "STEESE",]
  both <- both[both$TREAT == "2",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)
  
  # thrice-burned
  tree <- read.csv(here("tree regen manuscript/tree.csv"))
  sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
  both <- rbind(tree, sapling) ; rm(tree, sapling)
  both <- both[both$SITE == "STEESE",]
  both <- both[both$TREAT == "3",]
  
  regen.dist <- as.matrix(dist(cbind(both$LONG, both$LAT)))
  regen.dist.inv <- 1/regen.dist
  diag(regen.dist.inv) <-0
  regen.dist.inv[1:5, 1:5]
  
  Moran.I(both$COUNT_HA, regen.dist)