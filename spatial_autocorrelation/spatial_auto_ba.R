# spatial autocorrelation - basal area
# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
library(ape)
library(spdep)
library(here)
library(tidyverse)
options(scipen = 9999)

# putting long/lat into basal area
    # spp_ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    # site_attributes <- read.csv(here("site_attributes.csv"), stringsAsFactors = F)  
    # site <- site_attributes %>%
      # group_by(site_attributes$SITE, site_attributes$FIRES,
        #       site_attributes$SITECODE) %>%
      #slice(rep(1:n(), each = 4))
    #spp_ba$LAT <- site$LAT ; spp_ba$LONG <- site$LONG ; rm(site, site_attributes)
    # write.csv(spp_ba, here("tree regen manuscript/spp_ba.csv"), row.names = F)

ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))

# DALTON
  # unburned
    ba <- ba[ba$SITE == "DALTON",]
    ba <- ba[ba$TREAT == "0",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)

  # once-burned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "DALTON",]
    ba <- ba[ba$TREAT == "1",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)
    
    # twice-burned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "DALTON",]
    ba <- ba[ba$TREAT == "2",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)
    
    # thrice-burned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "DALTON",]
    ba <- ba[ba$TREAT == "3",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)

# STEESE
    # unburned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "STEESE",]
    ba <- ba[ba$TREAT == "0",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)
    
    # once-burned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "STEESE",]
    ba <- ba[ba$TREAT == "1",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)
    
    # twice-burned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "STEESE",]
    ba <- ba[ba$TREAT == "2",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)
    
    # thrice-burned
    ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))
    ba <- ba[ba$SITE == "STEESE",]
    ba <- ba[ba$TREAT == "3",]
    regen.dist <- as.matrix(dist(cbind(ba$LONG, ba$LAT)))
    regen.dist.inv <- 1/regen.dist
    diag(regen.dist.inv) <-0
    
    Moran.I(ba$BA_DIV, regen.dist)
    