# calculating average distance between plots
library(here)
library(tidyverse)
library(fossil)
session <- sessionInfo()

coord <- read.csv(here("data/site_locations.csv"))

dalt_coord <- coord %>%
  filter(SITE == "DALTON")
dalt_coord <- dalt_coord[,-c(1,2,3)]

steese_coord <- coord %>%
  filter(SITE == "STEESE")
steese_coord <- steese_coord[,-c(1,2,3)]

dalt_dist <- earth.dist(dalt_coord, dist = TRUE)
dalt_dist <- as.vector(dalt_dist)
summary(dalt_dist)
dalt_dist_m <- dalt_dist*1000
summary(dalt_dist_m)

stee_dist <- earth.dist(steese_coord, dist = TRUE)
stee_dist <- as.vector(stee_dist)
summary(stee_dist)
stee_dist_m <- stee_dist*1000
summary(stee_dist_m)

coord_dist_m <- c(dalt_dist_m, stee_dist_m)
summary(coord_dist_m)
