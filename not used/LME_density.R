# LMEs for density
library(lme4)
library(sjPlot) 
library(lattice)
library(nlme)
library(here)
library(effectsize)
library(parameters)
library(ggplot2) 
library(cowplot) 
theme_set(theme_cowplot()) 
options(scipen = 9999)

tree <- read.csv(here("tree regen manuscript/tree.csv"))
sapling <- read.csv(here("tree regen manuscript/sapling.csv"))
both <- rbind(tree, sapling) ; rm(tree, sapling)

# standardize all fixed effects (allows for comparison of effect sizes) # function comes from "effectsize" pkge
both$OL_AV <- standardize(both$OL_AV)
both$SLOPE <- standardize(both$SLOPE)
both$SOLAR <- standardize(both$SOLAR)
both$EXP_MIN <- standardize(both$EXP_MIN)

# removing unburned sites
both <- both[both$TREAT !=0,]

# FIRE MODEL - conifer density
  # model acronym: modeled conifer density fire 
  mCDF1 <- lmer(CONIF_COUNT_HA ~ TREAT+ (1|SITE), data = both) 
  summary(mCDF1)
  model_parameters(mCDF1)

  # double checking significance of interactive effect
  plot_model(mCDF1, type = "pred", terms = c("TREAT", "SITE"), show.data = T) # plots the model with CI # not as useful for conifers
  model_parameters(mCDF2) # prints parameters # used in figure 2

# FIRE MODEL - deciduous density
  # model acronym: modeled decid density fire
  mDDF1 <- lmer(DECID_COUNT_HA ~ TREAT + (1 | SITE), data = both)
  summary(mDDF1)
  model_parameters(mDDF1)

  plot_model(mDDF2, show.values = T, show.p = T, sort.est = T) # plots effect size # show values/p shows value/p 
  plot_model(mDDF2, type = "pred", terms = c("TREAT", "SITE")) # like as a visual # but 1.5 fires is not a real thing
  plot_model(mDDF2, type = "pred", terms = c("TREAT", "SITE"), show.data = T) # dalton outlier messes this up a bit
  model_parameters(mDDF2)

# SITE MODEL - conifer density
  # model acronym: modeled conifer density site attributes
  # started with: SLOPE + SOLAR  + OL_AV + EXP_MIN + (1 + TREAT | SITE)
  # removed: EXP_MIN
  mCDS1 <- lmer(CONIF_COUNT_HA ~ OL_AV + EXP_MIN  + (1 | TREAT ), data = both)
  mCDS2 <- lmer(CONIF_COUNT_HA ~  EXP_MIN  + (1 | TREAT ), data = both)
  anova(mCDS1, mCDS2) # mCDS1
  summary(mCDS1)
  model_parameters(mCDS1)
  
    plot_model(mCDS2, show.values = T, show.p = T, sort.est = T) # same function as before # now plotting effect sizes
    plot_model(mCDS1, show.values = T, show.p = T, sort.est = T)
   # show.p and show.values show what they say # sort.est sorts by effect size
   # effect sizes/significance entered into table 4

# SITE MODEL - deciduous density
    # model acronym: modeled deciduous density site attributes
    # started with: SLOPE + SOLAR  + ELEVATION  + OL_AV + EXP_MIN + (1 + TREAT | SITE)
    # removed: SLOPE
    mDDS1 <- lmer(DECID_COUNT_HA ~ SLOPE + EXP_MIN  + OL_AV + (1 | TREAT ), data = both) 
    mDDS2 <- lmer(DECID_COUNT_HA ~  OL_AV + EXP_MIN + (1 | TREAT), data = both) 
    anova(mDDS1, mDDS2) # mDDS1
    model_parameters(mDDS1)
    
    plot_model(mDDS2, show.values = T, show.p = T, sort.est = T)
    plot_model(mDDS1, show.values = T, show.p = T, sort.est = T)
    
