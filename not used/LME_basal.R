# LMEs - basal area
#library(lme4)
#library(sjPlot) 
#library(lattice)
#library(nlme)
#library(here)
#library(effectsize)
#library(parameters)
#library(ggplot2) 
#library(cowplot) 
#library(tidyverse)
#theme_set(theme_cowplot()) 
options(scipen = 9999)

ba <- read.csv(here("tree regen manuscript/ba.csv"), stringsAsFactors = F)
ba <- ba[ba$TREAT != 0,]

# FIRE MODEL -  conifer basal area
  # model acronym: modeled conifer basal area fire
  mCBF1 <- lmer(CONIF_BA ~ TREAT + (1|SITE), data = ba)
  model_parameters(mCBF1)

# FIRE MODEL - deciduous basal area
  # model acronym: modeled deciduous basal area fire
  mDBF <- lmer(DECID_BA ~ TREAT + (1|SITE), data = ba)
  summary(mDBF)
  model_parameters(mDBF)

# SITE MODEL
  spp_ba <- read.csv(here("tree regen manuscript/spp_ba.csv"),
                     stringsAsFactors = F)
  spp_ba <- spp_ba[spp_ba$TREAT != 0,]
  spp_ba <- spp_ba[spp_ba$DIV == "c",]
  spp_ba$OL_AV <- standardize(spp_ba$OL_AV)
  spp_ba$SLOPE <- standardize(spp_ba$SLOPE)
  spp_ba$SOLAR <- standardize(spp_ba$SOLAR)
  spp_ba$EXP_MIN <- standardize(spp_ba$EXP_MIN)
  
# SITE MODEL -  conifer basal area
  # model acronym: modeled conifer basal area site attributes
  # started with: SLOPE + SOLAR + OL_AV + EXP_MIN + (1 + TREAT | SITE)
  # removed: EXP_MIN
  mCBS1 <- lmer(BA_DIV ~ SLOPE +  SOLAR + OL_AV + EXP_MIN + (1 | TREAT), data = spp_ba) 
  mCBS2 <- lmer(BA_DIV ~ SLOPE + SOLAR + EXP_MIN +(1 | TREAT), data = spp_ba) 
  anova(mCBS1, mCBS2) # mCBS1
  summary(mCBS1)
  model_parameters(mCBS1)
  
  plot_model(mCBS2, show.values = T, show.p = T, sort.est = T) 
  plot_model(mCBS1, show.values = T, show.p = T, sort.est = T)


# SITE MODEL -  deciduous basal area
  spp_ba <- read.csv(here("tree regen manuscript/spp_ba.csv"),
                     stringsAsFactors = F)
  spp_ba <- spp_ba[spp_ba$TREAT != 0,]
  spp_ba <- spp_ba[spp_ba$DIV == "d",]
  spp_ba$OL_AV <- standardize(spp_ba$OL_AV)
  spp_ba$SLOPE <- standardize(spp_ba$SLOPE)
  spp_ba$SOLAR <- standardize(spp_ba$SOLAR)
  spp_ba$EXP_MIN <- standardize(spp_ba$EXP_MIN)
  # model acronym: modeled deciduous basal area site attributes
  # started with: SLOPE + SOLAR + OL_AV + EXP_MIN + (1 + TREAT | SITE)
  # removed: SLOPE, SOLAR, EXP_MIN
  mDBS1 <- lmer(BA_DIV ~  SLOPE + OL_AV + EXP_MIN + (1 | SITE), data = spp_ba) 
  mDBS2 <- lmer(BA_DIV ~   SLOPE  + OL_AV +(1 | SITE), data = spp_ba) 
  anova(mDBS1, mDBS2) # mDBS1
  model_parameters(mDBS1)
  
  plot_model(mDBS2, show.values = T, show.p = T) 
  plot_model(mDBS1, show.values = T, show.p = T)
  summary(mDBS2) # retrieving effect size/significance