# LMEs - basal area
#library(lme4)
library(sjPlot) 
#library(lattice)
#library(nlme)
library(here)
#library(effectsize)
library(parameters)
library(ggplot2) 
library(cowplot) 
#library(tidyverse)
theme_set(theme_cowplot()) 
options(scipen = 9999)

ba <- read.csv(here("ba.csv"), stringsAsFactors = F)
ba <- ba[ba$TREAT != 0,]

# MODEL -  conifer basal area
  mC_BA <- lm(CONIF_BA ~ TREAT + SITE, data = ba)
  summary(mC_BA)
  model_parameters(mC_BA)
  
  plot_model(mC_BA, show.values = T, show.p = T, sort.est = T)
  plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # like as a visual # but 1.5 fires is not a real thing

# MODEL - deciduous basal area
  mD_BA <- lm(DECID_BA ~ TREAT + SITE, data = ba)
  summary(mD_BA)
  model_parameters(mD_BA)
  
  plot_model(mD_BA, show.values = T, show.p = T, sort.est = T)
  plot_model(mD_BA, type = "pred", terms = c("TREAT", "SITE"))


  
