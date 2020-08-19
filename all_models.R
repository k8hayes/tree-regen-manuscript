#  poisson distribution 
library(tidyverse)
library(sjPlot)
library(MASS)
library(here)
# library(parameters)
# library(stats)
library(cowplot); theme_set(theme_cowplot()) 
# options(scipen = 9999)

# bringing in data
  species_dens <- read.csv(("species_dens.csv"))
  ba <- read.csv(("ba.csv"), stringsAsFactors = F)
  
  # removing unburned sites
  species_dens <- species_dens[species_dens$TREAT !=0,]
  ba <- ba[ba$TREAT != 0,]
  
# looking at format of data, first three rows
  ba[1:3,]
  species_dens[1:3,]

################################################
## conifer density
## 

#Different way of subsetting, base R  
CONIF_dens <- subset(species_dens, species_dens$DIV == "c")

#the plots are duplicated with one entry per conifer species, so this reduces it to one value per plot
cnt <- aggregate(CONIF_dens$COUNT_HA, list(CONIF_dens$PLOT), FUN = sum)  	#adds up the count per ha for each row within a plot
trt <- aggregate(CONIF_dens$TREAT, list(CONIF_dens$PLOT), FUN = mean)	#gets the treatment number (mean works because they are the same
CONIF_dens_new <- cbind(cnt, trt[,2])						#combines

temp <- merge(x=CONIF_dens_new, y=CONIF_dens, by.x="Group.1", by.y="PLOT", all.x=F, all.y=F)	#adding in the site in a clumsy way but it was quick
CONIF_dens_new <- temp[duplicated(temp[,1]),]					#merging and removing
CONIF_dens_new <- CONIF_dens_new[,1:4]						#remove extra columns

names(CONIF_dens_new) <- c("PLOT","COUNT","TREAT", "SITE")			#get names back in there
CONIF_dens <- CONIF_dens_new								#back to original name so code below works
hist(CONIF_dens$COUNT)									#check


  mC_D.nb <- 	glm.nb(COUNT ~ TREAT + SITE + (TREAT*SITE), link = log, data = CONIF_dens) 
  mC_D <- 		glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = CONIF_dens) 
  mC_D_qp <- 	glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson(link = "log"), data = CONIF_dens) 

  # residuals
    par(mfrow=c(3,4))
    plot(mC_D, main="Poisson")
    plot(mC_D_qp, main="Quasi-poisson")
    plot(mC_D.nb, main="NB")

  # parameters
    summary(mC_D); # model_parameters(mC_D)
    summary(mC_D.nb)
    summary(mC_D_qp)
 
  #note the prediction vs. observed isn't great, but that's not the point here
  plot(predict(mC_D.nb, type="response",se.fit=T)$fit,CONIF_dens$COUNT)	#plots the exponentiated prediction back
  abline(0,1, col="red")								#one to one line

  #the model doesn't do bad at predicting areas with zero trees (treatment effect) are zero, it's the variability
  #in the regen where there is regen (treatments 1 and 2 primarily) that must be explained by other factors.
  #That's clear from this prediction plot and where the major errors are, but also in the standard errors.

  plot(predict(mC_D.nb, type="response",se.fit=T)$fit, predict(mC_D.nb, type="response",se.fit=T)$se.fit, ylab="SE", xlab="Prediction")
  abline(0,1, col="red")
  
  #Now, you could start bringing in other covariates, but that's generally not been your aim with this particular
  #investigation, correct?

###################################################  stopped here

##
## deciduous density
##

  #Different way of subsetting, base R  
  DECID_dens <- subset(species_dens, species_dens$DIV == "d")
  
  #the plots are duplicated with one entry per conifer species, so this reduces it to one value per plot
  cnt <- aggregate(DECID_dens$COUNT_HA, list(DECID_dens$PLOT), FUN = sum)  	#adds up the count per ha for each row within a plot
  trt <- aggregate(DECID_dens$TREAT, list(DECID_dens$PLOT), FUN = mean)	#gets the treatment number (mean works because they are the same
  DECID_dens_new <- cbind(cnt, trt[,2])						#combines
  
  temp <- merge(x=DECID_dens_new, y=DECID_dens, by.x="Group.1", by.y="PLOT", all.x=F, all.y=F)	#adding in the site in a clumsy way but it was quick
  DECID_dens_new <- temp[duplicated(temp[,1]),]					#merging and removing
  DECID_dens_new <- DECID_dens_new[,1:4]						#remove extra columns
  
  names(DECID_dens_new) <- c("PLOT","COUNT","TREAT", "SITE")			#get names back in there
  DECID_dens <- DECID_dens_new								#back to original name so code below works
  hist(DECID_dens$COUNT)									#check
  
  mD_D.nb <- 	glm.nb(COUNT ~ TREAT + SITE + (TREAT*SITE), link = log, data = DECID_dens) 
  mD_D <- 		glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = DECID_dens) 
  mD_D_qp <- 	glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson(link = "log"), data = DECID_dens) 
  
  # residuals
  par(mfrow=c(3,4))
  plot(mD_D, main="Poisson")
  plot(mD_D_qp, main="Quasi-poisson")
  plot(mD_D.nb, main="NB")
  
  # parameters
  summary(mD_D); # model_parameters(mD_D)
  summary(mD_D.nb)
  summary(mD_D_qp)

  
  plot(predict(mD_D.nb, type="response",se.fit=T)$fit,DECID_dens$COUNT)	#plots the exponentiated prediction back
  abline(0,1, col="red")								#one to one line
  
  #the model doesn't do bad at predicting areas with zero trees (treatment effect) are zero, it's the variability
  #in the regen where there is regen (treatments 1 and 2 primarily) that must be explained by other factors.
  #That's clear from this prediction plot and where the major errors are, but also in the standard errors.
  
  plot(predict(mD_D.nb, type="response",se.fit=T)$fit, 
       predict(mD_D.nb, type="response",se.fit=T)$se.fit, ylab="SE", xlab="Prediction")
  abline(0,1, col="red")
    
##   
## conifer basal area
##
    CONIF_BA <- ba %>% filter(ba$DIV == "c")
    CONIF_BA <- subset(ba, ba$DIV == "c")	


mC_BA <- glm(log(BA) ~ TREAT + SITE + (TREAT*SITE), data = CONIF_BA) 
  # residuals
    plot(mC_BA)
  # parameters
    summary(mC_BA); #model_parameters(mC_BA)
  # plots
    # plot_model(mC_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    # plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values




# quasi
    mC_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson(link = "log"), data = CONIF_BA) 
    # residuals
    plot(mC_BA)
    # parameters
    summary(mC_BA); #model_parameters(mC_BA)
    # plots
    # plot_model(mC_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    # plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values

##    
## deciduous basal area
##     
    DECID_BA <- ba %>%
      filter(DIV == "d")
    mD_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = DECID_BA) 
  # residuals
    plot(mD_BA)
  # parameters
    summary(mD_BA); # model_parameters(mD_BA)
  # plots
    # plot_model(mD_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    # plot_model(mD_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
    
# quasi 
    mD_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson(link = "log"), data = ba) 
    # residuals
    plot(mD_BA)
    # parameters
    summary(mD_BA); # model_parameters(mD_BA)
    # plots
    # plot_model(mD_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    # plot_model(mD_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
