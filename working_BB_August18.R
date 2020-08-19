#  poisson distribution 
library(tidyverse)
library(sjPlot)
library(MASS)
library(here)
library(pscl)
# library(parameters)
# library(stats)
library(cowplot); theme_set(theme_cowplot()) 
# options(scipen = 9999)




#Hey Kate, I have perhaps been a bit more hands off than I should have 
#been in this regard. Sorry about that, if it was a problem.  
#Just let me know when you've adjusted the rest of this code
#and how it went and we can go over it again.



# bringing in data
  species_dens <- read.csv(("species_dens.csv")); species_dens <- species_dens[species_dens$TREAT !=0,] # removing unburned sites
  ba <- read.csv(("ba.csv"), stringsAsFactors = F); ba <- ba[ba$TREAT != 0,]

#looking at format of data, first three rows
ba[1:3,]
species_dens[1:3,]


##
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


  # plots

    #BB:  I don't know this package used here (plot_model) so make sure you know how it's deriving what it's plotting

    plot_model(mC_D.nb, show.values = T, show.p = T) # effect sizes
    plot_model(mC_D.nb, type = "pred", terms = c("TREAT", "SITE")) # predicted values
 


  #note the prediction vs. observed isn't great, but that's not the point here
  plot(predict(mC_D.nb, type="response",se.fit=T)$fit,CONIF_dens$COUNT)	#plots the exponentiated prediction back
  abline(0,1, col="red")								#one to one line

  #the model doesn't do bad at predicting areas with zero trees (treatment effect) are zero, it's the variability
  #in the regen where there is regen (treatments 1 and 2 primarily) that must be explained by other factors.
  #That's clear from this prediction plot and where the major errors are, but also in the standard errors.

  plot(predict(mC_D.nb, type="response",se.fit=T)$fit, predict(mC_D.nb, type="response",se.fit=T)$se.fit, ylab="SE", xlab="Prediction")

  #Now, you could start bringing in other covariates, but that's generally not been your aim with this particular
  #investigation, correct?

###################################################  stopped here





##
## deciduous density
##
    DECID_dens <- species_dens %>%
      filter(DIV == "d")



  mD_D <- glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = DECID_dens) 
  # residuals
   plot(mD_D)
  # parameters
   summary(mD_D); # model_parameters(mD_D)
  # plots 
    # plot_model(mD_D, show.values = T, show.p = T) # effect sizes
    # plot_model(mD_D, type = "pred", terms = c("TREAT", "SITE")) # predicted values

# quasi poisson
    mD_D <- glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson(link = "log"), data = DECID_dens)
    # residuals
    plot(mD_D)
    # parameters
    summary(mD_D); # model_parameters(mD_D)
    # plots 
    # plot_model(mD_D, show.values = T, show.p = T) # effect sizes
    # plot_model(mD_D, type = "pred", terms = c("TREAT", "SITE"))  
    
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
