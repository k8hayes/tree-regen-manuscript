#  testing distributions
library(tidyverse)
library(sjPlot)
library(MASS)
library(here)
library(parameters)
library(cowplot); theme_set(theme_cowplot()) 


################################################
##  density

# bringing in data
species_dens <- read.csv(("species_dens.csv"))

# removing unburned sites
species_dens <- species_dens[species_dens$TREAT !=0,]

# looking at format of data, first three rows
species_dens[1:3,]

#############################
## conifer density

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
  mC_D    <- 	glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = CONIF_dens) 
  mC_D_qp <- 	glm(COUNT ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson(link = "log"), data = CONIF_dens) 
  
  # residuals
  par(mfrow=c(3,4))
  plot(mC_D, main="Poisson")
  plot(mC_D_qp, main="Quasi-poisson")
  plot(mC_D.nb, main="NB")
  
  # parameters
  summary(mC_D)
  summary(mC_D_qp)
  summary(mC_D.nb)
  
  model_parameters(mC_D.nb)
  
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

  #to do so, you would add them into the glm above, but then check the diagnostics again of course

######################################
## deciduous density
##

  # Different way of subsetting, base R  
  DECID_dens <- subset(species_dens, species_dens$DIV == "d")

  # the plots are duplicated with one entry per species, so this reduces it to one value per plot
  cnt <- aggregate(DECID_dens$COUNT_HA, list(DECID_dens$PLOT), FUN = sum)  	#adds up the count per ha for each row within a plot
  trt <- aggregate(DECID_dens$TREAT, list(DECID_dens$PLOT), FUN = mean)		#gets the treatment number (mean works because they are the same
  DECID_dens_new <- cbind(cnt, trt[,2])						#combines
  
  temp <- merge(x=DECID_dens_new, y=DECID_dens, by.x="Group.1", by.y="PLOT", all.x=F, all.y=F)	#adding in the site in a clumsy way but it was quick
  temp2 <- temp[duplicated(temp[,1]),]										#merging and removing
  temp3 <- temp2[duplicated(temp2[,1]),]        # have to loop through, since there's so many more species
  temp4 <- temp3[duplicated(temp3[,1]),]        # absolutely sure there's a faster way to do this, just wanted to get it done
  temp5 <- temp4[duplicated(temp4[,1]),]
  DECID_dens_new <- temp5[duplicated(temp5[,1]),]
  DECID_dens_new <- DECID_dens_new[,1:4]						#remove extra columns
  rm(temp,temp2, temp3, temp4, temp5, cnt, trt) 				# cleaning up workspace
  
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
  summary(mD_D)
  summary(mD_D_qp)
  summary(mD_D.nb)
  
  model_parameters(mD_D.nb)
  
  dev.off()
  plot(predict(mD_D.nb, type="response",se.fit=T)$fit,DECID_dens$COUNT)	#plots the exponentiated prediction back
  abline(0,1, col="red")								#one to one line
  
  plot(predict(mD_D.nb, type="response",se.fit=T)$fit, 
       predict(mD_D.nb, type="response",se.fit=T)$se.fit, ylab="SE", xlab="Prediction")






########################################### 
## basal area
  
  # bringing in data
  ba <- read.csv(("ba.csv"), stringsAsFactors = F)
 
  # removing unburned sites
  ba <- ba[ba$TREAT != 0,]
  
  # looking at format of data, first three rows
  ba[1:3,]
  ba$BA_ha <- ba$BA * 50				#why 50?  10,000 m2 / plot size


############################  
## conifer basal area
##

	#again, just my one line at a time rather than tidyverse
  CONIF_BA <- subset(ba, ba$DIV == "c")
  CONIF_BA  			#immediatley note there isn't a lot of basal area to work 
 					#with re: conifers.  I wouldn't do anything with this directly.



  CONIF_BA <- ba %>%
    filter(DIV == "c") %>%
    group_by(SITE, TREAT, PLOT) %>%
    summarise(BA = sum(BA_ha)) %>%
    ungroup() %>%
    complete(SITE, TREAT, PLOT, fill = list(BA = 0))
  hist(CONIF_BA$BA)
  CONIF_BA[CONIF_BA$BA > 100,] # checking high value
  
  mC_BA.lm <- lm(BA ~ TREAT + SITE + (TREAT*SITE), data = CONIF_BA) 
  mC_BA.log <- lm(log(BA + 1) ~ TREAT + SITE + (TREAT*SITE), data = CONIF_BA) 
  mC_BA.ga <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = gaussian(link = "identity"), data = CONIF_BA)
  
  # residuals
  par(mfrow=c(3,4))
  plot(mC_BA.lm, main = "Linear")
  plot(mC_BA.log, main = "logged")
  plot(mC_BA.ga, main = "Gaussian")
  
  # parameters
  summary(mC_BA.lm); #model_parameters(mC_BA)
  summary(mC_BA.log)
  summary(mC_BA.ga)

  
########################  
## deciduous basal area
  
#  DECID_BA <- ba %>%
#    filter(DIV == "d") %>%
#    group_by(SITE, TREAT, PLOT) %>%
#    summarise(BA = sum(BA_ha)) %>%
#    ungroup() %>%
#    complete(SITE, TREAT, PLOT, fill = list(BA = 0))
#  hist(DECID_BA$BA)

  #
  DECID_BA <- subset(ba, ba$DIV == "d")

  #expansion factor. Make sure this is correct if you stick with this value.  
  #remember the expansion factor is the same as the inverse of the plot size relative to a ha,
  #so a 400m2 plot is 1/25 of a ha, so the expansion factor is 25.  

  DECID_BA$QUAD[DECID_BA$QUAD == 2] <- 50	#assuming a plot is 1/25 of a ha, so half a plot is 1/50 of a ha.
  DECID_BA$QUAD[DECID_BA$QUAD == 1] <- 100
  DECID_BA$QUAD[DECID_BA$QUAD == .1] <- 1000
  DECID_BA$QUAD[DECID_BA$QUAD == .2] <- 500

 
  #calc basal area
  DECID_BA$BA <- (pi * (DECID_BA$DBH/2)^2)/10000	#convert to ba and cm2 to m2. Just pi*r2
  DECID_BA$BA_ha <- DECID_BA$BA * DECID_BA$QUAD		#multiply by expansion factor 


  #a single line, does the same thing (calculates basal area and scales by expansion factor):
  #DECID_BA$BA_ha <- DECID_BA$DBH^2*0.00007854*DECID_BA$QUAD  #note this does the same thing, just simplified algebraically

  ba.plot <- aggregate(DECID_BA$BA_ha, list(DECID_BA$PLOT), FUN = sum)  	#adds up the count per ha for each row within a plot
  ba.trt <- aggregate(DECID_BA$TREAT, list(DECID_BA$PLOT), FUN = mean)		#gets the treatment number (mean works because they are the same

  DECID_ba_new <- cbind(ba.plot, ba.trt[,2])						#combines
  names(DECID_ba_new) <- c("PLOT","BA","TREAT")

  ba.site <- cbind(DECID_BA$SITE,DECID_BA$PLOT)
  temp <- as.data.frame(ba.site[!duplicated(ba.site[,2]),])
  names(temp) <- c("SITE","PLOT")

  DECID_BA <- merge(DECID_ba_new,temp,by="PLOT")	#double check all is well with the rows - right site/treatment/value
  
  #histogram
  hist(DECID_BA$BA)
  hist(log(DECID_BA$BA))	#log transforming
 
  mD_BA.log <- lm(log(BA)  ~ TREAT + SITE + (TREAT*SITE), data = DECID_BA)						#models E[log(y)]
  mD_BA.ga <- glm(BA  ~ TREAT + SITE + (TREAT*SITE), family = gaussian(link = "log"), data = DECID_BA)	#models log[E(y)]
  mD_BA.gamma <- glm(BA  ~ TREAT + SITE + (TREAT*SITE), family = Gamma(link = "log"), data = DECID_BA)
  
  library(logNormReg)  #another option, look up lognormal distributions
  BA_logn <- lognlm((BA)  ~ TREAT + SITE + (TREAT*SITE), data = DECID_BA)


  par(mfrow=c(3,4))
  plot(mD_BA.log, main= "Logged")
  plot(mD_BA.ga, main="Guassian")
  plot(mD_BA.gamma, main="Gamma")


  #choosing a distribution is often best done with an understanding of the relationship between variance and mean
  #Gaussian has constant variance; gamma and lognormal do not.
  #heteroskedastic
  par(mfrow=c(1,4))
  plot((DECID_BA$BA),residuals(mD_BA.log),main="log"); abline(h=0)
  plot((DECID_BA$BA),residuals(mD_BA.ga), main="Gaussian"); abline(h=0)	#pretty clear Gaussian is not great.
  plot((DECID_BA$BA),residuals(mD_BA.gamma), main="Gamma"); abline(h=0)
  plot((DECID_BA$BA),residuals(BA_logn), main="Lognormal"); abline(h=0)

  # parameters
  summary(mD_BA.log)
  summary(mD_BA.ga)
  summary(mD_BA.gamma)
  summary(BA_logn)

  #Compare root mean squared error
  sqrt(mean(mD_BA.log$residuals^2))
  sqrt(mean(mD_BA.ga$residuals^2))
  sqrt(mean(mD_BA.gamma$residuals^2))
  sqrt(mean(BA_logn$residuals^2))



  par(mfrow=c(1,4)	)
  plot(exp(predict(mD_BA.log)),DECID_BA$BA, main="logged LM")	#plots the exponentiated prediction back
  abline(0,1)
  plot((predict(mD_BA.ga, type="response")),DECID_BA$BA, main="Guassian")	#plots the exponentiated prediction back
  abline(0,1)
  plot(predict(mD_BA.gamma, type="response"),DECID_BA$BA, main="Gamma with log link")	#plots the exponentiated prediction back
  abline(0,1)
  plot(BA_logn$fitted.values,DECID_BA$BA, main = "lognormal")	#plots the exponentiated prediction back
  abline(0,1)

  




###  For more gamma 
#Gea-Izquierdo, G. and Canellas, I., 2009. Analysis of holm oak intraspecific competition using Gamma regression. Forest Science, 55(4), pp.310-322.




