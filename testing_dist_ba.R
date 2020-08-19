#  tesing distributions: basal area
library(tidyverse)
library(sjPlot)
library(MASS)
library(here)
# library(parameters)
# library(stats)
library(cowplot); theme_set(theme_cowplot()) 
# options(scipen = 9999)

# bringing in data
ba <- read.csv(("ba.csv"), stringsAsFactors = F)

  # removing unburned sites
  ba <- ba[ba$TREAT != 0,]
  
  # looking at format of data, first three rows
  ba[1:3,]

#####################################################   
## conifer basal area
##
    
    CONIF_BA <- subset(ba, ba$DIV == "c")	
  
    mC_BA <- lm(log(BA) ~ TREAT + SITE + (TREAT*SITE), data = CONIF_BA) 
    # residuals
    plot(mC_BA)
    # parameters
    summary(mC_BA); #model_parameters(mC_BA)
    # plots
    # plot_model(mC_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    # plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
    
#################################################    
## deciduous basal area
##     

    #Different way of subsetting, base R  
    DECID_BA <- subset(ba, ba$DIV == "d")
    
    #the plots are duplicated with one entry per conifer species, so this reduces it to one value per plot
    cnt <- aggregate(DECID_BA$BA, list(DECID_BA$PLOT), FUN = sum)  	#adds up the count per ha for each row within a plot
    trt <- aggregate(DECID_BA$TREAT, list(DECID_BA$PLOT), FUN = mean)	#gets the treatment number (mean works because they are the same
    DECID_BA_new <- cbind(cnt, trt[,2])						#combines
    
    temp <- merge(x=DECID_BA_new, y=DECID_BA, by.x="Group.1", by.y="PLOT", all.x=F, all.y=F)	#adding in the site in a clumsy way but it was quick
    DECID_BA_new <- temp[duplicated(temp[,1]),]					#merging and removing
    DECID_BA_new <- DECID_BA_new[,1:4]						#remove extra columns
    
    names(DECID_BA_new) <- c("PLOT","BA","TREAT", "SITE")			#get names back in there
    DECID_BA <- DECID_BA_new								#back to original name so code below works
    hist(DECID_BA$BA)			
  
    mD_BA <- lm(log(BA) ~ TREAT + SITE + (TREAT*SITE), data = DECID_BA) 
  # residuals
  plot(mD_BA)
  # parameters
  summary(mD_BA); # model_parameters(mD_BA)
  
