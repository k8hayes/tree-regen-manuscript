#useful helper package
library(doBy)

#replicable
set.seed(100)

#data frame - note that because we are setting the values we know the exact SE
dd <- data.frame(
  group = c(rep("group1", 100),rep("group2", 100)),
  values = c(rpois(n=100, lambda=2), rpois(n=100, lambda=7))
)

#quick calculation
logmean <- function(x){
  log(mean(x))
}

#quick summary stats for comparsion
ss <- summaryBy(values ~ group, data=dd,
                FUN=c(length, logmean, mean, var, sd)
)

#calculating SE normal way (note incorrect given this is a Poisson generated process)
ss$SE.normal <- sqrt(ss$values.var/(ss$values.length-1))

#calculate SE for a poisson process
ss$sd.Poiss <- sqrt(ss$values.mean)
ss$se.Poiss <- sqrt(ss$values.mean/ss$values.length)

#organize
ss <- cbind(ss[,"group"], round(ss[,-1],3))
names(ss) <- c("group", "N", "log.mean", "mean", "variance", "sd", "SE","sd.Poiss","se.Poiss")


#now, pretend we have a model built from that data (so it's predicting the log mean (not the mean of the logs)
#note that it's only pulling group ID and values

glm1 <- glm(values ~ group, data=dd, family="poisson"(link = "log"))

#OK, so this is like your data
summary(glm1)

#now predict new data, but the response, which will back transform
preddata <- data.frame(group=unique(dd$group))
preds <- predict(glm1, newdata=preddata, type="response", se.fit=T)

#note the evidence it's back transformed proper
preds$fit; ss$mean

#and the se is calculated back OK
preds$se.fit; ss$se.Poiss

# Confidence interval (95)
fit <- preds$fit
lwr <- fit - 1.96*preds$se.fit
upr <- fit + 1.96*preds$se.fit


#so you can do the same thing w/ predicting your distributions. Do note to calculate the SD of your 
#test distribution (if you do it this way) properly.