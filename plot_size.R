# fixing plot size
library(here)
library(tidyverse)

dalton <- read.csv(head("data/Dalton_DBH.csv"))

test <- dalton %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(mean = mean(QUAD))

dalton$QUAD[dalton$PLOT == "32_2"] <- 1
dalton$QUAD[dalton$PLOT == "32_9"] <- 1
dalton$QUAD[dalton$PLOT == "48_1"] <- 1
dalton$QUAD[dalton$PLOT == "56_2"] <- 1
dalton$QUAD[dalton$PLOT == "7_3"] <- 0.2
dalton$QUAD[dalton$PLOT == "12_1"] <- 0.2
dalton$QUAD[dalton$PLOT == "50_1"] <- 0.2

write.csv(dalton, "data/Dalton_DBH.csv", row.names = F)

steese <- read.csv(head("data/Steese_DBH.csv"))

test <- steese %>%
  group_by(SITE, TREAT, PLOT) %>%
  summarise(mean = mean(QUAD))

steese$QUAD[steese$PLOT == "34_2"] <- 1

write.csv(steese, "data/Steese_DBH.csv", row.names = F)
