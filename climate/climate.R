# climate variables for reburns manuscript
# mean, sd, max and min 
# 03/23/20

# winter - oct through march
# summer - june through august

# climate stations:
  # preacher creek - steese
  # seven mile - dalton

library(tidyverse)
library(here)

# seven mile
upland <- read.csv(here( "/climate/up_seven_airtemp.csv"), stringsAsFactors = F)

# winter
win_up <- upland %>%
  na.omit() %>%
  filter(Month == "09/" | Month == "10/" | Month == "11/"|
         Month == "12/"| Month == "01/" | Month == "02/"| 
           Month == "03/") 
  summary(win_up$Ave)
  sd(win_up$Ave)

# summer
sum_up <- upland %>%
  na.omit() %>%
  filter(Month == "04/" | Month == "05/" | Month == "06/"|
           Month == "07/") 
  summary(sum_up$Ave)
  sd(sum_up$Ave)
  
# lowland
lowland <- read.csv(here("tree regen manuscript", 
                         "/climate/low_preacher_airtemp.csv"), stringsAsFactors = F)
# winter
win_low <- lowland %>%
  na.omit() %>%
  filter(Month == "09/" | Month == "10/" | Month == "11/"|
           Month == "12/"| Month == "01/" | Month == "02/"| 
           Month == "03/") 
summary(win_low$Ave)
sd(win_low$Ave)

# summer
sum_low <- lowland %>%
  na.omit() %>%
  filter(Month == "04/" | Month == "05/" | Month == "06/"|
           Month == "07/") 
summary(sum_low$Ave)
sd(sum_low$Ave)
cli