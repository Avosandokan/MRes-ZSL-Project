### Calculate swept area of swept area

rm(list = ls()) # clear workspace
library(tidyverse)

calcs <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Trawled area calcs.csv")


calcs$area_all <- NA
calcs$area_deep <- NA
calcs$area_shallow <- NA
calcs$area_jag <- NA
calcs$area_turned <- NA

calcs$area_all <- calcs$percalltrawling*calcs$swept_area_in_vid_m2/100
calcs$area_deep <- calcs$perdeepfurrow*calcs$swept_area_in_vid_m2/100
calcs$area_shallow <- calcs$pershallowfurrow*calcs$swept_area_in_vid_m2/100
calcs$area_jag <- calcs$perjagged*calcs$swept_area_in_vid_m2/100
calcs$area_turned <- calcs$peroverturned*calcs$swept_area_in_vid_m2/100

sum(calcs$area_all)
# 2453.774 of the seabed had trawling marks
sum(calcs$area_deep)
# 320.6505 of the seabed had deep furrows
sum(calcs$area_shallow)
#  1545.698 of it had shallow furrows
sum(calcs$area_jag)
# 489.3003 m2
sum(calcs$area_turned)
# 373.8876 m2 of turned rocks
notrawlling <- calcs %>% filter(percalltrawling > 0)


# find the trawling mark that has been most persistent (ye stayed the longest on when untrawled)
plot(calcs$percalltrawling~calcs$years_since_trawled) #between 12 and 15 years
plot(calcs$perdeepfurrow~calcs$years_since_trawled) # over 15 years
plot(calcs$pershallowfurrow~calcs$years_since_trawled) #between 12 and 15 years
plot(calcs$perjagged~calcs$years_since_trawled) # after 7 years ish
plot(calcs$peroverturned~calcs$years_since_trawled) # dont

d<-data.frame(calcs$video_filename, calcs$percalltrawling, calcs$years_since_trawled)
order(d)
??sort
