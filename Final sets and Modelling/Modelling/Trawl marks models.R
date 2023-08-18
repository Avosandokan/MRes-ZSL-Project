## TRAWL MARK MODELS WITH LENGTH AND ABUNDANCE
# Now maybe it is good to have  in the appendix 

rm(list = ls())
library(tidyverse)
library(lme4)
library(MASS)
library(gvlma)
library(PerformanceAnalytics)


###############################################################################
# ABUNDANCE
################################################################################

abundance <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")
chart.Correlation(data.frame(abundance$percalltrawling, abundance$perdeepfurrow, abundance$pershallowfurrow, abundance$perjagged))

# Shallow furrows and all trawling marks are extremely correlated (0.8), so, we can consider deep and jagged as "deep and superficial marks"

### ACANELLA
counta <- lm(data = abundance, normalise_acanella ~ percalltrawling + perdeepfurrow + perjagged)
gvlma(counta) # 2 of 5

counta_log <- lm(data = abundance, log(normalise_acanella+1) ~ percalltrawling + perdeepfurrow + perjagged)
gvlma(counta_log) # 5 of 5
summary(counta_log)

#what if you simplify
trial1 <- lm(data = abundance, log(normalise_acanella+1) ~ percalltrawling )
summary(trial1)
# it is, so we decide to actually leave out all the other type of trawling marks
gvlma(trial1)


## ASCONEMA
countaf <- lm(data = abundance, normalise_asconema ~ percalltrawling )
gvlma(countaf) # 2 of 5

countaf_log <- lm(data = abundance, log(normalise_asconema+1) ~ percalltrawling )
gvlma(countaf_log) # 4 of 5
summary(countaf_log)


# ANTHOPTILUM
countag <- lm(data = abundance, normalise_anthoptilum ~ percalltrawling )
gvlma(countag) # 2 of 5

countag_log <- lm(data = abundance, log(normalise_anthoptilum+1) ~ percalltrawling )
gvlma(countag_log) # 4 of 5
summary(countag_log) # neagtive R square not ok

countag_bn <- glm.nb(data = abundance, normalise_anthoptilum ~ percalltrawling)
par(mfrow=c(2,2))
plot(countag_bn) # not ideal.....
summary(countag_bn) # ok


### HALIPTERIS
counth <- lm(data = abundance, normalise_halipteris ~ percalltrawling )
gvlma(counth) # 1 of 5

counth_log <- lm(data = abundance, log(normalise_halipteris+1) ~ percalltrawling )
gvlma(counth_log) # 2 of 5
summary(counth_log) # very small r2
plot(counth_log) # absolutely off

counth_nb <- glm.nb(data = abundance, normalise_halipteris ~ percalltrawling )
plot(counth_nb) # even uglier
summary(counth_nb) # sort of ol

############################################################################
# MEDIAN LENGTH 
##############################################################################
rm(list = ls())
length <-  read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height_complete.csv")

aa <- length %>% filter(label_name == "Acanella arbuscula")
af <- length %>% filter(label_name == "Asconema foliatum")
ag <- length %>% filter(label_name == "Anthoptilum grandiflorum")
hf <- length %>% filter(label_name == "Halipteris finmarchica")

## ACANELLA
aa <- lm(data = aa, Length ~ percalltrawling)
gvlma(aa) # 5 of 5
summary(aa)

## ASCONEMA
aflm <- lm(data = af, Length ~ percalltrawling)
gvlma(aflm) # 5 of 5
summary(aflm) # negative r2

aflog <-lm(data = af, log(Length+1) ~ percalltrawling)
gvlma(aflog)
summary(aflog) # still negative

afnb <- glm.nb(data = af, Length ~ percalltrawling)
plot(afnb) # almost passable
summary(afnb) # ok



## ANTHOPTILUM
aglm <- lm(data = ag, Length ~ percalltrawling)
gvlma(aglm) # 3 of 5

aglog <-lm(data = ag, log(Length+1) ~ percalltrawling)
gvlma(aglog) # 5 of 5
summary(aglog) # very low r 2 



## HALIPTERIS
hflm <- lm(data = hf, Length ~ percalltrawling)
gvlma(hflm) # 5 of 5
summary(hflm) # negative r 2


hflog <- lm(data = hf, log(Length+1) ~ percalltrawling)
gvlma(hflog)
summary(hflog) # again negative r2

hfnb <- glm.nb(data = hf, Length ~ percalltrawling)
plot(hfnb) # sort of ok
summary(hfnb) #ok

plot(aa$Length ~ aa$percalltrawling)
plot(af$Length ~ af$percalltrawling)
plot(ag$Length ~ ag$percalltrawling)
plot(hf$Length ~ hf$percalltrawling)
