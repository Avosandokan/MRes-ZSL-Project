# Frames assesment and plots
# To assess the presence of different trawling scars on the seafloor, run models
# Also make a nice Plot with the four different types and the most recurringly significant explanatory variable

rm(list=ls()) #clear workspace 
d <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv")
d <- d[,-1]

# visual inspection of the data
hist(d$percalltrawling)
hist(d$pershallowfurrow)
hist(d$perdeepfurrow)
hist(d$perjagged)
hist(d$peroverturned)
#none of the is well distributed, all are heavily zero inflated. Might be a problem when modelling


##################################################################
#MODELLING
##################################################################
# In models we include depth, effort, and years since last trawled
library(lmerTest)
library(Matrix)



#ALL TRAWLING
mall <- lm(data = d, percalltrawling ~ mean_depth  + years_since_trawled)
summary(mall) # 46 r2
par(mfrow = c(2, 2))
plot(mall) # scale location is off
library(gvlma)
gvlma(mall) #2/5

#LOG+1
malllog <- lm(data = d, log(percalltrawling+1) ~ mean_depth  + years_since_trawled)
summary(malllog) # R2 a bit higher 
plot(malllog) # residual fitted off
gvlma(malllog)# 2 of 5

#Poisson
mallp <- glm(data = d, percalltrawling ~ mean_depth + years_since_trawled, family = poisson(link = "log"))
summary(mallp)
# residual dev versus df is off

#NB
library(MASS)
mallnb <- glm.nb(data = d, percalltrawling ~ mean_depth + years_since_trawled)
summary(mallp) #residual deviance still off                 


#### DEEP FURROW 
deep <- lm(data = d, perdeepfurrow ~ mean_depth  + years_since_trawled)
gvlma(deep) #1/5

#Log
deeplog <- lm(data = d, log(perdeepfurrow+1) ~ mean_depth  + years_since_trawled)
gvlma(deeplog) #1/5

#poisson
deeppoi <- glm(data = d, perdeepfurrow ~ mean_depth + years_since_trawled, family = poisson(link = "log"))
summary(deeppoi) #res vs deviacne is off

#negative
deepnb <- glm.nb(data = d, perdeepfurrow ~ mean_depth + years_since_trawled)
summary(deepnb) #Res deviance is ok
par(mfrow = c(2, 2))
plot(deepnb) #very nice
paste(round(with(deepnb, deviance/df.residual),2),
      ifelse((with(deepnb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) 
#Not overdispersed


#### SHALLOW FURROW
shallow <- lm(data = d, pershallowfurrow ~ mean_depth  + years_since_trawled)
gvlma(shallow) #1/5

#log
shallowlog <- lm(data = d, log(pershallowfurrow+1) ~ mean_depth  + years_since_trawled)
gvlma(shallowlog) #2/5

#poi
shallowpoi <- glm(data = d, pershallowfurrow ~ mean_depth  + years_since_trawled, family = poisson(link = "log"))
summary(shallowpoi) # res vs Df is off

#nb
shallownb <- glm.nb(data = d, pershallowfurrow ~ mean_depth  + years_since_trawled)
summary(shallownb) # res deviance vs DF is ok
plot(shallownb) #ok
paste(round(with(shallownb, deviance/df.residual),2),
      ifelse((with(shallownb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) 
# not overdispersed
# THIS IS THE BEST MODEL



#### JAGGED
jagged <- lm(data = d, perjagged~ mean_depth  + years_since_trawled)
gvlma(jagged) #1/5

#log
jaggelog<- lm(data = d, log(perjagged+1) ~ mean_depth  + years_since_trawled)
gvlma(jaggelog) #1/5

#poisson
jaggedpoi <- glm(data = d, perjagged~ mean_depth  + years_since_trawled, family = poisson(link = "log"))
summary(jaggedpoi) # res deviance vs Df is off

#NB
jaggednb <- glm.nb(data = d, perjagged~ mean_depth  + years_since_trawled)
summary(jaggednb) #res deviance vs df is ok, slighlty overfitting
paste(round(with(jaggednb, deviance/df.residual),2),
      ifelse((with(jaggednb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) 
#not overdispersed

#THIS IS THE BEST MODEL



#### OVERTURNED
over <- lm(data = d, peroverturned ~ mean_depth  + years_since_trawled)
summary(over)
gvlma(over) #1/5

#log
overlog <- lm(data = d, log(peroverturned+1) ~ mean_depth  + years_since_trawled)
gvlma(overlog) # none

#poisson
overpoi <- glm(data = d, peroverturned ~ mean_depth  + years_since_trawled, family = poisson(link = "log"))
summary(overpoi) # res dev vs Df is off

#Nb
overnb <- glm.nb(data = d, peroverturned ~ mean_depth  + years_since_trawled)
summary(overnb) # res dev vs DF is ok
plot(overnb) # qq residuals slighlty off but rest is perfect
paste(round(with(overnb, deviance/df.residual),2),
      ifelse((with(overnb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
# Not overdispersed



#####################################################################
#  DATA SET WITHOUT ZEROS! NO MODELS FIT OTHERWISE
#####################################################################
# create a dataset where the total effort is different from zero -this will get rid of a lot of data but keep the case where there is trawling effort without visual scars
#library(tidyverse)
#dbis <- d %>% filter(total_effort!=0)

##ALL TRAWLING
#mall1 <- lm(data = dbis, percalltrawling ~ mean_depth + total_effort + years_since_trawled)
#summary(mall1)
#R2 at 35% - only effort significant
#plot(mall1) # scale location is off
#gvlma(mall1) # 5/5 acceptable

# LOG 
#mall1log <- lm(data = dbis, log(percalltrawling+1) ~ mean_depth + total_effort + years_since_trawled)
#summary(mall1log)
#R2 at 19% - nothing significant
#plot(mall1log) # scale location is off
#gvlma(mall1log) #everything ok but because R2 is higher in previous we keep that, no need to test more complex models 
#total effort almost significant. Lets take away years since last trawled (highli correlated)

#Simplify
#mall1log2 <- lm(data = dbis, log(percalltrawling+1) ~ mean_depth + total_effort)
#summary(mall1log2) # R2 at 21% - total effort significant
#gvlma(mall1log2) #5/5 
# >>>> BEST MODEL FOR ALL TRAWLING EVIDENCE


## DEEP FURROW
#mdp <- lm(data = dbis, perdeepfurrow ~ mean_depth + total_effort + years_since_trawled)
#summary(mdp) #negative Rsquared complitely not OK
#gvlma(mdp) 1/5

#log trasnforming
#mdplog <-lm(data = dbis, log(perdeepfurrow+1) ~ mean_depth + total_effort + years_since_trawled)
#summary(mdplog) # still a negative R2 (NOt ok !) - nothing significant 
#gvlma(mdplog) (5/5)

#poisson
#mdppoi <- glm(data = dbis, perdeepfurrow ~ mean_depth + total_effort + years_since_trawled, family = poisson(link = "log"))
#summary(mdppoi) # residual deviance vs DF is off / depth and yearssince significant
#plot(mdppoi) # Assumptions are Ok!

#negative binomial
#mdnb <- glm.nb(data = dbis, perdeepfurrow ~ mean_depth + total_effort + years_since_trawled)
#summary(mdnb) #Residual deviance vs Df is OK! - nothing significant
#plot(mdnb) #assumptions are ok
#paste(round(with(mdnb, deviance/df.residual),2),
 #     ifelse((with(mdnb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
# NEGATIVEBINOMIAL is best way to go - not overdispersed



### SHALLOW FURROW
#msf <- lm(data = dbis, pershallowfurrow ~ mean_depth + total_effort + years_since_trawled)
#summary(msf) # R2 11% - nothing significant 
#plot(msf) # look ok
#gvlma(msf) #4/5

#Log
#msflog <- lm(data = dbis, log(pershallowfurrow+1) ~ mean_depth + total_effort + years_since_trawled)
#summary(msflog) # R2 12% - effort almost significant
#plot(msflog) # not too pretty
#gvlma(msflog) #5/5
#SIMPLIFY
#msflog2 <- lm(data = dbis, log(pershallowfurrow+1) ~ mean_depth + total_effort)
#summary(msflog2) # R2 16 - effort significant
#plot(msflog2) # not too pretty
#gvlma(msflog2) #5/5
# also tried simplifying with years only but still not sign
#BEST MODEL 

## JAGGED GROOVES
#mjg <- lm(data = dbis, perjagged ~ mean_depth + total_effort + years_since_trawled)
#summary(mjg) # r2 at 19% - effort significant
#plot(mjg) # looks so and so
#gvlma(mjg) #1/5 

#log
#mjglog <- lm(data = dbis, log(perjagged+1) ~ mean_depth + total_effort + years_since_trawled)
#summary(mjglog) # R2 at 37% - effort significant 
#plot(mjglog) #scale location is off
#gvlma(mjglog)# 2/5

#Simplify 
#mjglog2 <- lm(data = dbis, log(perjagged+1) ~ mean_depth + total_effort)
#summary(mjglog2) # R2 at 40%
#plot(mjglog2) # same
#gvlma(mjglog2) # same

##poisson
#mjpoi <- glm(data = dbis, perjagged ~ mean_depth + total_effort + years_since_trawled, family = poisson(link = "log"))
#summary(mjpoi) #Deviance Vs DF are off - AIC off the roof throw in the bin this model

#Binomial
#mjnb <- glm.nb(data = dbis, perjagged ~ mean_depth + total_effort + years_since_trawled)
#summary(mjnb) # residual deviance OK
#AIC 108
#paste(round(with(mjnb, deviance/df.residual),2),
   #   ifelse((with(mjnb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
# not overdispersed

#simplify 
#mjnb2 <- glm.nb(data = dbis, perjagged ~ mean_depth + total_effort)
#summary(mjnb2) # significance still the same
#mjnb3 <- glm.nb(data = dbis, perjagged ~ mean_depth + years_since_trawled)
#summary(mjnb3) # neither significant
# FULL BINOMIAL IS THE BEST MODEL


### OVERTURNED ROCKS
#mo <- lm(data = dbis, peroverturned  ~ mean_depth + total_effort + years_since_trawled)
#summary(mo) #negative r2 - nothing significant 
#plot(mo) # looks good
#gvlma(mo) #only 1/5

#log
#molog <- lm(data = dbis, log(peroverturned+1)  ~ mean_depth + total_effort + years_since_trawled)
#summary(molog) # R2 is at 15% now, years almost significant
#plot(molog) # looks ok
#gvlma(molog) 2/5
#Simplify
##molog2 <- lm(data = dbis, log(peroverturned+1)  ~ mean_depth + years_since_trawled)
#summary(molog2) #r2 at 18% - years is signifcant
#plot(molog2)
#gvlma(molog2) # still 2/5

#poisson
#mopoi <- glm(data = dbis, peroverturned  ~ mean_depth + total_effort + years_since_trawled, family = poisson(link = "log"))
#summary(mopoi) # absolutely off!!!!
#paste(round(with(mopoi, deviance/df.residual),2),
 #     ifelse((with(mopoi, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#overdispersed

#nb
#monb <- glm.nb(data = dbis, peroverturned  ~ mean_depth + total_effort + years_since_trawled)
#summary(monb) # Res deviance is Ok - years is significant
#paste(round(with(monb, deviance/df.residual),2),
#      ifelse((with(monb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#not overdispersed
#BEST Model


##########################################################################
# EFFORT INSTEAD OF YEARS SINCE TRAWLED
##########################################################################


#### ALL TRAWLING - log
alleg <-lm(data = d, percalltrawling ~ mean_depth  + total_effort)
gvlma(alleg) #none


all_log <- lm(data = d, log(percalltrawling+1) ~ mean_depth  + total_effort)
gvlma(all_log) # none

all_nb <- glm.nb(data = d, percalltrawling ~ mean_depth  + total_effort)
summary(all_nb) # absolutely off 
# unfittable


#### DEEP - NB
deep_lm <- lm(data = d, perdeepfurrow ~ mean_depth  + total_effort)
gvlma(deep_lm) # nonne

deep_log <- lm(data = d, log(perdeepfurrow+1) ~ mean_depth  + total_effort)
gvlma(deep_log) # none

deep_nb <- glm.nb(data = d, perdeepfurrow ~ mean_depth  + total_effort)
summary(deep_nb) # ok
plot(deep_nb) # ok
paste(round(with(deep_nb, deviance/df.residual),2),
      ifelse((with( deep_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) # ok



#### SHALLOW -NB
sh_lm <- lm(data = d, pershallowfurrow ~ mean_depth  + total_effort)
gvlma(sh_lm) #none

sh_log <- lm(data = d, log(pershallowfurrow+1) ~ mean_depth  + total_effort)
gvlma(sh_log) # none


shallow_nb <-  glm.nb(data = d, pershallowfurrow ~ mean_depth  + total_effort)
summary(shallow_nb) # error..unfittable ??? model looks crap
plot(shallow_nb) #off
paste(round(with(shallow_nb, deviance/df.residual),2),
      ifelse((with( shallow_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) # overdispersed

library(pscl)
szero<- zeroinfl(data = d, pershallowfurrow ~ mean_depth  + total_effort, dist = "negbin")
# Error in zeroinfl(data = d, pershallowfurrow ~ mean_depth + total_effort,  : invalid dependent variable, non-integer values



## JAGGED
j_lm <- lm(data = d, perjagged ~ mean_depth  + total_effort)
gvlma(j_lm) # 1 of 5

j_log  <- lm(data = d, log(perjagged+1) ~ mean_depth  + total_effort)
gvlma(j_log) # 1 of 5

j_nb <- glm.nb(data = d, perjagged ~ mean_depth  + total_effort)
summary(j_nb) # not ok
plot(j_log) # not ok
paste(round(with(j_nb, deviance/df.residual),2),
      ifelse((with(j_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) # overdispersed

### TURNED
o_lmef <- lm(data = d, peroverturned ~ mean_depth  + total_effort)
gvlma(o_lmef) # none

o_logef <- lm(data = d, log(peroverturned+1) ~ mean_depth  + total_effort)
gvlma(o_logef) # none

O_nbef <- glm.nb(data = d, peroverturned ~ mean_depth  + total_effort)
summary(O_nbef) # ok
plot(O_nbef) # so and so nut passable
paste(round(with(O_nbef, deviance/df.residual),2),
      ifelse((with(O_nbef, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) # not overdispersed

