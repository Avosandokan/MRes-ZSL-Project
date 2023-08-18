#PRELIMINARY MODELLING 
# Here let's work on a consistent workflow that can be applied to all data for modelling. Four models are tried on each variable(species).
# After quality control on each model, we can chose the best fitting one

rm(list=ls()) #clear workspace 
par(mfrow=c(1,1)) #reset margins
count <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")

par(mfrow = c(2, 2))
hist(count$normalise_acanella)
hist(count$normalise_halipteris)
hist(count$normalise_anthoptilum)
hist(count$normalise_asconema)
# they all have a very left skewed distribution

#look for the easiest variable (the one with the leats zeros)
length(count$normalise_acanella[count$normalise_acanella>0])
length(count$normalise_halipteris[count$normalise_halipteris>0])
length(count$normalise_anthoptilum[count$normalise_anthoptilum>0])
length(count$normalise_asconema[count$normalise_asconema>0])
#it s acanella - then asconema and anthoptilum, halipteris has the most zeros

#round to whole numbers
#count$normalise_acanella  <- round(count$normalise_acanella, digits = 0)
#count$normalise_asconema  <- round(count$normalise_asconema, digits = 0)
#count$normalise_anthoptilum  <- round(count$normalise_anthoptilum, digits = 0)
#count$normalise_halipteris  <- round(count$normalise_halipteris, digits = 0)
#write.csv(count,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")

count <-na.omit(count)


#### CORRELATION TESTS  ####
library(lme4)
library(lmerTest)
par(mfrow=c(1,1))
## It is very important to check to COLLINEARITY AND CORRELATION in the veriables, especially for temp - depth
# We are measuring trawling in two forms (scars and effort). By assessing collinearity we can REMOVE excess variables
plot(count$mean_depth, count$best_temp) #there could be a negative trend but the three lowest temp could be skewing
plot(count$percalltrawling,count$total_effort) #seems like a possible positive trend

#there are a three stations that are very shallow and very cold, we get rid of them and see if there is a correlation
count$mean_depth[count$mean_depth>640]
count$best_temp[count$best_temp>3]
library(PerformanceAnalytics)
chart.Correlation(data.frame(count$best_temp[count$best_temp>3], count$mean_depth[count$mean_depth>640]))
# Now you can clearly see the negative trend. For the scope of the research we EXCLUDE TEMPERATURE FROM MODELLING

# This library makes you do a multiple figure plot with several scatters and correlation coefficients (pearson).
# Coefficients  0.7 -0.9 indicate high correlation || 0.5 - 0.7 considered moderately correlated.
chart.Correlation(data.frame(count$normalise_acanella, count$mean_depth, count$percalltrawling, count$total_effort, count$years_since_trawled))
# Scars and effort are highly correlated. We could probably take away percall

chart.Correlation(data.frame(count$normalise_acanella, count$mean_depth, count$total_effort, count$years_since_trawled))
#now okay, years since last trawled is still borderline
#we decided to keep effort OUT for the scope and novelty of this research ! And because highly correlated

############################
#ACANELLA
############################

## LINEAR MODEL
lmaca<- lm(data = count, normalise_acanella ~ mean_depth +  years_since_trawled)
summary(lmaca)
# Explains only 5% Of the data - only years since lats trawled almost significant
# DIAGNOSTICS
par(mfrow = c(2, 2))
plot(lmaca) # scale location off
library(gvlma)
gvlma(lmaca) #2/5 asusmptions met
# The point of the gvlma test is to check whether a simple lm is suitable for the data you have.
# If / when gvlma indicates that the data violates the assumptions for lm, that is when you move onto the funkier models that try to cope with the violated assumptions. 


## LOG TRANSFORMING
lmalog <- lm(data = count, log(normalise_acanella+1) ~ mean_depth  + years_since_trawled)
summary(lmalog)
# explains 23% of the acanella presence
# since last trawled significant
plot(lmalog) #Better, everything is almost acceptable
gvlma(lmalog) #5/5 met


### POISSON
lmapoi <- glm(data = count, normalise_acanella ~ mean_depth  + years_since_trawled, family = poisson(link="log"))
summary(lmapoi)
#everything significant - aic 4947 - Residual deviance vs DF is OFFF!!!!
plot(lmapoi) # cooks distance and scale location off


## NEGATIVE BINOMIAL
library(MASS)
nb <- glm.nb(data = count, normalise_acanella ~ mean_depth + years_since_trawled)
summary(nb) # years since last trawled significant  - AIC 537 - Residual deviance is OK
plot(nb) # looks ok, QQ slighly wacky
#check for overdispersion
paste(round(with(nb, deviance/df.residual),2),
      ifelse((with(nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#1.23 not overdisperse

## THE BEST MODEL IS A LOG TRANFORMED 




######################################################
# ASCONEMA 
######################################################
par(mfrow=c(1,1))

## LINEAR MODEL
lmasc <- lm(data = count, normalise_asconema ~ mean_depth  + years_since_trawled)
summary(lmasc) #R2 1% - nothing significant 
par(mfrow = c(2, 2))
plot(lmasc) # looks good, qq residuals am bit out 
gvlma(lmasc) # 2/5 ok


## LOG+1
lmasclog <- lm(data = count, log(normalise_asconema+1) ~ mean_depth  + years_since_trawled)
summary(lmasclog) #R2 6% nothing significant
plot(lmasclog) #scale location is off
gvlma(lmasclog) # 4/5 met


## POISSON
lmascp <- glm(data = count, normalise_asconema ~ mean_depth + years_since_trawled, family = poisson(link = "log"))
summary(lmascp) #res.dev versus DF is OFF!!!!!
# years since last trawled significant
#check for overdispersion
paste(round(with(lmascp, deviance/df.residual),2),
      ifelse((with( lmascp, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#overdispersed


## NB
lmascnb <- glm.nb(normalise_asconema ~ mean_depth  + years_since_trawled, data = count)
summary(lmascnb) #residual vs deviance OK - nothing significant
plot(lmascnb) # looks OK, QQ slighly off
paste(round(with(lmascnb, deviance/df.residual),2),
      ifelse((with( lmascnb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#not overdispersed

### LOG + 1 IS THE BEST MODEL


#########################################################
# ANTHOPTILUM
########################################################
lmant <- lm(data = count, normalise_anthoptilum~ mean_depth  + years_since_trawled )
summary(lmant) #negative r2
#go straight to another model


# LOG +1
lmantlog <- lm(data = count, log(normalise_anthoptilum+1)~ mean_depth  + years_since_trawled)
summary(lmantlog) #R2 8% - depth and since last trawled significant
plot(lmantlog) # scale location off but rest is fine
gvlma(lmantlog) #4/5


# Poisson
lmantp <- glm(data = count, normalise_anthoptilum~ mean_depth  + years_since_trawled, family=poisson(link = "log"))
summary(lmantp) #res vs DF is OFF! 
plot(lmantp) #cooks distance is off
paste(round(with(lmantp, deviance/df.residual),2),
      ifelse((with( lmantp, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
# overdispersed


#NB
lmantnb <- glm.nb(data = count, normalise_anthoptilum~ mean_depth  + years_since_trawled)
summary(lmantnb) #res vs DF is ok - years since last trawled almost significant
plot(lmantnb) # Ok besies QQ
paste(round(with(lmantnb, deviance/df.residual),2),
      ifelse((with( lmantnb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#not overdispersed

### LOG + 1 IS THE BEST MODEL


#HELP############################
#################################################
#HALIPTERIS
################################################
#after omitting NA lets see if it happens (two stations where there was no trawling and no species)
hal<- lm(data = count, normalise_halipteris ~ mean_depth + years_since_trawled)
summary(hal) #R2 4%
plot(hal) #residual fitted and scale are off!
gvlma(hal) #none met


## LOG+1
hallog <- lm(data = count, log(normalise_halipteris+1) ~ mean_depth  + years_since_trawled)
summary(hallog) #negative r2
plot(hallog) #same still res fitted and scale OFF
gvlma(hallog) #1/4


## Poisson 
halp <- glm(data = count, normalise_halipteris ~ mean_depth  + years_since_trawled, family = poisson(link = "log"))
summary(halp) #res vs deviance is off
plot(halp) #scale location and cooks totally off


#NB
halnb <- glm.nb(data = count, normalise_halipteris ~ mean_depth  + years_since_trawled)
#Error in glm.fitter(x = X, y = Y, weights = w, etastart = eta, offset = offset,  : NA/NaN/Inf in 'x'
#In addition: Warning message:step size truncated due to divergence 

#try take away one variable
halnb1 <- glm.nb(data = count, normalise_halipteris ~ mean_depth  + years_since_trawled) #same issue

#FROM https://stats.stackexchange.com/questions/93352/error-in-fitting-negative-binomial-regression-model-in-r-when-replicating-publis
# glmmADMB based on AD Model builder, to do among other things negative binomial mixed models in R. I was curious to see if the software could handle the special case of zero random effects i.e not a mixed model. 
#The results look pretty good so I think that one could use glmmADMB to fit negative binomial (or ZINB) models in R even when there are no random effects. 
library(glmmTMB) #Warning message:In checkMatrixPackageVersion() : Package version inconsistency detected. 

# TRY ZERO INFLATED NEGATIVE BINOMIAL
library(pscl)
halzero <- zeroinfl(data = count, normalise_halipteris ~ mean_depth  + years_since_trawled, dist = "negbin")
# Warning message ! In value[[3L]](cond) :system is computationally singular: reciprocal condition number = 5.97703e-41FALSE
summary(halzero) # NA only everywhere - model UNFITTABle


## DOUBLE CHECK WITH CHRIS AND STEVE ABOUT HALIPTERIS - Models do not comply





###################################################################################
 # FISHING EFFORT MODELS
###################################################################################
library(MASS)
library(gvlma)

# ACANELLA
efa_lm <- lm(data = count, normalise_acanella ~ mean_depth  + total_effort)
gvlma(efa_lm) #2/5 not ok

efa_log <- lm(data = count, log(normalise_acanella+1) ~ mean_depth  + total_effort)
gvlma(efa_log) #5/5
par(mfrow = c(2, 2))
plot(efa_log) # nice
summary(efa_log)


# ASCONEMA
efaf_lm <- lm(data = count, normalise_asconema ~ mean_depth  + total_effort)
gvlma(efaf_lm) #2 of 2 not good

efaf_log <- lm(data = count, log(normalise_asconema +1)~ mean_depth  + total_effort)
gvlma(efaf_log) #4 of 5 is ok
plot(efaf_log) # so and so but alright
summary(efaf_log)


# ANTHOPTILUM
efag_lm <- lm(data = count, normalise_anthoptilum ~ mean_depth  + total_effort)
gvlma(efag_lm)      # 1 of 5

efag_log <- lm(data = count, log(normalise_anthoptilum +1)~ mean_depth  + total_effort)
gvlma(efag_log) # 2 of 5

efag_poi <- glm(data = count, normalise_anthoptilum ~ mean_depth  + total_effort, family = poisson(link = "log"))
summary(efag_poi) # res vs dev is off
plot(efag_poi) # cooks distance not too good

efag_nb <- glm.nb(data = count, normalise_anthoptilum ~ mean_depth  + total_effort)
summary(efag_nb) # very nice now
plot(efag_nb) # not too nice but ugh....
paste(round(with(efag_nb, deviance/df.residual),2),
      ifelse((with( efag_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) # ok


# HALIPTERIS
efh_lm <- lm(data = count, normalise_halipteris ~ mean_depth  + total_effort)
gvlma(efh_lm) # 1 of 5

efh_log <- lm(data = count, log(normalise_halipteris+1) ~ mean_depth  + total_effort)
gvlma(efh_log) #  2 of 5

efh_nb <- glm.nb(data = count, normalise_halipteris ~ mean_depth  + total_effort)
#Error: no valid set of coefficients has been found: please supply starting values
# Changing the formula to use the logit link (as you want a logistic regression according to your question) gets rid of the warnings (and does not need starting parameters):

efh_nb <- glm.nb(data = count, normalise_halipteris ~ mean_depth  + total_effort, family = binomial(link = "logit"))
summary(efh_nb)

# Not possible

# TRY ZERO INFLATED NEGATIVE BINOMIAL
library(pscl)
halzero_eff <- zeroinfl(data = count, normalise_halipteris ~ mean_depth  + total_effort, dist = "negbin")
# Warning message ! In value[[3L]](cond) :system is computationally singular: reciprocal condition number = 5.97703e-41FALSE
summary(halzero_eff) 
# Na only

# model failed
