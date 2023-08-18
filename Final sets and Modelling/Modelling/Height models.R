#Finalising the Height dataset


rm(list=ls())
height <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height.csv")
trawl <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv")

library(tidyverse)


############# TIDYING######################################################

#select columns to merge
d <- trawl[,c(2,3,6,7,9:14, 16)]
#clean extra cols
height <- height[,-c(1,3)]
boxplot(height$Length) # there is a negative value
height <- height %>% filter(Length > 0)
boxplot(height$Length) # OK

#create new data to save the summarise WITH MEDIAN
d2 <- height %>%
 group_by(label_name, video_filename) %>%
  summarize(Length = median(Length, na.rm = TRUE),
           minimum = min(Length),
           maximum = max(Length)) 

# two NA values in Halipteris 2017_PA_07_061 that should be there
#trial <- height %>% filter(label_name == "Halipteris finmarchica", video_filename == "2017_PA_07_061")
#min(trial$Length) 
#returns NA there must be a problem with the data structure 
#plot(trial$Length)
#manually retrieve values
#trial<-trial[-1138,]
#min(trial$Length) # now it works = 14.03827
#max(trial$Length) #183.8666

#substitue values
#d2$`minimum`[136] <- 14.03827
#d2$`maximum`[136] <- 183.8666
#OK


d1 <- full_join(d2, trawl, by = "video_filename")

write.csv(d1,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height_complete.csv" )
write.csv(height,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height_full.csv" )
write()


##################################################################
# MODELLING 
###################################################################
# INSPECTING

hist(d1$Length) # a perfect poisson distribution!

plot(d1$Length ~ d1$years_since_trawled) #not sure either
plot(d1$Length ~ d1$mean_depth) # clear relationship! 


## MODELLING
library(lme4)
library(MASS)
library(gvlma)

#### Because we know that effort and years since last trawled are highly correlated, we need to make a choice on the approach here
# I think effort makes more sense from an abundance point of view
# years since last from a growth point of view (ie. an area can be lightly trawled for a long time = )

# now we can split the data into each species category
aa <- d1 %>% filter(label_name == "Acanella arbuscula")
af <- d1 %>% filter(label_name == "Asconema foliatum")
ag <- d1 %>% filter(label_name == "Anthoptilum grandiflorum")
hf <- d1 %>% filter(label_name == "Halipteris finmarchica")




### ACANELLA ###
 
# MEDIAN
aa_lm<- lm(data = aa, Length ~ mean_depth + years_since_trawled)
summary(aa_lm) # both depth and effort - r2 at 49%
gvlma(aa_lm) #3/5
par(mfrow = c(2, 2))
plot(aa_lm) #also looks ok

#log
aa_log <- lm(data = aa, log(Length+1) ~ mean_depth + years_since_trawled)
gvlma(aa_log) #0/5
plot(aa_log) # looks quite bad

#poi
aa_poi <- glm(data = aa, Length ~ mean_depth + years_since_trawled, family = poisson(link = "log"))
summary(aa_poi) # res vs Df is off

#NB
aa_nb<- glm.nb(data = aa, Length ~ mean_depth + years_since_trawled)
summary(aa_nb) # res deviance is so and so but much better
par(mfrow = c(2, 2))
plot(aa_nb) # looks okay!
paste(round(with(aa_nb, deviance/df.residual),2),
            ifelse((with(aa_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#Not overdispersed
 

#Minimum
#aa_lm_min <- lm(data = aa, minimum~ mean_depth + years_since_trawled)
#gvlma(aa_lm_min) #2/5
#plot(aa_log_min) # also looks ok
#summary(aa_lm_min) # r2 at 5%

#aa_log_min <- lm(data = aa, log(minimum+1) ~ mean_depth + years_since_trawled)
#gvlma(aa_log_min) #2/5

#aa_poi_min <- glm(data = aa, minimum~ mean_depth + years_since_trawled, family = poisson(link = "log"))
#summary(aa_poi_min) # usual Res dev VS Df is off

#aa_nb_min <- glm.nb(data = aa, minimum~ mean_depth + years_since_trawled)
#summary(aa_nb_min) # res dev vs Df is still pretty high but better - AIc 456
#plot(aa_nb_min) # looks ok
#paste(round(with(aa_nb_min, deviance/df.residual),2),
#      ifelse((with(aa_nb_min, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
# not overdispersed
# BEST MODEL


# MAX
#aa_lm_max <- lm(data = aa, maximum ~ mean_depth + years_since_trawled)
#gvlma(aa_lm_max) #5/5
#plot(aa_lm_max)
#summary(aa_lm_max)
# BEST MODEL



### ASCONEMA ###
# Median
af_lm <- lm(data = af, Length ~ mean_depth + years_since_trawled)
gvlma(af_lm) # 3/5
par(mfrow = c(2, 2))
plot(af_lm) # looks ok
summary(af_lm) # R of 12%

af_log <- lm(data = af, log(Length +1 )~ mean_depth + years_since_trawled )
gvlma(af_log) #5/5
plot(af_log) #ok
summary(af_log) # R2 is at 10% now, still very low


#others not necessary
af_poi <- glm(data = af, Length ~ mean_depth + years_since_trawled, family = poisson(link = "log"))
plot(af_poi) #cooks almost off but passable
summary(af_poi) # res dev VS DF is off

af_nb <- glm.nb(data = af, Length ~ mean_depth + years_since_trawled)
plot(af_nb) #better, OK!
summary(af_nb) # This one is OK! 
paste(round(with(af_nb, deviance/df.residual),2),
      ifelse((with(af_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
# not overdispersed
# THIS BEST MODEL


# Minimum
#af_lm_min <- lm(data = af, minimum ~ mean_depth + years_since_trawled)
#gvlma(af_lm_min) #2/5

#af_log_min <- lm(data = af, log(minimum+1) ~ mean_depth + years_since_trawled)
#gvlma(af_log_min) #4/5
#summary(af_log_min) # again a negative R square

#af_nb_min <- glm.nb(data = af, minimum ~ mean_depth + years_since_trawled)
#summary(af_nb_min) # is OK!
#plot(af_nb_min) #passable
#paste(round(with(af_nb_min, deviance/df.residual),2),
#      ifelse((with(af_nb_min, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#not overdispersed


## Maximum
#af_lm_max <- lm(data = af, maximum ~ mean_depth + years_since_trawled)
#gvlma(af_lm_max) #5/5
#summary(af_lm_max) #r at 13

#af_log_max <-lm(data = af, log(maximum+1) ~ mean_depth + years_since_trawled)
#gvlma(af_log_max) #5/5
#summary(af_log_max) # even lower R2
# LINEAR IS BEST MODEL



### ANTHOPTILUM ###
ag_lm <- lm(data = ag, Length~ mean_depth + years_since_trawled) 
gvlma(ag_lm) #2/5
summary(ag_lm) #r2 at 6%

ag_log <- lm(data = ag, log(Length+1)~ mean_depth + years_since_trawled) 
gvlma(ag_log) #4/5
summary(ag_log) #r at 5
plot(ag_log)
# best model


# Minimum
#ag_lm_min <- lm(data = ag, minimum~ mean_depth + years_since_trawled) 
#gvlma(ag_lm_min) #2/5

#ag_log_min <-lm(data = ag, log(minimum+1) ~ mean_depth + years_since_trawled) 
#gvlma(ag_log_min) #4/5
#summary(ag_log_min) #R2 at 1%
#best model

#ag_poi_min <-glm(data = ag, minimum~ mean_depth + years_since_trawled, family = poisson(link = "log")) 
#summary(ag_poi_min) #rubbish

#ag_nb_min <-glm.nb(data = ag, minimum~ mean_depth + years_since_trawled) 
#summary(ag_nb_min) #Res dev versus Df is ok
#plot(ag_nb_min) #passable
#paste(round(with(ag_nb_min, deviance/df.residual),2),
#      ifelse((with(ag_nb_min, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#not


# Maximum
#ag_lm_max <- lm(data = ag,maximum ~ mean_depth + years_since_trawled)
#gvlma(ag_lm_max) #2/5

#ag_poi_max <- lm(data = ag, log(maximum+1) ~ mean_depth + years_since_trawled)
#gvlma(ag_poi_max) #5/5
#summary(ag_poi_max) # r2 at 14




#### HALIPTERIS ###
hf_lm <- lm(data = hf, Length  ~ mean_depth + years_since_trawled)
gvlma(hf_lm) #5/5
summary(hf_lm) #r at 34 Good model!

hf_lm_easy <- lm(data = hf, Length  ~  years_since_trawled)
summary(hf_lm_easy) # nothing

# hf_log <-lm(data = hf, log(Length+1)  ~ mean_depth + years_since_trawled)
# summary(hf_log) # better r2
# gvlma(hf_log) #0/5

# hf_poi <- glm(data = hf, Length  ~ mean_depth + years_since_trawled, family = poisson(link = "log"))
# summary(hf_poi) # ususal Res dev VS DF totally off
# 
# hf_nb <- glm.nb(data = hf, Length  ~ mean_depth + years_since_trawled)
# summary(hf_nb) # res dev VS DF is perfect!
# plot(hf_nb) # doesnt look too pretty
# #We will have to take this
# paste(round(with(hf_nb, deviance/df.residual),2),
#         ifelse((with(hf_nb, deviance/df.residual)>1.5), "overdispersed", "not overdispersed"))
#       #not overdispersed

#minimum
#hf_lm_min <- lm(data = hf, minimum  ~ mean_depth + years_since_trawled)
#gvlma(hf_lm_min) #5/5
#summary(hf_lm_min) # r2 at 3 %

#hf_log_min <- lm(data = hf, log(minimum +1) ~ mean_depth + years_since_trawled)
#gvlma(hf_log_min) #5/5#
#summary(hf_log_min) #r2 at 5%
#best model


#Maximum
#hf_lm_max<-lm(data = hf, maximum  ~ mean_depth + years_since_trawled)
#gvlma(hf_lm_max) #5/5
#summary(hf_lm_max) #r2 at 7%

#hf_log_max <- lm(data = hf, log(maximum +1) ~ mean_depth + years_since_trawled)
#gvlma(hf_log_max) #5/5
#summary(hf_log_max) #r2 at 15%

##################################################################
# TRAWLING EFFORT
###################################################################

# Lets see if the saem signifciance values apply to trawling effort rather than years since last trawled

#### ACANELLA
aa_lm <- lm(data = aa, Length ~ mean_depth + total_effort)
gvlma(aa_lm) # 5 of 5
summary(aa_lm)



#### ASCONEMA - log
af_ef<- lm(data = af,Length ~ mean_depth + total_effort)
gvlma(af_ef) #5/5
summary(af_ef) # negative r squared

af_ef_log <- lm(data = af,log(Length+1) ~ mean_depth + total_effort)
gvlma(af_ef_log) #good
summary(af_ef_log) # still a negative r2 ? 

af_nbef <- glm.nb(data = af,Length ~ mean_depth + total_effort)
summary(af_nbef) # look ok
plot(af_nbef) # looks good
paste(round(with(af_nbef, deviance/df.residual),2),
      ifelse((with(af_nbef, deviance/df.residual)>1.5), "overdispersed", "not overdispersed")) # not



#### ANTHOPTILUM - LOG
ag_eflm <- lm(data = ag, Length ~ mean_depth + total_effort)
gvlma(ag_eflm) #3 of 5

ag_log <-  lm(data = ag,log(Length+1) ~ mean_depth + total_effort)
gvlma(ag_log) #5/5
summary(ag_log) # Effort is not - SAME RESULT



#### HALIPTERIS - NB
hfef_lm <- lm(data = hf,Length ~ mean_depth + total_effort )
gvlma(hfef_lm) #5 of 5
summary(hfef_lm)










#########################################################################
  # to export the data into qgis first need to pivot wider

median_pivot <- d1 %>% pivot_wider(names_from = "label_name", values_from = "Length")
#lets try
write.csv(median_pivot, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/median4gis.csv")


