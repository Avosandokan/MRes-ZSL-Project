## CATEGORICAL MODEL FOR MEDIAN LENGTH 
# After looking at the qgis plots, it seems like there might be an underlying story on the years since trawling for the median length
# however, the models have been hard to implement, thus, we are substituting the years since trawled with a categrical variable
# 18 = NO (never been trawled)
# 0-17 = YES (it has been trawled)

# Then I can also try separating the data into 3 categoricals like trawled "currently", "recently" "never"
rm(list = ls())
library(tidyverse)
library(lme4)

d <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height_complete.csv")
d <- d[,-c(1,7)]
d <- d[,1:19] # clearins some extra columns



# Because every species is different here, we will have to split them into the groups according to a reasonable number of observations.
# In other words, we want to having one category where there are little to no observations.

aa <- d %>% filter(label_name == "Acanella arbuscula")
ag <- d %>% filter(label_name == "Anthoptilum grandiflorum")
hf <- d %>% filter(label_name == "Halipteris finmarchica")
af <- d %>% filter(label_name == "Asconema foliatum")



#### ACANELLA ###
#0-5 is recently
#6-17 is in the past
# 18 is never

#make a new column variable
aa$category <- NA

aa$category <- ifelse(aa$years_since_trawled >= 0 & aa$years_since_trawled <= 5, "A_recently", 
                     ifelse(aa$years_since_trawled >= 6 & aa$years_since_trawled <= 17, "B_in the past",
                            "C_never")) #otherwise for 18 print never

aa$category <- as.factor(aa$category)
sort(aa$category) # 6 recent, 7 in the past



#### ASCONEMA ###
# 0 to 6 is recent
#6 to 17 is in the past
af$category <- NA

af$category <- ifelse(af$years_since_trawled >= 0 & af$years_since_trawled <= 6, "A_recently", 
                      ifelse(af$years_since_trawled >= 7 & af$years_since_trawled <= 17, "B_in the past",
                             "C_never")) #otherwise for 18 print never

af$category <- as.factor(af$category)
sort(af$category)
# 6 each for recent and in the past



#### ANTHOPTILUM ###
# 0 to 6 and 6 to 17
ag$category <- NA

ag$category <- ifelse(ag$years_since_trawled >= 0 & ag$years_since_trawled <= 6, "A_recently", 
                      ifelse(ag$years_since_trawled >= 7 & ag$years_since_trawled <= 17, "B_in the past",
                             "C_never")) #otherwise for 18 print never

ag$category <- as.factor(ag$category)
sort(ag$category) # 7 each


#### HALIPTERIS ###
# Because we have so little data points here we just have to differentiate between in the past and never....
hf$category <- NA

hf$category <- ifelse(hf$years_since_trawled >= 0 & hf$years_since_trawled <= 17, "B_in the past", 
                             "C_never") #otherwise for 18 print never

hf$category <- as.factor(hf$category)
sort(hf$category) # 5 for in the past


par(mfrow = c(2,2))
boxplot(aa$Length  ~ aa$category, main = "acanella")
boxplot(af$Length  ~ af$category, main = "asconema")
boxplot(ag$Length  ~ ag$category, main = "anth")
boxplot(hf$Length  ~ hf$category, main = "halip")


#########################################################################
# MODELLING 3 categories
##########################################################################

library(tidyverse)
library(lme4)
library(gvlma)

# Split into species



#### ACANELLA
aalm <- lm(aa$Length ~ aa$category + aa$mean_depth)
summary(aalm) #r 2 at 51%
par(mfrow = c(2,2))
plot(aalm) # looks ok
gvlma(aalm) #5/5

# Model simplification to assess if the variable is significant all together 
aa_easy <- lm(aa$Length ~ aa$category)
summary(aa_easy)
# not really




##########################################################################
# MODELLING  2 categories
##########################################################################
# We only categorise by YES or NO

d$binary <- NA
d$binary <- ifelse(d$years_since_trawled >= 0 & d$years_since_trawled <= 17, "A_YES", 
                     "B_ NO")

aa_bis <- d %>% filter(label_name == "Acanella arbuscula")
ag_bis <- d %>% filter(label_name == "Anthoptilum grandiflorum")
hf_bis <- d %>% filter(label_name == "Halipteris finmarchica")
af_bis <- d %>% filter(label_name == "Asconema foliatum")

par(mfrow = c(2,2))
boxplot(aa_bis$Length  ~ aa_bis$binary, main = "acanella")
boxplot(af_bis$Length  ~ af_bis$binary, main = "asconema")
boxplot(ag_bis$Length  ~ ag_bis$binary, main = "anth")
boxplot(hf_bis$Length  ~ hf_bis$binary, main = "halip")

# now it looks very obvious



##### MODELS
# You need to do stepwise model simplification to be able to assess if each variable is significant
# in the case of categoricals, this way you can say if the category Yes or No are significant.
# If in model simplification it turns out that the varianle is not, then the whoel yes VS no is not isignificant

### ACANELLA
aa2lm <- lm(aa_bis$Length ~ aa_bis$binary + aa_bis$mean_depth)
plot(aa2lm) # ok
gvlma(aa2lm) # 5/5
summary(aa2lm)

aa2_easy <- lm(aa_bis$Length~ aa_bis$binary)
summary(aa2_easy) # yes the variable is significant
aa22_easy <- lm(aa_bis$Length ~ aa_bis$mean_depth)
summary(aa22_easy) # yeas is significant



### ASCONEMA
af2lm <- lm(af_bis$Length ~ af_bis$binary + af_bis$mean_depth)
summary(af2lm) # depth is not at all
gvlma(af2lm) #5/5

#simplify
af2_easy <- lm(af_bis$Length ~ af_bis$binary)
summary(af2_easy)
# The untrawled sites are NOT statistically significant from the trawled ones
af22_easy <- lm(af_bis$Length ~ af_bis$mean_depth)
summary(af22_easy) # not either



#### ANTHOPTILUM
ag2lm <- lm(ag_bis$Length~ag_bis$binary + ag_bis$mean_depth)
summary(ag2lm) # looks like it is not
gvlma(ag2lm) # only 3/5

#Try a log 
ag2log <-  lm(log(ag_bis$Length+1)~ag_bis$binary + ag_bis$mean_depth)
summary(ag2log) # R2 improved very slightly
gvlma(ag2log) # much better it is a 5/5 now

#simplify
ag2log_easy <- lm(log(ag_bis$Length+1)~ag_bis$binary)
summary(ag2log_easy) # not statistical difference



#### HALIPTERIS
hf2lm <- lm(hf_bis$Length ~ hf_bis$binary + hf_bis$mean_depth)
summary(hf2lm) # looks like no
gvlma(hf2lm) #5/5

hf2lm_easy <- lm(hf_bis$Length ~ hf_bis$binary)
summary(hf2lm_easy) # no ???????

hal_trial <- lm(hf_bis$Length ~ hf_bis$years_since_trawled + hf_bis$mean_depth)
summary(hal_trial) # neither
gvlma(hal_trial)



############################################################################
# PLOTS
############################################################################

library(ggplot2)

# ACANELLA #
par(mfrow = c(2,2))
ggplot(aa, aes(x=category, y=Length)) + geom_violin() +
  # make a violin plot with median boxplot inside
  geom_boxplot(width=0.1) +
  xlab("Time since trawled") + ylab("Median Length (mm)") + ggtitle("A. arbuscula") +
  #get rid of ugly background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Try with only a yes or no
ggplot(aa_bis, aes(x=binary, y=Length)) + geom_violin() +
  # make a violin plot with median boxplot inside
  geom_boxplot(width=0.1) +
  #get rid of ugly background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# HALIPTERIS #
ggplot(hf, aes(x=category, y=Length)) + geom_violin() +
  xlab("Time since trawled") + ylab("Median Length (mm)") + ggtitle("B. finmarchica")+
  # make a violin plot with median boxplot inside
  geom_boxplot(width=0.1) +
  #get rid of ugly background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



# ASCONEMA #
ggplot(af, aes(x=category, y=Length)) + geom_violin() +
  xlab("Time since trawled") + ylab("Median Length (mm)") + ggtitle("A. foliatum") +
  # make a violin plot with median boxplot inside
  geom_boxplot(width=0.1) +
  #get rid of ugly background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# ANTHOPTILUM #
ggplot(ag, aes(x=category, y=Length)) + geom_violin() +
  xlab("Time since trawled") + ylab("Median Length (mm)") + ggtitle("A. grandiflorum") +
  # make a violin plot with median boxplot inside
  geom_boxplot(width=0.1) +
  #get rid of ugly background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
