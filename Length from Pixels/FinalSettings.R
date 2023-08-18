# load code with height calculation function
source("HeightsInImage.R")

# put your biggle file in here
d<-read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/df_lines.csv")

# rows 1 : 2479 are 2017 videos where you have to change the cam set up 
d2017 <- d[1:2479,]
d201822 <- d[2480:4772,]


##############################################
#2018 TILL 2022
##############################################

# find full camera setup based on your image specifications
# declare known camera details (these are for Steve's tests)
CamAngle<-28.8
CamHeight<-550
PixDimY<-1520
PixDimX<-2704
FullHFOV<-122.6 # gopro 5 full use
FullVFOV<-94.4 # gopro 5 full use
UsedHFOV<-94.4 # gopro 5 - 16x9 M setting
UsedVFOV<-55 # gopro 5 - 16x9 M setting
FullSensorWidth<-6.17
FullSensorHeight<-4.65


# calculate FOV in water
#FullHFOVWater <- getInWaterFOV(FullHFOV, max(RI)) # 81.78
#FullVFOVWater <- getInWaterFOV(FullVFOV, max(RI)) # 66.40
#UsedHFOVWater <- getInWaterFOV(UsedHFOV, max(RI)) # 66.40
#UsedVFOVWater <- getInWaterFOV(UsedVFOV, max(RI)) # 40.31
#Error: object 'RI' not found, substituting with value suggested in other script

FullHFOVWater <- getInWaterFOV(FullHFOV, 1.34) # 81.78 all results match 
FullVFOVWater <- getInWaterFOV(FullVFOV, 1.34) # 66.40
UsedHFOVWater <- getInWaterFOV(UsedHFOV, 1.34) # 66.40
UsedVFOVWater <- getInWaterFOV(UsedVFOV, 1.34) # 40.31


CamSetup<-getCamSetup(FullHFOVWater, FullVFOVWater,
                      UsedHFOVWater, UsedVFOVWater,
                      PixDimX, PixDimY,
                      FullSensorWidth, FullSensorHeight,
                      CamAngle, CamHeight)


# filter to just linestring annotations
#d<-d[d$shape_name=="LineString",]
# make columns for coordinates
#d$X1<-NA
#d$Y1<-NA
#d$X2<-NA
#d$Y2<-NA

# convert annotations into start and end points
#for(i in 1:nrow(d)){
    # get point & strip brackets
#    p<-stringr::str_replace(d$points[i],"\\[\\[","")
#    p<-stringr::str_replace(p,"\\]\\]","")
 #   p4<-stringr::str_split(p,",")
 #   d$X1[i]<-as.numeric(p4[[1]][1])
 #   d$Y1[i]<-as.numeric(p4[[1]][2])
 #   d$X2[i]<-as.numeric(p4[[1]][3])
#    d$Y2[i]<-as.numeric(p4[[1]][4])}


# add column for object height and length
d201822$Height<-NA
d201822$Length<-NA

# now loop through and calculate heights
for(i in 1:nrow(d201822)){
    xx<-getAnnotationHeight(d201822$X1[i], d201822$Y1[i], d201822$X2[i], d201822$Y2[i], CamSetup)
    d201822$Height[i]<-xx$ObjHeight
    d201822$Length[i]<-xx$ObjLength
}




#################################################
#2017 
#################################################

# find full camera setup based on your image specifications
# declare known camera details (these are for Steve's tests)
CamAngle<-28.8
CamHeight<-550
PixDimY<-1080
PixDimX<-1920
FullHFOV<-122.6 # gopro 5 full use
FullVFOV<-94.4 # gopro 5 full use
UsedHFOV<-94.4 # gopro 5 - 16x9 M setting
UsedVFOV<-55 # gopro 5 - 16x9 M setting
FullSensorWidth<-6.17
FullSensorHeight<-4.65

# calculate FOV in water
FullHFOVWater <- getInWaterFOV(FullHFOV, 1.34) # 81.78 all results match 
FullVFOVWater <- getInWaterFOV(FullVFOV, 1.34) # 66.40
UsedHFOVWater <- getInWaterFOV(UsedHFOV, 1.34) # 66.40
UsedVFOVWater <- getInWaterFOV(UsedVFOV, 1.34) # 40.31

CamSetup<-getCamSetup(FullHFOVWater, FullVFOVWater,
                      UsedHFOVWater, UsedVFOVWater,
                      PixDimX, PixDimY,
                      FullSensorWidth, FullSensorHeight,
                      CamAngle, CamHeight)

for(i in 1:nrow(d2017)){
  xx<-getAnnotationHeight(d2017$X1[i], d2017$Y1[i], d2017$X2[i], d2017$Y2[i], CamSetup)
  d2017$Height[i]<-xx$ObjHeight
  d2017$Length[i]<-xx$ObjLength
}




############# MERGING AND CLEANING #########################
library(tidyverse)
d <- bind_rows(d2017, d201822)
d <- d[,-4]


###############################################################################
# length profile
hist(d$Length)
# housekeeping check
d$ObjPosCentre<- d$X1 > CamSetup$PixDimX*0.25 & d$X1< CamSetup$PixDimX*0.75
hist(d$Length[d$ObjPosCentre], col=rgb(0,0,1,0.5), add=T)
hist(d$Length[!d$ObjPosCentre], col=rgb(1,0,0,0.5), add=T)
# objects away from the centre of the image are smaller

# plot heights and distance to the centre of the image
d$dist2centre<-abs(d$X1-(CamSetup$PixDimX/2))
plot(d$Length~d$dist2centre)
abline(lm(d$Length~d$dist2centre), col = "RED")
# significant bias in size by position in image, the trend is negative
# notice how after 1500 the data looks pinched, this means there is a large underestimation when objects are on the far edges of the image

summary(lm(d$Length~d$dist2centre))
#highly significant

# considering just the centre annotations - trend is not significant
summary(lm(d$Length[d$ObjPosCentre]~d$dist2centre[d$ObjPosCentre]))

# considering just the out annotations - trend is very highly significant
summary(lm(d$Length[!d$ObjPosCentre]~d$dist2centre[!d$ObjPosCentre]))



# we might just have to categorically exclude some data, it looks pinched after 1500
#lets see how much data we lose
d_centre <- d %>% filter(dist2centre < 1500) #4655

# plot heights and distance to the centre of the image
d_centre$dist2centre<-abs(d_centre$X1-(CamSetup$PixDimX/2))
plot(d_centre$Length~d_centre$dist2centre)
abline(lm(d_centre$Length~d_centre$dist2centre), col = "RED")

#Still quite a negative trend
summary(lm(d_centre$Length~d_centre$dist2centre))
#Still quite significant 

#there also seems to be a high outlier at length over 500
boxplot(d_centre$Length)
d_centre <-  d_centre %>% filter(Length < 500)
boxplot(d_centre$Length) # much better

write.csv(d_centre, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height.csv")
