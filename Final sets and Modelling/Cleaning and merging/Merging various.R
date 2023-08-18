# METADATA CLEANING AND ORGANISING

rm(list=ls())
d <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/GHL_station_SOUTH_metadata_processed_updated 2023_with swept areas.csv")
par(mfrow=c(1,1))

#Remove extra columns


d1 <- d[,-c(2,3,4,5,6,7,8,9,10,13,14,18)]

#Load other dataset to start merging
abundance <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/abundancedata.csv")
frames <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Merge Trawling Frames/data.csv")

#Still waiting for height annotations

#change name of column to make them match
names(d1)[names(d1) == "Station"] <- "video_filename"


###### TRAWLING FRAMES ########################
library(tidyverse)
frames <- left_join(frames, d1, by = "video_filename")
frames1 <- frames[,-c(13,14,15,16)]


#Hours * 3600 = times in seconds FOR VIDEO HOURS 
frames1$seconds <- frames1$video_hours*3600
frames1 <- frames1[,-12]


#Turn Seconds of trawling into %
frames1$percalltrawling <- frames1$all*100/frames1$seconds
frames1$pershallowfurrow <- frames1$Wavy.shallow.furrow*100/frames1$seconds
frames1$perdeepfurrow <- frames1$Large.deep.furrow*100/frames1$seconds
frames1$perjagged <- frames1$Jagged.regular.grooves*100/frames1$seconds
frames1$peroverturned <- frames1$Overturned.sediment.rocks*100/frames1$seconds

#clean double columns
frames1<-frames1[,-c(2,3,4,5,6)]



#### SPECIES DENSITY ########################################


abundance <- abundance[,-c(2,7)]
abundance1 <- left_join(abundance, d1, by = "video_filename")
abundance1 <- abundance1[,-c(14,13,12,11)]

abundance1$density.acanella <- abundance1$Acanella.arbuscula/abundance1$swept_area_in_vid_m2
abundance1$density.halipteris <- abundance1$Halipteris.finmarchica/abundance1$swept_area_in_vid_m2
abundance1$density.anthoptilum <- abundance1$Anthoptilum.grandiflorum/abundance1$swept_area_in_vid_m2
abundance1$density.asconema <- abundance1$Asconema.foliatum/abundance1$swept_area_in_vid_m2

#Because the swept are is not the same in every video, the abundance count data is not comparable.
#Thus we need to normalise the data to use it for modelling

hist(abundance1$swept_area_in_vid_m2)
summary(abundance1$swept_area_in_vid_m2) #median 475,09 AND mean is 470.09
boxplot(abundance1$swept_area_in_vid_m2)

#you need to divide the swept are with the mean, and mutliply the result for the count of organisms
# Create an empty vector to store the result
abundance1$normalise_acanella <-NA
abundance1$normalise_asconema <-NA
abundance1$normalise_anthoptilum <-NA
abundance1$normalise_halipteris <-NA

#normalise by mean
abundance1$normalise_acanella <- (abundance1$swept_area_in_vid_m2/470.09)*abundance1$Acanella.arbuscula
abundance1$normalise_anthoptilum <- (abundance1$swept_area_in_vid_m2/470.09)*abundance1$Anthoptilum.grandiflorum
abundance1$normalise_asconema <- (abundance1$swept_area_in_vid_m2/470.09)*abundance1$Asconema.foliatum
abundance1$normalise_halipteris <- (abundance1$swept_area_in_vid_m2/470.09)*abundance1$Halipteris.finmarchica
#double checked a few by eye and looks about right to me!
abundance1[is.na(abundance1)] <- 0

# Get some basic stats to include in the results
median(abundance1$normalise_acanella) # Acanella 10.4645
median(abundance1$normalise_asconema) # Asconema 1.162723
median(abundance1$normalise_anthoptilum) # anthp 1.799687
median(abundance1$normalise_halipteris) # Hlipteris 0




###  MISSING THE CODE WHERE I ROUND UP TO NEAREST WHOLE NUMBER!!!!


abundance1$normalise_acanella <- round(abundance1$normalise_acanella, digits = 0)
abundance1$normalise_asconema <- round(abundance1$normalise_asconema, digits = 0)
abundance1$normalise_anthoptilum <- round(abundance1$normalise_anthoptilum, digits = 0)
abundance1$normalise_halipteris <- round(abundance1$normalise_halipteris, digits = 0)

####################
# turn NAs into zero 

frames1[is.na(frames1)] <- 0
write.csv(abundance1,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")
write.csv(frames1,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv")


######### PRELIMINERY PLOTS #############

# mainly depth and temp versus species and versus trawling types
# Also species vs trawling
par(mfrow = c(2, 2))
plot(abundance1$mean_depth~ abundance1$normalise_acanella )
plot(abundance1$mean_depth~ abundance1$normalise_asconema)
plot(abundance1$mean_depth~ abundance1$normalise_halipteris )
plot(abundance1$mean_depth~ abundance1$normalise_anthoptilum) 
#not much going on 

plot(abundance1$best_temp~ abundance1$normalise_acanella )
plot(abundance1$best_temp~ abundance1$normalise_asconema)
plot(abundance1$best_temp~ abundance1$normalise_halipteris )
plot(abundance1$best_temp~ abundance1$normalise_anthoptilum)
#not much going on neither


# To plot the trawling marks against the abundance you have to include the rest of the trawling stations 
abundance_trawl <- full_join(abundance1, frames1, by= c("video_filename", "mean_depth" = "mean_depth", "best_temp" = "best_temp", "sled_mid_point_lng" = "sled_mid_point_lng", "sled_mid_point_lat"))
abundance_trawl <- abundance_trawl[,-c(7,20)]
#This data set is the one we will use to compare trawling evidence on the seafloor with abundance data

#substituting with zeros

abundance_trawl[is.na(abundance_trawl)] <- 0
write.csv(abundance_trawl,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")



#doing some exploratory plots
par(mfrow = c(2, 2))
plot(  abundance_trawl$density.acanella~abundance_trawl$percalltrawling )
plot(  abundance_trawl$density.asconema~abundance_trawl$percalltrawling )
plot( abundance_trawl$density.anthoptilum ~abundance_trawl$percalltrawling )
plot( abundance_trawl$density.halipteris~abundance_trawl$percalltrawling  )

# a bit hard to look at and visualise, might log transform and + 1 at the end of the response variable

