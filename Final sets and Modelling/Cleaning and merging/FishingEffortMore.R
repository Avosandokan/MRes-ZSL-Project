#Trawling Effort model cleaning and prepping

rm(list=ls())
effort <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/GHL_station_SOUTH_trawl_effort.csv")

#one column has numeric and character, get rid of characters
effort$years_since_trawled <- as.numeric(effort$years_since_trawled)
#substitute NAs with 18 (years since last trawled at least)
effort[is.na(effort)] <- 18

#clean extra column and rename to merge
effort <- effort[,-2]
names(effort)[names(effort) == "Station"] <- "video_filename"

##### COUNT #####
# on count and abundance data
count <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")
#already merged ?
count <- count[,-c(1, 74, 75)]
write.csv(count, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")

#### TRAWLING SCARS####
#on trawling scars on the seafloor
frames <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv")
frames <- frames[,-1]

#split the columns we need to merge them to frames dataset
c <- count[,c(1,6:9)]
#paste the columns into the frames trawling data according to the stations
frames <- full_join(frames, c, by = c("video_filename", "mean_depth" = "mean_depth", "best_temp" = "best_temp", "sled_mid_point_lng" = "sled_mid_point_lng", "sled_mid_point_lat"))
#merge effort data
frames <- full_join(frames, effort, by = c("video_filename", "sled_mid_point_lng" , "sled_mid_point_lat"))
#turn Nas into 0
frames[is.na(frames)] <- 0
 write.csv(frames,"C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv" )
