# 2022 TA 02 VIDEOS ISABELLA AND STEVE PREP
rm(list = ls())

library(dplyr)
library(tidyverse)
library(readr) 

#import data and set directory
Steve <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/109-stephen-long.csv")
Isa <-  read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/2110-isabella-mele.csv")

#check data structure
Isa<- as.data.frame(Isa) 
str(Isa)
Steve <- as.data.frame(Steve)
str(Steve)


# 1)) ### MERGE STEVE AND ISA'S ANNOTATIONS  #####
#Each species has a corresponding label_id
# 6284 = Asconema foliatum
# 5458 = Acanella arbuscula
# 5507 = Anthoptilum grandiflorum
# 5624 = Halipteris finmarchica

#set class for merging and selecting
class(Steve$label_id)
Steve$label_id <- as.numeric(Steve$label_id)

# bind columns that have the corresponding values as label id from steve into my data frame
complete_df <- rbind(Isa, subset (Steve, 
                                  label_id %in% c("6284", "5458", "5507", "5624")))

#checking
levels(as.factor(complete_df$firstname)) #ok
levels(as.factor(complete_df$label_name)) #OK
levels(as.factor(complete_df$shape_name)) # All three points, lines and wholeframes


# 2)) ## SEPARATE DATA INTO ANNOTATION TYPES ######

df_points <- complete_df[complete_df$shape_name == "Point",]
#keep every row with shape name point 

df_lines <-complete_df[complete_df$shape_name == "LineString",]
#Keep every row with shape name line string

df_frames <- complete_df[complete_df$shape_name == "WholeFrame",]
#keep every row with shape name whole frame for trawling marks




# 3)) ### SEPARATE X AND Y COORDINATES #####

#For POINT annotations
#create new columns for x and y
df_points$X<-NA
df_points$Y<-NA

# separate x and y coordinates
for(i in 1:nrow(df_points)){  #for every elemnent in the rows of the data frame
  #delete brakets
  p<-stringr::str_replace(df_points$points[i],"\\[\\[","") 
  p<-stringr::str_replace(p,"\\]\\]","")
  #separate by comma
  p4<-stringr::str_split(p,",")
  #assign first value to x and the second one to y
  df_points$X[i]<-as.numeric(p4[[1]][1])
  df_points$Y[i]<-as.numeric(p4[[1]][2])
}


# # For LINESTRING annotations, here we need an X/y for the bottom and top points (hence 4 coordinates)
df_lines$X1<-NA
df_lines$Y1<-NA
df_lines$X2 <- NA
df_lines$Y2 <- NA

# separate x and y coordinates
for(i in 1:nrow(df_lines)){  #for every elemnent in the rows of the data frame
  #delete brakets
  p<-stringr::str_replace(df_lines$points[i],"\\[\\[","") 
  p<-stringr::str_replace(p,"\\]\\]","")
  #separate by comma
  p4<-stringr::str_split(p,",")
  df_lines$X1[i]<-as.numeric(p4[[1]][1])
  df_lines$Y1[i]<-as.numeric(p4[[1]][2])
  df_lines$X2[i]<-as.numeric(p4[[1]][3])
  df_lines$Y2[i]<-as.numeric(p4[[1]][4])
}



# 4)) ### QUALITY CONTROL OF Y COORDINATES #####

boxplot(df_points$Y) #looks alright to me
boxplot(df_lines$Y1) #OK

# Save dataframes to be able to merge them by annotation later
df_frames_2022_TA_02 <- df_frames
df_points_2022_TA_02 <- df_points
df_lines_2022_TA_02 <- df_lines

#export data
write.csv(df_frames_2022_TA_02, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/df_frames_2022_TA_02.csv", row.names=FALSE)
write.csv(df_points_2022_TA_02, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/df_points_2022_TA_02.csv", row.names=FALSE)
write.csv(df_lines_2022_TA_02, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/df_lines_2022_TA_02.csv", row.names=FALSE)
