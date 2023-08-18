# 2018 SA 11 VIDEOS
# 2 dataframes from steve and one dataframe from Isabella

rm(list = ls())

#Steve = X109_stephen_long
#Isa = X2110_isabella_mele
library(dplyr)
library(tidyverse)
library(readr) 

Steve <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/109-stephen-long.csv")
Isa <-  read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/2110-isabella-mele.csv")
Steve2 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/109-stephen-long2.csv")

#Steve 2 only contains indet revise and undetermined trawling evidence so we will IGNORE IT from analysis
# !!!! In this volume Steve has also annotated trawling marks ! So make sure to include them during data merging

#turn into dataframe
Isa<- as.data.frame(Isa) 
str(Isa)
Steve <- as.data.frame(Steve)
str(Steve) #now they both are a data frame


# 1)) ### MERGE STEVE AND ISA'S ANNOTATIONS  #####

#This time we have to merge by character to include whole frames
levels(as.factor(Steve$label_name))
complete_df <- rbind(Isa, subset (Steve, 
                                  label_name %in% c("Asconema foliatum", "Acanella arbuscula", "Large deep furrow", "Wavy shallow furrow")))

#checking
levels(as.factor(complete_df$firstname)) #ok
levels(as.factor(complete_df$label_name)) #OK



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

boxplot(df_points$Y) #OK
boxplot(df_lines$Y1) #OK


#export data
write.csv(df_frames, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/df_frames_2018_SA_11.csv", row.names=FALSE)
write.csv(df_points, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/df_points_2018_SA_11.csv", row.names=FALSE)
write.csv(df_lines, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/df_lines_2018_SA_11.csv", row.names=FALSE)

