# 2017 PA 07 VIDEO - apparentely only my annotations present
rm(list = ls())

library(dplyr)
library(tidyverse)
library(readr) 

Isa <-  read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/2110-isabella-mele.csv")
#turn into dataframe
Isa<- as.data.frame(Isa) 
str(Isa)

# 1)) ## SEPARATE DATA INTO ANNOTATION TYPES ######

df_points <- Isa[Isa$shape_name == "Point",]
#keep every row with shape name point 

df_lines <-Isa[Isa$shape_name == "LineString",]
#Keep every row with shape name line string

df_frames <- Isa[Isa$shape_name == "WholeFrame",]
#keep every row with shape name whole frame for trawling marks



# 2)) ### SEPARATE X AND Y COORDINATES #####

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

boxplot(df_points$Y) #one outlier at 350 (has to be removed)
boxplot(df_lines$Y1) #OK
df_points <- filter(df_points, Y > 400) 
boxplot(df_points$Y)#another one at 480, after double checking in Biiglem this appears out of range
df_points <- filter(df_points, Y > 500) # thus removed
boxplot(df_points$Y) # now OK


#export data
write.csv(df_frames, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/df_frames_2017_PA_07.csv", row.names=FALSE)
write.csv(df_points, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/df_points_2017_PA_07.csv", row.names=FALSE)
write.csv(df_lines, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/df_lines_2017_PA_07.csv", row.names=FALSE)
