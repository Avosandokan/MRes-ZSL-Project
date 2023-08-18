# 2019_HM_04 VIDEOS ISABELLA ONLY (apparentely Steve didnt annotate in here)
rm(list = ls()) #clear workspace

library(dplyr)
library(tidyverse)
library(readr) 

#import data and set directory
Isa <-  read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/2110-isabella-mele.csv")
# double check data frame structure
Isa<- as.data.frame(Isa) 
str(Isa)

#No need for merging here in this volume

#Check how many type of annotations we have
levels(as.factor(Isa$label_name))
# all 4 species
#also whole frames of trawling marks
# >>>> Some INDET REVISE which we will keep for now (maybe delete later when all data is merged)

levels(as.factor(Isa$shape_name)) # All three points, lines and wholeframes



# 2)) ## SEPARATE DATA INTO ANNOTATION TYPES ######

df_points <- Isa[Isa$shape_name == "Point",]
#keep every row with shape name point 

df_lines <-Isa[Isa$shape_name == "LineString",]
#Keep every row with shape name line string

df_frames <- Isa[Isa$shape_name == "WholeFrame",]
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
#Warning message:: NAs introduced by coercion in Row 221 in video 2019_HM_04_012 (double check later)
#Y NA should be 756.07. Checked with y on 762 on the video and there is no annotation there
sum(is.na(df_points$Y)) #only one NA
sum(is.na(df_lines$Y2)) #none
sum(is.na(df_lines$Y1)) #none

#replace NA with 756.06
df_points$Y <- df_points$Y %>% replace_na(756.06)
sum(is.na(df_points$Y)) #Okay, value replaced
  
  
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
write.csv(df_frames, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/df_frames_2019_HM_04.csv", row.names=FALSE)
write.csv(df_points, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/df_points_2019_HM_04.csv", row.names=FALSE)
write.csv(df_lines, "C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/df_lines_2019_HM_04.csv", row.names=FALSE)


