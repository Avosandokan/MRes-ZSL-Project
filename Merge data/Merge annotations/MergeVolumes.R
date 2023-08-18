# MERGING All volumes from all years based on annotation types
# 1. Points for Abundance data | 2, Lines for height | 3. Whole frames for trawling evidence

rm(list = ls())
library(dplyr)
library(tidyverse)

#### IMPORT #####

#2017
df_frames_2017_PA_07 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/df_frames_2017_PA_07.csv")
df_points_2017_PA_07 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/df_points_2017_PA_07.csv")
df_lines_2017_PA_07 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2017_PA_07/df_lines_2017_PA_07.csv")

df_frames_2017_PA_07 <- as.data.frame(df_frames_2017_PA_07)
df_points_2017_PA_07 <- as.data.frame(df_points_2017_PA_07)
df_lines_2017_PA_07 <- as.data.frame(df_lines_2017_PA_07)

#2018
df_frames_2018_SA_11 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/df_frames_2018_SA_11.csv")
df_points_2018_SA_11 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/df_points_2018_SA_11.csv")
df_lines_2018_SA_11 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2018_SA_11/df_lines_2018_SA_11.csv")

df_frames_2018_SA_11 <- as.data.frame(df_frames_2018_SA_11)
df_points_2018_SA_11 <- as.data.frame(df_points_2018_SA_11)
df_lines_2018_SA_11 <- as.data.frame(df_lines_2018_SA_11)

#2019
df_frames_2019_HM_04 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/df_frames_2019_HM_04.csv")
df_points_2019_HM_04 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/df_points_2019_HM_04.csv")
df_lines_2019_HM_04 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2019_HM_04/df_lines_2019_HM_04.csv")

df_frames_2019_HM_04 <- as.data.frame(df_frames_2019_HM_04)
df_points_2019_HM_04 <- as.data.frame(df_points_2019_HM_04)
df_lines_2019_HM_04 <- as.data.frame(df_lines_2019_HM_04)

#2022-02
df_frames_2022_TA_02 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/df_frames_2022_TA_02.csv")
df_lines_2022_TA_02 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/df_lines_2022_TA_02.csv")
df_points_2022_TA_02 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_02/df_points_2022_TA_02.csv")

df_frames_2022_TA_02 <- as.data.frame(df_frames_2022_TA_02)
df_lines_2022_TA_02 <- as.data.frame(df_lines_2022_TA_02)
df_points_2022_TA_02 <- as.data.frame(df_points_2022_TA_02)

#2022-08
df_frames_2022_TA_08 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_08/df_frames_2022_TA_08.csv")
df_points_2022_TA_08 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_08/df_points_2022_TA_08.csv")
df_lines_2022_TA_08 <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/97_csv_video_annotation_report/2022_TA_08/df_lines_2022_TA_08.csv")

df_frames_2022_TA_08 <- as.data.frame(df_frames_2022_TA_08)
df_points_2022_TA_08 <- as.data.frame(df_points_2022_TA_08)
df_lines_2022_TA_08 <- as.data.frame(df_lines_2022_TA_08)

#### MERGE #####

# df_points <- bind_rows(df_points_2017_PA_07, df_points_2018_SA_11, df_points_2019_HM_04, df_points_2022_TA_02, df_points_2022_TA_08)
# Warning ! can't combine created_at column

#RE formatting as numeric (the format was weird)
df_frames_2017_PA_07$created_at <- as.numeric(df_frames_2017_PA_07$created_at)
df_points_2017_PA_07$created_at <- as.numeric(df_points_2017_PA_07$created_at)
df_lines_2017_PA_07$created_at <- as.numeric(df_lines_2017_PA_07$created_at)
df_frames_2018_SA_11$created_at <- as.numeric(df_frames_2018_SA_11$created_at)
df_points_2018_SA_11$created_at <- as.numeric(df_points_2018_SA_11$created_at)
df_lines_2018_SA_11$created_at <- as.numeric(df_lines_2018_SA_11$created_at)
df_frames_2019_HM_04$created_at <- as.numeric(df_frames_2019_HM_04$created_at)
df_points_2019_HM_04$created_at <- as.numeric(df_points_2019_HM_04$created_at)
df_lines_2019_HM_04$created_at <- as.numeric(df_lines_2019_HM_04$created_at)
df_frames_2022_TA_02$created_at <- as.numeric(df_frames_2022_TA_02$created_at)
df_lines_2022_TA_02$created_at <- as.numeric(df_lines_2022_TA_02$created_at)
df_points_2022_TA_02$created_at <- as.numeric(df_points_2022_TA_02$created_at)
df_frames_2022_TA_08$created_at <- as.numeric(df_frames_2022_TA_08$created_at)
df_lines_2022_TA_08$created_at <- as.numeric(df_lines_2022_TA_08$created_at)
df_points_2022_TA_08$created_at <- as.numeric(df_points_2022_TA_08$created_at)

df_points <- bind_rows(df_points_2017_PA_07, df_points_2018_SA_11, df_points_2019_HM_04, df_points_2022_TA_02, df_points_2022_TA_08)
#All good it works now!

df_lines <- bind_rows(df_lines_2017_PA_07, df_lines_2018_SA_11, df_lines_2019_HM_04, df_lines_2022_TA_02, df_lines_2022_TA_08)

df_frames <- bind_rows(df_frames_2017_PA_07, df_frames_2018_SA_11, df_frames_2019_HM_04, df_frames_2022_TA_02, df_frames_2022_TA_08)


#### COLUMNS CLEANING #####

#Whole frames DELETE column n. = 1,2,4,5,7,8,10,12,14,15
df_frames <- df_frames[, -c(1,2,4,5,7,8,10,12,14,15)]

#Lines DELETE column n. = 1,2,4,5,7,8,10,12,13,14,15
df_lines <- df_lines[, -c(1,2,4,5,7,8,10,12,13,14,15)]

#Points DELETE column n. = 1,2,4,5,7,8,10,12,13,14,15
df_points <- df_points[,-c(1,2,4,5,7,8,10,12,13,14,15)]


#Get rid of the "_usablesegments_midline.MP4" at the end of the Video File name column
df_frames$video_filename <- substr(df_frames$video_filename, #column that we are selecting
                                   1, 14) #includes digit from the 1st to the 14th
df_points$video_filename <- substr(df_points$video_filename,1, 14)
df_lines$video_filename <- substr(df_lines$video_filename, 1, 14)


#Export point annotations
write.csv(df_points, "C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/df_points.csv", row.names=FALSE)

df_frames <- df_frames[, -c(6,7)] #cleaning extra columns
# Double checked, Frame duration is correct for the single frame annotations!

write.csv(df_frames, "C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/df_frames.csv", row.names=FALSE)


### HEIGHT FROM LINE will be done in a separate Script ####

write.csv(df_lines, "C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/df_lines.csv", row.names=FALSE)
