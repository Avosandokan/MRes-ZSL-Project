## organise the Point count annotation to turn it into abundance data
# now you have to group by video filename and by species -> with a sum count 

library(readr)
library(tidyverse)
library(dplyr)
rm(list = ls())
df_points <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/df_points.csv")


#What is the most occuring annotation?
Mode <- function(df_points) {
  ux <- unique(df_points)
  ux[which.max(tabulate(match(df_points, ux)))]
}
print(Mode)
Mode(df_points$label_name)

#Least occurring?
Minimum_mode <- function(df_points) {
  ux <- unique(df_points)
  ux[which.min(tabulate(match(df_points, ux)))]
}
Minimum_mode(df_points$label_name[df_points$label_name!= "Indet revise"])



abundance <- data.frame
abundance <- df_points %>% group_by(video_filename, label_name) %>%
  count(shape_name)

#pivot wider
abundance <- abundance %>% pivot_wider(names_from = label_name, values_from = n)

write.csv(abundance, "C:/Users/iavom/Desktop/MRes project/Data and R/abundancedata.csv", row.names=F)
