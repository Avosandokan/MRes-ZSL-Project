# Sum total trawling by TYPE of mark and group by station
#in other words, how much of the video has grooves/furrows? 

#use df_frames

#rename column
names(trawlsum)[names(trawlsum) == "sum(duration)"] <- "duration"
trawlsum$mark_type <- "all"

#then we need to work with the data frame called d

#make a copy just in case
d_bis <- d

#change name of column to make them match
names(d_bis)[names(d_bis) == "label_name"] <- "mark_type"

#calculate the total trawling mark presence based on the scare type present on the seafloor
d_bis <- d_bis %>%
  group_by(video_filename, mark_type) %>% 
  summarise(sum(frames.duration))

#rename column
names(d_bis)[names(d_bis) == "sum(frames.duration)"] <- "duration"

#This final data frame will have the total number of seconds that each trawling mark type has
finaltrawlmarks <- rbind(trawlsum,d_bis)

#############################

# Now you have to make it into a pivot wider type of format, where one trawling mark is each column with the respective duration.
#this is necessary later to do the analysis against fishing effort data.

finaltrawlmarks <- finaltrawlmarks %>% pivot_wider(names_from = mark_type, values_from = duration)
write.csv(finaltrawlmarks, "C:/Users/iavom/Desktop/MRes project/Data and R/data.csv", row.names=F)

