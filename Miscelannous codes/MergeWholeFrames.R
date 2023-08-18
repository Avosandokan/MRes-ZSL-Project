#d<-read.csv("ExampleVideoAnnotation.csv", stringsAsFactors=F)
# filter to whole frame annotations
#d<-d[d$shape_name=="WholeFrame",]
# check labels
#table(d$label_name)
#table(d$video_filename)

library(readr)
df_frames <- read_csv("C:/Users/iavom/Desktop/MRes project/Data and R/Merged Annotations Data/df_frames.csv")
d <- df_frames

xx<-stringr::str_replace(d$frames,"\\[", "")
xx<-stringr::str_replace(xx,"\\]", "")
xx<-stringr::str_split(xx,",")
d$frames.start<-NA
d$frames.end<-NA
for(i in 1:nrow(d)){
    d$frames.start[i]<-as.numeric(xx[[i]][1])
    if(is.na(xx[[i]][2])){
        d$frames.end[i]<-as.numeric(xx[[i]][1])
    } else {        
        d$frames.end[i]<-as.numeric(xx[[i]][2])
    }
}
d$frames.duration<-d$frames.end-d$frames.start    









# set up output dataframe
out<-data.frame(video_filename=NA, start=NA, end=NA, N=NA)

# loop through stations
for(s in unique(d$video_filename)){
    # filter to just this station
    ds<-d[d$video_filename==s,]
    # loop through entries
    for(i in 1:nrow(ds)){
        # check if time code matches existing record
        x<-which(ds$frames.start[i] <= out$start & out$end <= ds$frames.end[i]   |
                 out$start <= ds$frames.start[i] & ds$frames.end[i] <= out$end   |
                 out$start <= ds$frames.start[i] & ds$frames.start[i] <= out$end |
                 out$start <= ds$frames.end[i]   & ds$frames.end[i] <= out$end   )
        if(length(x)==0){
            match.len<-1
            prev.match.len<-0
            ## haven't done this one yet
            while(match.len!=prev.match.len){
                # find records that overlap with me
                x<-which(ds$frames.start[i] <= ds$frames.start & ds$frames.end <= ds$frames.end[i]  |
                         ds$frames.start <= ds$frames.start[i] & ds$frames.end[i] <= ds$frames.end  |
                         ds$frames.start <= ds$frames.start[i] & ds$frames.start[i] <= ds$frames.end  |
                         ds$frames.start <= ds$frames.end[i] & ds$frames.end[i] <= ds$frames.end )

                # fetch matching records
                dss<-ds[x,]
                # get start and end time
                best.start<-min(dss$frames.start)
                best.end<-max(dss$frames.end)
                prev.match.len<-match.len
                match.len<-nrow(dss)
            }
            # when we get here we've finished searching
            out<-rbind(out, data.frame(video_filename=ds$video_filename[i],
                                       start=best.start, end=best.end, N=match.len))
        }
    }
}


# add duration of segment
out$duration<-out$end-out$start
# sort on video & time code
out<-out[order(out$video_filename, out$start),]

# write to file
write.csv(out, "C:/Users/iavom/Desktop/MRes project/Data and R/MergedAnnotations.csv", row.names=F)
