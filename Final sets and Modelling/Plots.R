####### PLOTS

# Lets first start ith plots for the length (height) of the species
# to run plots we need the full length, without the median, minimum and maximum because the boxplot function does it anyways 


rm(list=ls())
library(tidyverse)
library(ggplot2)

##########################################################
# LENGTH (MM) ESTIMATES 
##########################################################


# Need to import height and trawling data in the long format to do the violin plots
height <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/height.csv")
trawl <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv")

#select columns to merge
d <- trawl[,c(2,3,6,7,9:14, 16)]
#clean extra cols
height <- height[,-c(1,3)]
boxplot(height$Length) # there is a negative value
height <- height %>% filter(Length > 0)
boxplot(height$Length) # OK

# Check that the distance to centre is not biased
par(mfrow = c(1, 1))
plot(height$dist2centre) # it is already cut to distance 2 centre not greater than 1500

#merge the data
height_plot <- inner_join(height, d, by = "video_filename")

height_plot$years_since_rounded <- NA
height_plot$years_since_rounded <- ifelse(height_plot$years_since_trawled >= 0 & height_plot$years_since_trawled <= 6, 6,
                                 #if the years are between 0 and 4, then print 4 in the new "column"years since rounded"
                                 ifelse(height_plot$years_since_trawled >= 7 & height_plot$years_since_trawled <= 17,17,
                                  18)
                                        )
                                        # if none of the cases above happens, the print 18 because its been trawled longer than 18


# Distinguish each species 
aa <- height_plot %>% filter(label_name == "Acanella arbuscula")
#52 acanellas can be split in
af <- height_plot%>% filter(label_name == "Asconema foliatum")
# 38 acanellas can be split in
ag <- height_plot %>% filter(label_name == "Anthoptilum grandiflorum")
#40 anthoptilum can be split by 8 into 6 groups
hf <- height_plot %>% filter(label_name == "Halipteris finmarchica")


# Run violin plot
ggplot(aa, aes(y=Length, x=as.factor(years_since_rounded))) + geom_violin() + 
  geom_boxplot(width=0.1)+ 
  theme( panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) 

ggplot(af, aes(y=Length, x=as.factor(years_since_rounded))) + geom_violin() + 
  geom_boxplot(width=0.1)+ 
  theme( panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) 

ggplot(ag, aes(y=Length, x=as.factor(years_since_rounded))) + geom_violin() + 
  geom_boxplot(width=0.1)+ 
  theme( panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) 


# because of the data dostribution you need to further reduce HF
hf$years_since_rounded <- ifelse(hf$years_since_rounded >= 0 & hf$years_since_rounded  <= 17, 17, 18)
ggplot(hf, aes(y=Length, x=as.factor(years_since_rounded))) + geom_violin() + 
  geom_boxplot(width=0.1)+ 
  theme( panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) 

s1 <- aa %>% filter(aa$years_since_rounded == 6) # 61
s2 <- aa %>% filter(aa$years_since_rounded == 17)  # 71
s3 <- aa %>% filter(aa$years_since_rounded == 18) # 2020

s11<- ag %>% filter(ag$years_since_rounded == 6) # 60
s12 <- ag %>% filter(ag$years_since_rounded == 17) # 65
s13<- ag %>% filter(ag$years_since_rounded == 18) # 160

s2<- af %>% filter(af$years_since_rounded == 6) # 20
s22 <- af %>% filter(af$years_since_rounded == 17) # 9
s23<- af %>% filter(af$years_since_rounded == 18) #127




# 
# 
# #create new data to save the summarise
# d2 <- height %>%
#   group_by(label_name, video_filename) %>%
#   summarize(median_length = median(Length, na.rm = TRUE),
#             minimum = min(Length),
#             maximum = max(Length))
# 
# d1 <- inner_join(d, d2, by = "video_filename")
# 
# # BEFORE WE PLOt... we will need to draw maps and plots later on, so lets write a function that makes our years since trawling more visually appealing
# # We probably need to group the years since trawling in a more presentable way
# #for example by grouping 0-6 and 7-12 and 13-18
# 
# #write an empty vector for rounded values 
# #d1$years_since_rounded <- NA
# #### cannot make fact grid work, so i will have to do it individualy instead
# 
# d1 <- na.omit(d1)
# aa <- d1 %>% filter(label_name == "Acanella arbuscula")
# #52 acanellas can be split in
# af <- d1 %>% filter(label_name == "Asconema foliatum")
# # 38 acanellas can be split in
# ag <- d1 %>% filter(label_name == "Anthoptilum grandiflorum")
# #40 anthoptilum can be split by 8 into 6 groups
# hf <- d1 %>% filter(label_name == "Halipteris finmarchica")
# #17 halipteris can be split / 4 groupshttp://127.0.0.1:14595/graphics/plot_zoom_png?width=852&height=420
# 
# #To have a whisker plot that is evenly distributed, we need to do split the number of observations by equal parts
# # Probably the best idea is to group them every 11 values because we have 44 observations in total
# # I will do this in excel....
# write.csv(aa, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/aa_plotlength.csv")
# write.csv(af, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/af_plotlength.csv")
# write.csv(ag, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/ag_plotlength.csv")
# write.csv(hf, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/hf_plotlength.csv")
# 
# aa<- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/aa_plotlength.csv")
# af <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/af_plotlength.csv")
# ag <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/ag_plotlength.csv")
# hf <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/hf_plotlength.csv")
# 
# par(mfrow = c(2, 2))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #because many species also have a relationship with depth
# par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
#because there is too much data, we will need to get the median length to be able to plot results

#

###### TRYING TO DO A SCATTER WITH FIT LINE

# #get rid of NAs
# height$median_length[is.na(height$median_length)] <- 0
# # filter zeros out
# height <- height %>% filter(median_length > 0)
# 
# 
# # create a colormblind palette
# colorBlindBlack8  <- c("Black", "#E69F00", 
#                        "#0072B2", "#D55E00")
# 
# 
# fakeyears <- data.frame(years_since_trawled=seq(from = 0, to = 18, by = 0.1))
# hf_length <- lm(data = hf, log(median_length+1) ~ years_since_trawled)
# p <- predict(hf_length, newdata = fakeyears, type="response") #log transf
# predicted_length <- exp(p)-1
# 
# 
# summary(hf_length)
# 
# class(fakeyears)
# 
# 
# 
# plot(x = height$years_since_trawled, y = height$median_length,
#   xlab = "Years since trawling",ylab = "Median length (mm)",
#   pch=16, # solid dots increase the readability of this data plot
#   col = colorBlindBlack8[diamonds$color])
# legend("topleft", legend=unique(height$label_name), col = 1:4, pch = 16)
# 
# abline(lm(data=aa, median_length ~ years_since_trawled), col = "Black", lwd = 2)
# abline(lm(data = af, median_length ~ years_since_trawled), col = "#E69F00", lwd = 2)
# abline(lm(data = ag,median_length ~ years_since_trawled), col = "#D55E00", lwd = 2)
# 
# 
# abline(predicted_length, fakeyears$years_since_trawled, col = "black", lwd = 2)
# 

# Not working 
#d1_clean %>%  ggplot(aes(x= years_since_rounded, y = Length  )) + 
    #creates a graph with the clidemia data on X axis 
#   facet_wrap(~label_name, nrow=2, ncol=2) + 
   #tells to separate the four species in the graph
#  geom_boxplot() 
#   facet_grid(Length ~years_since_rounded)




########## ABUNDANCE VERSUS LENGTH #################################

count <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/abundance.csv")
#keep only the columns we want
count <- count[,c(2:6)]
count <- count %>% pivot_longer(cols = "Acanella.arbuscula":"Halipteris.finmarchica",
                                names_to = "label_name",
                                values_to = "abundance")
#get rid of zeros so length matches
count <- count %>% filter(abundance >0)

# get median height
d2 <- height %>%
  group_by(label_name, video_filename) %>%
  summarize(median_length = median(Length, na.rm = TRUE),
            minimum = min(Length),
            maximum = max(Length)) 


dmedianlength <- d1[,c(1,12,13)]



####################################################################
# TRAWLING SCARS 
####################################################################

d_trawl <- read.csv("C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/trawling_frames.csv")
d_trawl <- d_trawl[,-1]

#we might nee dto pivot longer the data
dpivot <- d_trawl %>% pivot_longer(cols = "percalltrawling":"peroverturned",
                             names_to = "type",
                             values_to = "perc") 

library(ggpattern)
par(mfrow = c(1,1))
plot(data=dpivot, perc ~ total_effort, 
     xlab="Trawling effort", ylab = "% Trawling evidence",
     col = as.factor(dpivot$type), pch = 16)

# Group years since trawled as by other plot
dpivot$years_since_rounded <- NA
#After running preliminary plots I decide to further divide the grouping by 4 units (every 4 years)
dpivot$years_since_rounded <- ifelse(dpivot$years_since_trawled >= 0 & dpivot$years_since_trawled <= 4, 4,
                                 #if the years are between 0 and 4, then print 4 in the new "column"years since rounded"
                                 ifelse(dpivot$years_since_trawled >= 5 & dpivot$years_since_trawled <= 8,8,
                                        #if they are between 5 and 8, the print 8
                                        ifelse(dpivot$years_since_trawled >= 9 & dpivot$years_since_trawled <= 12,12, 
                                               #if between 8 and 12 then print 12
                                               ifelse(dpivot$years_since_trawled >= 13 & dpivot$years_since_trawled <= 16,16, 
                                                      18)
                                        ) 
                                        # if none of the cases above happens, the print 18 because its been trawled longer than 18 
                                 ) ) 
#clan all these extra columns...annoying
#dpivot<-dpivot[,c(1:10,57:58)]
write.csv(dpivot, "C:/Users/iavom/Desktop/MRes project/Data and R/Final sets/Plotting/trawling_plotting.csv")


#Lets try make a nice bar plot ( on y axis the perc % and on x the grouped by trawling type factor on years
ggplot(dpivot, aes(x = years_since_rounded, y = perc, fill = type)) + 
  geom_col(position="dodge")


ggplot(dpivot, aes(x=as.factor(years_since_rounded), y=perc, fill= type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Type",
                      values=c("black", "Grey", "blue", "green", "yellow"),
                      labels=c("percalltrawling"="All trawling", 
                               "perdeepfurrow"="Deep furrow",
                               "perjagged"="Jagged grooves",
                               "peroverturned"="Overturned rocks",
                               "pershallowfurrow"="Shallow furrow")) +
  xlab("Years since last trawling event") + ylab("Percentage (%) in video") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


library(ggpattern)
ggplot(dpivot, aes(x = as.factor(years_since_rounded), y = perc, fill = type)) +
  geom_bar_pattern(stat = "identity", position = "dodge", pattern_fill = "black", pattern_angle = 45) +
  scale_fill_manual(name = "Type",
                    values = c("percalltrawling" = "black",
                               "perdeepfurrow" = fill_pattern("stripe"),
                               "perjagged" = fill_pattern("horizontal_lines"),
                               "peroverturned" = fill_pattern("vertical_lines"),
                               "pershallowfurrow" = fill_pattern("diagonal_lines"))) +
  xlab("Years since last trawling event") + ylab("Percentage (%) in video") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# looks rather ugly 
## LETS try to fill by pattern
  ggplot(dpivot, aes(x=as.factor(years_since_rounded), y=perc, fill=type)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Years since last trawling event") + ylab("Percentage (%) in video") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


  ggplot(dpivot, aes(years_since_trawled, perc)) +
    geom_bar_pattern(
      aes( pattern = type, 
        pattern_angle = type ), 
      fill            = 'white', 
      colour          = 'black',
      pattern_spacing = 0.025
    ) +
    theme_bw() +
    labs(
      title    = "ggpattern::geom_col_pattern()",
      subtitle = 'geometry-based patterns'
    ) +
    scale_pattern_spacing_discrete(range = c(0.01, 0.05)) + 
    theme(legend.position = 'none') + 
    coord_fixed(ratio = 1)
  
  
  
  
  
#### TRAWLING EVIDENCE VERSUS DEPTH 
  d_trawl$years_since_rounded <- NA
  #After running preliminary plots I decide to further divide the grouping by 4 units (every 4 years)
  d_trawl$years_since_rounded <- ifelse(d_trawl$years_since_trawled >= 0 & d_trawl$years_since_trawled <= 4, 4,
                                       #if the years are between 0 and 4, then print 4 in the new "column"years since rounded"
                                       ifelse(d_trawl$years_since_trawled >= 5 & d_trawl$years_since_trawled <= 8,8,
                                              #if they are between 5 and 8, the print 8
                                              ifelse(d_trawl$years_since_trawled >= 9 & d_trawl$years_since_trawled <= 12,12, 
                                                     #if between 8 and 12 then print 12
                                                     ifelse(d_trawl$years_since_trawled >= 13 & d_trawl$years_since_trawled <= 16,16, 
                                                            18)
                                              ) 
                                              # if none of the cases above happens, the print 18 because its been trawled longer than 18 
                                       ) ) 
  
#make a bar plot
d_trawl$years_since_rounded <- as.factor(d_trawl$years_since_rounded)
d_trawl <- d_trawl %>% filter(percalltrawling != 0)

 pp <-  ggplot(d_trawl, aes(x=as.factor(mean_depth), y=percalltrawling, fill= years_since_rounded)) +
    geom_bar(stat="identity", position="dodge", colour = "black") +
    scale_fill_manual(name="Years",
                      values=c("Grey", "Black", "White", "green", "yellow"),
                      labels=c("4"="0-4", 
                               "8"="5-8",
                               "12"="9-12",
                               "16"="13-17",
                               "18"="+18")) +
    xlab("Depth") + ylab("Percentage (%) in video") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
 #  scale_x_discrete(breaks = c(seq(from = min(as.numeric(d_trawl$mean_depth)), to = max(as.numeric(d_trawl$mean_depth)), by = 300)))
 #                      
 # # omit +
  
                      
                      
                      ifelse(d_trawl$mean_depth <= 604 & d_trawl$mean_depth >= 900, 750,
                                    ifelse(d_trawl$mean_depth  > 900 & d_trawl$mean_depth  <= 1200, 1050,
                                           ifelse(d_trawl$mean_depth> 1200 & d_trawl$mean_depth  <= 1500, 1350,
                                                  1700)
                                           )   ) )
  
  
  
  
  


#### PLOT TRAWL VS YEARS SINCE TRAWLING  ###
### SCATTER
  library(ggpattern)
pivotshort <- dpivot%>%filter(type != "percalltrawling")  

ggplot(pivotshort, aes(years_since_trawled, perc, fill = type)) +
  geom_col_pattern(
    aes(fill=type, pattern=type),
    colour          = 'black', 
    pattern_density = 0.5 ) +
  theme_bw() +
  scale_pattern_manual(values=c('stripe', 'crosshatch', 'weave', 'none'))





 # LETS try to fit the lines from negative binomial regression
 library(MASS)
 library(lme4)
 dpivot$type <- as.factor(dpivot$type)
 modeltrawling <- dpivot %>% filter(type != "percalltrawling") 
   
 model_trial <-glm.nb(data = modeltrawling, perc ~ years_since_trawled + type)
summary(model_trial)
par(mfrow = c(2,2))
plot(model_trial) # looks almost too perfect

 
 
 
 
 
 
 
 
 

