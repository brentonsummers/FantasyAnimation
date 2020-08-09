# set working directory
# setwd("C:/Users/Brenton/Desktop/FF_Data")

# import necessary libraries
library(readxl)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

# read in data
#FF2010<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2010.xlsx")
#FF2011<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2011.xlsx")
#FF2012<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2012.xlsx")
#FF2013<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2013.xlsx")
#FF2014<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2014.xlsx")
FF2015<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2015.xlsx")
FF2016<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2016.xlsx")
FF2017<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2017.xlsx")
FF2018<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2018.xlsx")
FF2019<-read_excel("C:/Users/Brenton/Desktop/FF_Data/FF2019.xlsx")

# format files individually to ensure a smooth rbind
#FF2010<-select(FF2010, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
#FF2011<-select(FF2011, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
#FF2012<-select(FF2012, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
#FF2013<-select(FF2013, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
#FF2014<-select(FF2014, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
FF2015<-select(FF2015, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
FF2016<-select(FF2016, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
FF2017<-select(FF2017, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
FF2018<-select(FF2018, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)
FF2019<-select(FF2019, Player, FantPos,Tgt, Rec, RecTDs, FantPt, PPR, PosRank, Year)

# combine all individual files into one dataframe
# fantasydata<-rbind(FF2010, FF2011, FF2012, FF2013, FF2014, FF2015, FF2016, FF2017, FF2018, FF2019)
fantasydata<-rbind(FF2015, FF2016, FF2017, FF2018, FF2019)

# clean data for top 30 WRs
wrdata<-filter(fantasydata, FantPos=="RB")
wrdata<-filter(wrdata, PosRank<=30)
wrdata<-group_by(wrdata, Year)

# make the plot
staticplot = ggplot(wrdata, aes(PosRank, group = PosRank, fill = as.factor(PosRank), color = as.factor(PosRank))) + geom_tile(aes(y = FantPt/2,height = FantPt,
width = 0.9), alpha = 0.8, color = 'red') +
  geom_text(aes(y = 0, label = paste(PosRank, " ")), vjust = 0.2, hjust = 1) +
  scale_y_continuous(labels = scales::comma) +
  guides(color = FALSE, fill = FALSE) +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(Year, transition_length = 1, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Fantasy Points of Top 30 RBs be Year : {closest_state}',  
       subtitle  =  "Created by @BrentStats")


# and save!
animate(anim, nframes = 150,fps = 15,  width = 1200, height = 1000, 
        renderer = gifski_renderer("TopRBs.gif"))

# boxplot for distro
bxplt<-ggplot(wrdata, aes(x=Year, y=FantPt, group=Year)) + 
  geom_boxplot()+theme_minimal()+labs(title="Fantasy Top 30 RBs Distributed by Year", subtitle="Created by @BrentStats")

# remove unneeded variables
rm(FF2010, FF2011, FF2012, FF2013, FF2014, FF2015, FF2016, FF2017, FF2018, FF2019, fantasydata, anim, staticplot)
  