#TEAM_BASERUNNING_SB/CS exploratory analysis
library(tidyverse)
library(ggplot)
library(visdat)
library(corrplot)
dat<-read.csv('.\\data\\moneyball-training-data.csv',header = TRUE)%>%select(-INDEX)
dat<-dat%>%select(-TEAM_BATTING_HBP)
visdat::vis_miss(dat,sort_miss = TRUE)


#deselect data missing CS/ SB

dat<-dat%>%filter(!is.na(TEAM_BASERUN_CS & TEAM_BASERUN_SB))
visdat::vis_miss(dat,sort_miss = TRUE)
sum(is.na(dat$TEAM_BASERUN_CS))




#create net stolen bases

dat<-dat%>%mutate(TEAM_NET_SB=TEAM_BASERUN_SB-TEAM_BASERUN_CS)
#check correlation
correlation<-cor(dat)
corrplot.mixed(correlation,tl.col = 'black',tl.pos = 'lt')

ggplot(dat, aes(x=TEAM_NET_SB,y=TARGET_WINS))+geom_point()

cor(dat$TARGET_WINS,dat$TEAM_NET_SB)

0.15392132 + 0.02240407
