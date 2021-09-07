#final model


#LIBRARIES
library(tidyverse)
library(GGally)
#library(psych)
library(stats)

#RANDOM SEED
set.seed(210904)

#IMPORT
raw <- read.csv("./moneyball-training-data.csv")
raw<-raw%>%select(-INDEX)
#df<-raw%>%filter(between(TEAM_PITCHING_SO,308,1570))
#ggpairs(raw)

df_final<-raw%>%select(TARGET_WINS,TEAM_PITCHING_SO,TEAM_BASERUN_CS,TEAM_BATTING_HR,TEAM_BATTING_3B,TEAM_BATTING_2B,TEAM_PITCHING_BB,TEAM_PITCHING_H,TEAM_BASERUN_SB,TEAM_BATTING_HBP)
df_final<-df_final%>%mutate(TEAM_PITCHING_SO=log(TEAM_PITCHING_SO))
#cut out low homer teams
df_final<-df_final%>%filter(TEAM_BATTING_HR>30)
ggplot(df_final,aes(x=TEAM_BATTING_HR,y=TEAM_PITCHING_SO))+geom_point()
ggpairs(df_final)

lm_total<-lm(TARGET_WINS~.-TEAM_PITCHING_SO-TEAM_BASERUN_CS-TEAM_BATTING_HR-TEAM_BATTING_3B-TEAM_BATTING_2B-TEAM_PITCHING_BB-TEAM_PITCHING_H-TEAM_BASERUN_SB-TEAM_BATTING_HBP,data=raw)
summary(lm_total)

#only selecting data within 2SD of mean


df_no_outliers<-df_final%>%filter()