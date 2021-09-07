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

### GETTING TOP SPLIT SO/SO
ggplot(df, aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-0,slope=.97, color='red')

summary(raw$TEAM_BATTING_SO)

df_split_top<-df%>%filter(TEAM_BATTING_SO>(.97*TEAM_PITCHING_SO))
df_split_top<-df_split_top%>%mutate(AB=162*27+TEAM_BATTING_H+TEAM_BATTING_BB)
df_split_top<-df_split_top%>%mutate(B_NO_OUT=TEAM_BATTING_H+TEAM_BATTING_BB)
df_split_top<-df_split_top%>%mutate(B_OBP=B_NO_OUT/AB)
df_split_top<-df_split_top%>%mutate(B_1B=TEAM_BATTING_H-TEAM_BATTING_2B-TEAM_BATTING_3B-TEAM_BATTING_HR)
df_split_top<-df_split_top%>%mutate(B_TB=B_1B+2*TEAM_BATTING_2B+3*TEAM_BATTING_3B+4*TEAM_BATTING_HR)
df_split_top<-df_split_top%>%mutate(B_SLG=B_TB/AB)
df_split_top<-df_split_top%>%mutate(B_OPS=B_OBP+B_SLG)

#ggplot(df_split, aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-0,slope=.97, color='red')
c<-cor(df_split_top)
df_batting_only=df_split_top%>%select(TARGET_WINS,B_OBP,B_OPS,B_1B,TEAM_BATTING_2B,TEAM_BATTING_3B,TEAM_BATTING_HR,TEAM_BATTING_BB)
ggpairs(df_batting_only)
lm1<-lm(TARGET_WINS~B_OBP+TEAM_BATTING_SO+TEAM_PITCHING_SO+TEAM_FIELDING_E, data=df_split_top)
summary(lm1)



### GETTING 2ND SPLIT SO/SO
ggplot(df, aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-0,slope=.97=, color='red')

-

#REMOVING TOP FROM DATA
df_split_2<-df%>%filter(TEAM_BATTING_SO<(.97*TEAM_PITCHING_SO))
ggplot(df_split_2, aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-30,slope=.95, color='red')
#SPLITTING OUT 2
df_split_2<-df%>%filter(TEAM_BATTING_SO>(-30+.95*TEAM_PITCHING_SO))
#CHECKING SPLIT
ggplot(df_split_2, aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-30,slope=.95, color='red')
df_split_2<-df_split_2%>%mutate(AB=162*27+TEAM_BATTING_H+TEAM_BATTING_BB)
df_split_2<-df_split_2%>%mutate(B_NO_OUT=TEAM_BATTING_H+TEAM_BATTING_BB)
df_split_2<-df_split_2%>%mutate(B_OBP=B_NO_OUT/AB)
df_split_2<-df_split_2%>%mutate(B_1B=TEAM_BATTING_H-TEAM_BATTING_2B-TEAM_BATTING_3B-TEAM_BATTING_HR)
df_split_2<-df_split_2%>%mutate(B_TB=B_1B+2*TEAM_BATTING_2B+3*TEAM_BATTING_3B+4*TEAM_BATTING_HR)
df_split_2<-df_split_2%>%mutate(B_SLG=B_TB/AB)
df_split_2<-df_split_2%>%mutate(B_OPS=B_OBP+B_SLG)

#ggplot(df_split, aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-0,slope=.97, color='red')
c<-cor(df_split_2)
ggpairs(df_split_2)
 lm1<-lm(TARGET_WINS~., data=df_split_2)
summary(lm1)

lm_total<-lm(TARGET_WINS~.,data=raw)
summary(lm_total)
lm_total<-lm(TARGET_WINS~.-TEAM_PITCHING_SO-TEAM_BASERUN_CS-TEAM_BATTING_HR-TEAM_BATTING_3B-TEAM_BATTING_2B-TEAM_PITCHING_BB-TEAM_PITCHING_H-TEAM_BASERUN_SB-TEAM_BATTING_HBP,data=raw)
summary(lm_total)

ggplot(d)