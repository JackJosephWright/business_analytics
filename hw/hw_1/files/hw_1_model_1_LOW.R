
#jacks try
#LIBRARIES
library(tidyverse)
library(GGally)
library(psych)
library(stats)

#RANDOM SEED
set.seed(210904)

#IMPORT
setwd('C:/Program Files/GitHub/business_analytics')
raw <- read.csv("./data/moneyball-training-data.csv")%>%select(-INDEX)
raw<-raw%>%select(-TEAM_BATTING_HBP)

#split data


df<-raw%>%mutate(TEAM_BATTING_AB=162*27+TEAM_BATTING_H+TEAM_BATTING_BB,.before=TEAM_BATTING_H)

df<-df%>%mutate(TEAM_BATTING_NO_OUT=TEAM_BATTING_H+TEAM_BATTING_BB,.before=TEAM_BATTING_H)
df<-df%>%mutate(TEAM_BATTING_OBP=TEAM_BATTING_NO_OUT/TEAM_BATTING_AB,.before=TEAM_BATTING_H)
df<-df%>%mutate(TEAM_BATTING_1B=TEAM_BATTING_H-TEAM_BATTING_2B-TEAM_BATTING_3B-TEAM_BATTING_HR,.after=TEAM_BATTING_H)
df<-df%>%mutate(TEAM_BATTING_TOTAL_BASES=TEAM_BATTING_1B+2*TEAM_BATTING_2B+3*TEAM_BATTING_3B+4*TEAM_BATTING_HR,.before=TEAM_BATTING_H)
df<-df%>%mutate(TEAM_BATTING_SLG=TEAM_BATTING_TOTAL_BASES/TEAM_BATTING_AB,.after=TEAM_BATTING_OBP)
df<-df%>%mutate(TEAM_BATTING_OPS = TEAM_BATTING_OBP+TEAM_BATTING_SLG,.after=TARGET_WINS)
df<-df%>%select(-TEAM_BASERUN_CS,-TEAM_BASERUN_SB)

df<-df%>%mutate(TEAM_PITCHING_AB=162*27+TEAM_PITCHING_H+TEAM_PITCHING_BB,.before=TEAM_BATTING_H)
df<-df%>%mutate(TEAM_PITCHING_NO_OUT=TEAM_PITCHING_H+TEAM_PITCHING_BB,.before=TEAM_BATTING_H)
df<-df%>%mutate(TEAM_PITCHING_OBP=TEAM_PITCHING_NO_OUT/TEAM_PITCHING_AB, .before=TEAM_PITCHING_AB)
#df<-df%>%mutate(TEAM_PITCHING_1B=TEAM_PITCHING_H-TEAM_PITCHING_2B-TEAM_PITCHING_3B-TEAM_PITCHING_HR,.after=TEAM_PITCHING_H)
df<-df%>%mutate(TEAM_PITCHING_TOTAL_BASES=(TEAM_PITCHING_H-TEAM_PITCHING_HR)+TEAM_PITCHING_BB+TEAM_FIELDING_E+4*TEAM_PITCHING_HR,.before=TEAM_PITCHING_H)
df<-df%>%mutate(TEAM_PITCHING_SLG=TEAM_PITCHING_TOTAL_BASES/TEAM_PITCHING_AB,.after=TEAM_PITCHING_OBP)
df<-df%>%mutate(TEAM_PITCHING_OPS = TEAM_PITCHING_OBP+TEAM_PITCHING_SLG,.after=TEAM_BATTING_OPS)

#overall stats
#df<-df%>%mutate(OPS_DIF=TEAM_BATTING_OPS-TEAM_PITCHING_OPS,.after=TARGET_WINS)
#df<-df%>%mutate(NET_HR=TEAM_BATTING_HR-TEAM_PITCHING_HR)

df<-df%>%
  mutate(SO_factor= case_when(
    TEAM_BATTING_SO<800 ~'low',
    TEAM_BATTING_SO>800 ~'high'
  ))

#ggplot(df,aes(x=NET_HR,y=TARGET_WINS))+geom_point()+geom_smooth(method='lm')

#ggplot(df, aes(x=TEAM_BATTING_OPS,y=TARGET_WINS))+geom_point()+geom_smooth(method='lm')


#ggplot(df, aes(x=x,y=TARGET_WINS))+geom_point()+geom_smooth(method='lm')

df<-df%>%na.omit()
lm1<-lm(TARGET_WINS~TEAM_PITCHING_OPS+TEAM_PITCHING_SO+TEAM_FIELDING_E, data=df)
summary(lm1)

plot(lm1$residuals)
temp<-df%>%select(TARGET_WINS,TEAM_BATTING_OBP,TEAM_PITCHING_OBP,TEAM_PITCHING_SO,TEAM_BATTING_HR,TEAM_BATTING_3B,SO_factor)
temp<-temp%>%mutate(TEAM_PITCHING_SO=log(TEAM_PITCHING_SO))
library(GGally)
ggpairs(temp,aes(color=SO_factor, alpha=.4))

### EXPLORATORY ANALYSIS USING RANDOM FOREST AND BORUTA METHOD FOR SELECTING VARIABLES
library(psycho)
z_df<-as.data.frame(scale(df%>%select(-TARGET_WINS)))
z_df<-cbind(TARGET_WINS=df$TARGET_WINS,z_df)
lm2<-lm(TARGET_WINS~., data=z_df)
summary(lm2)

plot(y=df$TARGET_WINS,x=df$TEAM_BATTING_TOTAL_BASES)
library(ggcorrplot)
ggcorrplot(corr)

library(Boruta)
boruta.train<-Boruta(TARGET_WINS~.,data=df)
plot(boruta.train)
print(boruta.train)

#using random forest feature selection
library(caret)
library(randomForest)

control<-rfeControl(functions = rfFuncs,method = 'cv', number=10)

rfe.train<-rfe(df[,-1],df[,1],sizes = 1:12,rfeControl = control)

rfe.train
