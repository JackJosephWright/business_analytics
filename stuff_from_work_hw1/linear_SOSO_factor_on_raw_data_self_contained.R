#linearSOSO on raw data

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ruler)
library(GGally)
library(visdat)


raw <- read.csv("./moneyball-training-data.csv")
raw<-raw%>%select(-INDEX)
raw<-raw%>%select(-TEAM_BATTING_HBP)
df_new<-raw%>%na.omit()
#ggplot(df_new,aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(slope=.96,intercept = -120, color='red')

#adding in my stats BATTING

df_new<-df_new%>%mutate(AB=162*27+TEAM_BATTING_H+TEAM_BATTING_BB)
df_new<-df_new%>%mutate(B_NO_OUT=TEAM_BATTING_H+TEAM_BATTING_BB)
df_new<-df_new%>%mutate(B_OBP=B_NO_OUT/AB)
df_new<-df_new%>%mutate(B_1B=TEAM_BATTING_H-TEAM_BATTING_2B-TEAM_BATTING_3B-TEAM_BATTING_HR)
df_new<-df_new%>%mutate(B_TB=B_1B+2*TEAM_BATTING_2B+3*TEAM_BATTING_3B+4*TEAM_BATTING_HR)
df_new<-df_new%>%mutate(B_SLG=B_TB/AB)
df_new<-df_new%>%mutate(B_OPS=B_OBP+B_SLG)


#adding in my stats PITCHING

df_new<-df_new%>%mutate(P_AB=162*27+TEAM_PITCHING_H+TEAM_PITCHING_BB)
df_new<-df_new%>%mutate(P_NO_OUT=TEAM_PITCHING_H+TEAM_PITCHING_BB)
df_new<-df_new%>%mutate(P_OBP=P_NO_OUT/P_AB)

#MIGHT NEED TO CHANGE AFTER NORMALISING DATA, BUT I THINK THIS WILL BE OK
df_new <- df_new %>%
  mutate(SO_factor = case_when(TEAM_BATTING_SO >= TEAM_PITCHING_SO*.96+10~ 'high',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*.96+10 & TEAM_BATTING_SO>TEAM_PITCHING_SO*.96-50) ~'med_high',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*.96-50 & TEAM_BATTING_SO>TEAM_PITCHING_SO*.96-120) ~'med_low',
                               TEAM_BATTING_SO<TEAM_PITCHING_SO*.96-120 ~'low'))


#visdat::vis_miss(df_new, sort_miss = TRUE)
#ggpairs(data=df_new,mapping=ggplot2::aes(colour = SO_factor, alpha=.2))

c<-cor(df_new[-26])

df_high<-df_new%>%filter(SO_factor=='high')
df_med_high<-df_new%>%filter(SO_factor=='med_high')
df_med_low<-df_new%>%filter(SO_factor=='med_low')
df_med<-df_new%>%filter(SO_factor=='med_high'| SO_factor=='med_low')
df_low<-df_new%>%filter(SO_factor=='low')



lm_high<-lm(TARGET_WINS~.,data=df_high[-26])
summary(lm_high)
lm_med<-lm(TARGET_WINS~.,data=df_med[-26])
summary(lm_med)


#DF_HIGH

#eliminating unecessary variables

df_high_prune<-df_high%>%select(-TEAM_BATTING_BB,-TEAM_BATTING_SO,-TEAM_PITCHING_H,-TEAM_PITCHING_BB,-B_1B,-B_TB,-B_OPS,-P_NO_OUT,-B_NO_OUT,-TEAM_BATTING_H,-B_SLG,-TEAM_BATTING_HR,-AB,-B_OBP,-P_AB,-B_OBP,-TEAM_BASERUN_CS)

#ggpairs(data=df_high_prune%>%filter(SO_factor=='high'),mapping=ggplot2::aes(colour = SO_factor, alpha=.2))
lm_high_prune<-lm(TARGET_WINS~.,data=df_high_prune[-10])

summary(lm_high_prune)


#DF_MED

#eliminating unecessary variables

df_med_prune<-df_med%>%select(-TEAM_BATTING_BB,-TEAM_BATTING_SO,-TEAM_PITCHING_H,-TEAM_PITCHING_BB,-B_1B,-B_TB,-B_OPS,-P_NO_OUT,-B_NO_OUT,-TEAM_BATTING_H,-TEAM_BATTING_HR,-AB,-B_OBP,-P_AB,-TEAM_BASERUN_CS,-B_SLG)

lm_med_prune<-lm(TARGET_WINS~.,data=df_med_prune[-10])
summary(lm_med_prune)



#ggpairs(data=df_med_prune,mapping=ggplot2::aes(colour = SO_factor, alpha=.2))



df_med_and_high<-df_new%>%filter(SO_factor!='low')


df_not_low_prune<-df_med_and_high%>%select(-TEAM_BATTING_BB,-TEAM_BATTING_SO,-TEAM_PITCHING_H,-TEAM_PITCHING_BB,-B_1B,-B_TB,-B_OPS,-P_NO_OUT,-B_NO_OUT,-TEAM_BATTING_H,-TEAM_BATTING_HR,-AB,-B_OBP,-P_AB,-TEAM_BASERUN_CS,-B_SLG)


#ONLY LOSE .3 R^2 on the high group, gain .1 R2 on the med group
lm_not_low_prune<-lm(TARGET_WINS~.,data=df_not_low_prune[-10])
summary(lm_not_low_prune)


#DF_LOW

lm_low<-lm(TARGET_WINS~.,data=df_low[-26])
summary(lm_low)
#ggpairs(data=df_low,mapping=ggplot2::aes(colour = SO_factor, alpha=.2))


#pruning_df_low
df_low_prune<-df_low%>%select(-TEAM_BATTING_H,-TEAM_BATTING_2B,-TEAM_BATTING_3B,-TEAM_PITCHING_H,-AB,-B_NO_OUT,-B_TB,-B_OPS,-TEAM_BATTING_HR,-TEAM_BASERUN_CS,-TEAM_BASERUN_SB,-TEAM_PITCHING_BB,-TEAM_BATTING_BB,-TEAM_PITCHING_HR,-B_1B,-P_NO_OUT,-P_AB,-B_SLG,-P_OBP,-TEAM_PITCHING_SO,-TEAM_BATTING_SO,-TEAM_FIELDING_DP)

lm_low_prune<-lm(TARGET_WINS~.,data=df_low_prune[-4])
summary(lm_low_prune)

#ggpairs(data=df_low_prune)


eval_raw<-read.csv('moneyball-evaluation-data.csv')

df_eval<-eval_raw%>%select(-TEAM_BATTING_HBP,-INDEX)

df_eval<-df_eval%>%na.omit()
#filtering out 10,000 SO season from df_eval
df_eval<-df_eval%>%filter(TEAM_PITCHING_SO<7500)
ggplot(df_eval,aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(slope=.96,intercept = 10, color='blue')+geom_abline(slope=.96,intercept = -50, color='red')+ggtitle('model splitting on evaluation data')


df_all_prune<-df_new%>%select(-TEAM_BATTING_BB,-TEAM_BATTING_SO,-TEAM_PITCHING_H,-TEAM_PITCHING_BB,-B_1B,-B_TB,-B_OPS,-P_NO_OUT,-B_NO_OUT,-TEAM_BATTING_H,-TEAM_BATTING_HR,-AB,-B_OBP,-P_AB,-TEAM_BASERUN_CS,-B_SLG)

lm_all<-lm(TARGET_WINS~., data=df_all_prune[-10])
summary(lm_all)

#ggpairs(df_all_prune)


#shapiro test on df_high_prune
lshap<-lapply(df_high_prune[-10],shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
as.data.frame(lres)


#NORMALISING DATA (df_new)
library(normalr)
lambdas<-getLambda(df_new)
dat<-normaliseData(df_new,lambda_high)


dat%>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

lshap<-lapply(dat,shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
as.data.frame(lres)
