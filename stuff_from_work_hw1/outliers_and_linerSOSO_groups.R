#trying to identify outliers for each column
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ruler)


raw <- read.csv("./moneyball-training-data.csv")
raw<-raw%>%select(-INDEX)




mu_t=mean(raw$TARGET_WINS)
sd_t=sd(raw$TARGET_WINS)
temp<-as.data.frame(scale(raw))
temp<-temp%>%select(-TEAM_BATTING_HBP, -TEAM_BASERUN_CS,-TEAM_BASERUN_SB)



temp<-temp%>%filter_all(.,all_vars(.<3))
temp<-temp%>%filter_all(.,all_vars(.>-3))
temp<-temp%>%mutate(TARGET_WINS=TARGET_WINS*sd_t+mu_t)
#ggpairs(temp)

ggplot(temp,aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-.6,slope=2.12, color='red')
df_new <- temp %>%
  mutate(SO_factor = case_when(TEAM_BATTING_SO >= TEAM_PITCHING_SO*2.15+.25~ '6',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*2.15+.25 & TEAM_BATTING_SO>TEAM_PITCHING_SO*2.1+.06) ~'5',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*2.1+.06 & TEAM_BATTING_SO>TEAM_PITCHING_SO*2.12-.6) ~'4',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*2.12-.06 & TEAM_BATTING_SO>TEAM_PITCHING_SO*1.5-1) ~'3',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*1.5-1 & TEAM_BATTING_SO>TEAM_PITCHING_SO*1-1.5) ~'2',
                               (TEAM_BATTING_SO<TEAM_PITCHING_SO*1-1.5 & TEAM_BATTING_SO>TEAM_PITCHING_SO*1.5-1) ~'3',
                               TEAM_BATTING_SO<TEAM_PITCHING_SO*1.5-1 ~'1'))
#ggplot(df_top,aes(x=TEAM_PITCHING_SO,y=TEAM_BATTING_SO))+geom_point()+geom_abline(intercept=-.5,slope=2, color='red')
lm1<-lm(TARGET_WINS~.,data=df_top)

summary(lm1)

ggpairs(data=df_new,
        mapping=ggplot2::aes(colour = SO_factor, alpha=.2))


ggplot(df_new, aes(x=TEAM_BATTING_SO,y=TEAM_PITCHING_SO, color=SO_factor))+geom_point()+ggtitle('multiple trends In B_SO/P_SO')

ggplot(df_new, aes(x=TEAM_PITCHING_SO ,color=SO_factor, alpha=.3))+geom_density()

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


#hitting only stats
df_final_hit<-df_new%>%select(TARGET_WINS,AB,B_NO_OUT,B_OBP,B_SLG,B_OPS,B_1B,TEAM_BATTING_2B,TEAM_BATTING_3B,TEAM_BATTING_HR,TEAM_BATTING_BB,SO_factor)
df_final_pitch<-df_new%>%select(TARGET_WINS,P_AB,P_NO_OUT,P_OBP,TEAM_PITCHING_HR,TEAM_PITCHING_BB,SO_factor)
ggpairs(data=df_final_hit,
        mapping=ggplot2::aes(colour = SO_factor, alpha=.2))
ggpairs(data=df_final_pitch,
        mapping=ggplot2::aes(colour = SO_factor, alpha=.2))
df_4<-df_new%>%filter(SO_factor=='4')


c_4<-cor(df_4%>%select(-SO_factor))


lm_4<-lm(TARGET_WINS~.,data=df_final_pitch[-7])
summary(lm_4)

lm_total<-lm(TARGET_WINS~.,data=df_new[-21])
summary(lm_total)


