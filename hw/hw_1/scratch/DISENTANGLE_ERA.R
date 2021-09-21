#disentangling eras with TEAM_BATTING_SO

library(tidyverse)
library(ggplot)
library(visdat)
library(corrplot)
library(ggpub)
setwd('C:/Program Files/GitHub/business_analytics')
dat<-read.csv('.\\data\\moneyball-training-data.csv',header = TRUE)%>%select(-INDEX)

dat<-dat%>%select(-TEAM_BATTING_HBP)
visdat::vis_miss(dat,sort_miss = TRUE)

#adding total bases column
dat_tb<-dat%>%mutate(TEAM_BATTING_BASES=1*(TEAM_BATTING_H-TEAM_BATTING_2B-TEAM_BATTING_3B-TEAM_BATTING_HR)+2*TEAM_BATTING_2B+3*TEAM_BATTING_3B+4*TEAM_BATTING_HR+1*TEAM_BATTING_BB)


#adding baserunning column

df<-dat_tb%>%filter(!is.na(TEAM_BASERUN_CS & TEAM_BASERUN_SB))
df<-df%>%mutate(TEAM_NET_SB=TEAM_BASERUN_SB-TEAM_BASERUN_CS)


#factoring for SO
df<-df%>%filter(TEAM_BATTING_SO>308)
df_factor<-df%>%
  mutate(SO_factor= case_when(
    TEAM_BATTING_SO<700 ~'low',
    TEAM_BATTING_SO>700 ~'high'
  ))


#plotting

df_factor<-df_factor%>%filter(!is.na(SO_factor))
df_factor<-df_factor%>%select(TARGET_WINS, TEAM_BATTING_BASES,TEAM_NET_SB,TEAM_BATTING_3B,TEAM_BATTING_HR,SO_factor)



ggplot(pivot_longer(df_factor, -SO_factor, names_to = 'var', values_to = 'val'), aes(SO_factor, val, color=SO_factor))+
  geom_boxplot(outlier.color = 'black')+xlab('mode of BATTING_SO')+facet_wrap(~var, scales='free')+scale_x_discrete(guide=guide_axis(n.dodge=2))


#seeing if correlation improves by filtering for era

ggpairs(df_factor%>%filter(SO_factor=='low'))
ggpairs(df_factor%>%filter(SO_factor=='high'))
## LM_LOW
df_lm_low<-df_factor%>%filter(SO_factor=='low')%>%select(-SO_factor)

lm_low<-lm(TARGET_WINS~., data=df_lm_low)
summary(lm_low)


##LM_HIGH

df_lm_high<-df_factor%>%filter(SO_factor=='high')%>%select(-SO_factor)

lm_high<-lm(TARGET_WINS~., data=df_lm_high)
summary(lm_high)
