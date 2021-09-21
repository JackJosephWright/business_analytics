#TEAM_BATTING_SO exploratory analysis
library(tidyverse)
library(ggplot)
library(visdat)
library(corrplot)
dat<-read.csv('.\\data\\moneyball-training-data.csv',header = TRUE)%>%select(-INDEX)
dat<-dat%>%select(-TEAM_BATTING_HBP)
visdat::vis_miss(dat,sort_miss = TRUE)


#deselect data missing TEAM_PITCHING_SO

dat<-dat%>%filter(!is.na(TEAM_BATTING_SO))

sum(is.na(dat$TEAM_BATTING_SO))


#check correlation
correlation<-cor(dat)
corrplot.mixed(correlation,tl.col = 'black',tl.pos = 'lt')

#note zero correlation between wins


#note bimodal structure of team strikeouts

ggplot(dat, aes(TEAM_BATTING_SO))+geom_density()

#finding local maxima
dat<-dat%>%filter(TEAM_BATTING_SO>308)
library(mixtools)
mD<-normalmixEM(dat$TEAM_BATTING_SO)
summary(mD)
plot(mD,which=2)
lines(density(dat$TEAM_BATTING_SO,"SJ"), lwd=2)


lower_dist<-dat%>%filter(TEAM_BATTING_SO<(mD$mu[1]+mD$sigma[1]) &TEAM_BATTING_SO>(mD$mu[1]-mD$sigma[1]) )
upper_dist<-dat%>%filter(TEAM_BATTING_SO<(mD$mu[2]+mD$sigma[2]) &TEAM_BATTING_SO>(mD$mu[2]-mD$sigma[2]) )

#it seems like the distributions split at about 845


#factorizing with BATTING_SO split at 845

dat_SO<-dat%>%
  mutate(SO_factor= case_when(
    TEAM_BATTING_SO<845 ~'low',
    TEAM_BATTING_SO>845 ~'high'
  ))

dat_SO<-dat_SO%>%filter(!is.na(SO_factor))
dat_SO_hit<-dat_SO%>%select(TARGET_WINS, TEAM_BATTING_H,TEAM_BATTING_2B,TEAM_BATTING_3B,TEAM_BATTING_HR,SO_factor)

#pivot table for plotting

piv_hit<-dat_SO_hit%>% pivot_longer(cols =TARGET_WINS:TEAM_BATTING_HR, names_to = 'source',values_to = 'value')

ggplot(pivot_longer(dat_SO_hit, -SO_factor, names_to = 'var', values_to = 'val'), aes(SO_factor, val, color=SO_factor))+
  geom_boxplot(outlier.color = 'black')+xlab('mode of BATTING_SO')+facet_wrap(~var, scales='free')+scale_x_discrete(guide=guide_axis(n.dodge=2))



#redoing this but removing values less than all time for SO

dat_SO_clipped<-dat%>%filter(TEAM_BATTING_SO>308)
min(dat_SO_clipped$TEAM_BATTING_SO)

#getting new distribution

mD<-normalmixEM(dat_SO_clipped$TEAM_BATTING_SO)
summary(mD)
plot(mD,which=2, xlab='Strikeouts per season')
lines(density(dat$TEAM_BATTING_SO,"SJ"), lwd=2)


#split at 700

dat_SO_2<-dat%>%
  mutate(SO_factor= case_when(
    TEAM_BATTING_SO<700 ~'low',
    TEAM_BATTING_SO>700 ~'high'
  ))

#plotting


dat_SO_2<-dat_SO_2%>%filter(!is.na(SO_factor))
dat_SO_2_hit<-dat_SO_2%>%select(TARGET_WINS, TEAM_BATTING_H,TEAM_BATTING_2B,TEAM_BATTING_3B,TEAM_BATTING_HR,SO_factor)

#pivot table for plotting

piv_hit_2<-dat_SO_2_hit%>% pivot_longer(cols =TARGET_WINS:TEAM_BATTING_HR, names_to = 'source',values_to = 'value')

ggplot(pivot_longer(dat_SO_2_hit, -SO_factor, names_to = 'var', values_to = 'val'), aes(SO_factor, val, color=SO_factor))+
  geom_boxplot(outlier.color = 'black')+xlab('mode of BATTING_SO')+facet_wrap(~var, scales='free')+scale_x_discrete(guide=guide_axis(n.dodge=2))



