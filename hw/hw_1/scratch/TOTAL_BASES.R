#total bases


library(tidyverse)
library(ggplot)
library(visdat)
library(corrplot)
library(ggpub)
dat<-read.csv('.\\data\\moneyball-training-data.csv',header = TRUE)%>%select(-INDEX)
dat<-dat%>%select(-TEAM_BATTING_HBP)
visdat::vis_miss(dat,sort_miss = TRUE)


dat_tb<-dat%>%mutate(TEAM_BATTING_BASES=.7*(TEAM_BATTING_H-TEAM_BATTING_2B-TEAM_BATTING_3B-TEAM_BATTING_HR)+TEAM_BATTING_2B+1.27*TEAM_BATTING_3B+1.65*TEAM_BATTING_HR+.66*TEAM_BATTING_BB)


#check correlation
correlation<-cor(dat_tb)
corrplot.mixed(correlation,tl.col = 'black',tl.pos = 'lt')
cor(dat_tb$TARGET_WINS,dat_tb$TEAM_BATTING_BASES)


p1<-ggplot(dat_tb, aes(x=TEAM_BATTING_BASES,y=TARGET_WINS))+geom_point()

p2<-ggplot(dat_tb, aes(x=TEAM_BATTING_H,y=TARGET_WINS))+geom_point()

b1<-boxplot(dat_tb$TEAM_BATTING_BASES)$out
b2<-boxplot(dat_tb$TEAM_BATTING_H)$out
b3<-boxplot(dat_tb$TEAM_BATTING_2B)$out
b4<-boxplot(dat_tb$TEAM_BATTING_3B)$out
b5<-boxplot(dat_tb$TEAM_BATTING_HR)$out

tb_box<-dat_tb%>%select(TEAM_BATTING_BASES,TEAM_BATTING_H,TEAM_BATTING_2B,TEAM_BATTING_3B,TEAM_BATTING_HR)

ggplot(stack(tb_box),aes(x=ind,y=values))+
  geom_boxplot()

tb_BASES<-lm(formula = TARGET_WINS~TEAM_BATTING_BASES,data=dat_tb)
summary(tb_BASES)

tb_lm<-lm(formula = TARGET_WINS~.,data=dat_tb)
summary(tb_lm)


