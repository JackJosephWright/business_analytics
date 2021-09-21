#disentangling SO continued


library(tidyverse)
library(ggplot)
library(visdat)
library(corrplot)
library(ggpub)
setwd('C:/Program Files/GitHub/business_analytics')
dat<-read.csv('.\\data\\moneyball-training-data.csv',header = TRUE)%>%select(-INDEX)

dat<-dat%>%select(-TEAM_BATTING_HBP)

df<-dat%>%filter(TEAM_BATTING_SO>308)


#getting new distribution

mD<-normalmixEM(df$TEAM_BATTING_SO)
summary(mD)
plot(mD,which=2)
lines(density(dat$TEAM_BATTING_SO,"SJ"), lwd=2)

temp<-df%>%filter(TEAM_BATTING_SO>mD$mu[1] & TEAM_BATTING_SO<mD$mu[1]+1*mD$sigma[1])

ggplot(temp, aes(TEAM_BATTING_SO))+geom_histogram()


#hypothesis test if the distributions are different

low<-rnorm(1000,mean=mD$mu[1],sd=mD$sigma[1])
high<-rnorm(1000,mean=mD$mu[2],sd=mD$sigma[2])
t.test(low,high)

#confirmed


#crude segmentation of data using pickPeak from mixtools

df_factor<-df%>%
  mutate(SO_factor= case_when(
    TEAM_BATTING_SO<726 ~'low',
    TEAM_BATTING_SO>726 ~'high'
  ))

df_low<-df_factor%>%filter(SO_factor=='low')
df_high<-df_factor%>%filter(SO_factor=='high')

#getting boundaries of IQRS for HR given SO mode
low_HR_IQR<-c(boxplot(df_low$TEAM_BATTING_HR)$stats[2],boxplot(df_low$TEAM_BATTING_HR)$stats[4])
high_HR_IQR<-c(boxplot(df_high$TEAM_BATTING_HR)$stats[2],boxplot(df_high$TEAM_BATTING_HR)$stats[4])


#getting boundaries of IQRS for 3B given SO mode
low_3B_IQR<-c(boxplot(df_low$TEAM_BATTING_3B)$stats[2],boxplot(df_low$TEAM_BATTING_3B)$stats[4])
high_3B_IQR<-c(boxplot(df_high$TEAM_BATTING_3B)$stats[2],boxplot(df_high$TEAM_BATTING_3B)$stats[4])

low_3B_IQR
high_3B_IQR

#testing probabilities

#probability that df_low is above low_HR_IQR

p_low_SO_high_HR<-nrow(df_low%>%filter(TEAM_BATTING_HR>low_HR_IQR[2]))/nrow(df_low)
df_lSOhHR<-df_low%>%filter(TEAM_BATTING_HR>low_HR_IQR[2])

#df_low P(low_3B|high_HR)
df_lSOhHRl3B<-df_lSOhHR%>%filter(TEAM_BATTING_HR<low_3B_IQR[1])
(p_low_SO_high_HR_low_3b<-nrow(df_lSOhHRl3B%>%filter(TEAM_BATTING_3B<low_3B_IQR[1]))/nrow(df_low))                     


#probability that df_high is BELOW high_HR_IQR


p_high_SO_low_HR<-nrow(df_high%>%filter(TEAM_BATTING_HR<high_HR_IQR[1]))/nrow(df_low)
df_hSOlHR<-df_high%>%filter(TEAM_BATTING_HR<high_HR_IQR[1])


#df_high P(high_3B|low_HR)
df_hSOlHRh3B<-df_hSOlHR%>%filter(TEAM_BATTING_HR>high_3B_IQR[2])
(p_high_SO_low_HR_high_3b<-nrow(df_hSOlHRh3B%>%filter(TEAM_BATTING_3B>high_3B_IQR[1]))/nrow(df_high))                 
