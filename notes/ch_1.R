
library(tidyverse)
library(ggplot2)
dat<-read.csv('circulation.txt')
#tidying data
colnames(dat)<-'col_1'
df <- separate(data = dat, col = 'col_1', into = c('paper', 'sunday','weekday','competitor'), sep = '\\t')
df$weekday<-as.numeric(df$weekday)
df$sunday<-as.numeric(df$sunday)
df$competitor<-as.numeric(df$competitor)


#log plot of sunday vs weekday with log transform
temp<-ggplot(df, aes(x = log(weekday), y=log(sunday), color=competitor))+
  geom_point()

temp


#menu pricing in new italian restaurant in NYC

df<-read.csv('nyc.csv')
library(GGally)

ggpairs(df[,names(df)!='Restaurant'], title='correlogram with ggpairs()')

df$East<-as.factor(df$East)

ggplot(df, aes(x=East,y=Price))+
  geom_boxplot()
