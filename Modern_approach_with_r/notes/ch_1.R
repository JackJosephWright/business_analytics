
library(tidyverse)
library(ggplot2)
dat<-read.csv('./data/circulation.txt')
#tidying data
colnames(dat)<-'col_1'
df <- separate(data = dat, col = 'col_1', into = c('paper', 'sunday','weekday','competitor'), sep = '\\t')
df$weekday<-as.numeric(df$weekday)
df$sunday<-as.numeric(df$sunday)
df$competitor<-as.numeric(df$competitor)
library(GGally)
ggpairs(df%>%select(-paper))

#log plot of sunday vs weekday with log transform
temp<-ggplot(df, aes(x = log(weekday), y=log(sunday), color=competitor))+
  geom_point()

temp


#menu pricing in new italian restaurant in NYC

df<-read.csv('./data/nyc.csv')
library(GGally)

ggpairs(df[,names(df)!='Restaurant'], title='correlogram with ggpairs()')

df$East<-as.factor(df$East)

ggplot(df, aes(x=East,y=Price))+
  geom_boxplot()


#bordeaux wine

df<-read.csv('./data/bordeaux.csv')

df<-df%>%drop_na()

ggpairs(df[,names(df)!='Wine'], title='correlogram with ggpairs()')
attach(df)
model<-lm(Price~ParkerPoints+CoatesPoints)
summary(model)

model<-lm(Price~ParkerPoints+CoatesPoints+P95andAbove+FirstGrowth+CultWine+Pomerol+VintageSuperstar)
summary(model)

#using Boruta to find features

library(caret)
library(Boruta)

boruta_output<-Boruta(Price ~.,data=na.omit(df),doTrace=2)
names(boruta_output)        

boruta_signif<- getSelectedAttributes(boruta_output,withTentative = TRUE)
print(boruta_signif)

#do a tentative rough fix
rough_fix_mod<- TentativeRoughFix(boruta_output)

#get importance scores of the variables

imps <-attStats(rough_fix_mod)
imps2<- imps[imps$decision !='Rejected', c('meanImp','decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

#model with selected features
model<-lm(Price~ParkerPoints+CoatesPoints+P95andAbove+FirstGrowth+CultWine)
summary(model)

model<-lm(Price~ParkerPoints+FirstGrowth+CultWine)
summary(model)
