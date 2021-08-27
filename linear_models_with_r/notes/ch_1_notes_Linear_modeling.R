library(faraway)
library(tidyverse)
data(pima)
view(pima)

#use sort() to dig a little deeper

sort(pima$diastolic)

#note 35 values of zero (cant have 0 bloodpressure)

#probably a missing value code

#set zero values for specific columns to NA

factors<-names(pima)
for_na<-factors[2:6]

#sets zeros on list to NA 
df<-pima
for (i in 2:length(for_na)){
  df[i]<-na_if(df[i],0)
}

#simpler way to set NA using mutate

df<-pima%>% mutate(across(for_na, ~na_if(.,0)))

summary(df)

pima<-df

pima$test<-as_factor(pima$test)

is.factor(pima$test)

summary(pima$test)

#now that we have tidyed the data look at some summary statistics

p1<-ggplot(data=pima, aes(x=na.rm(diastolic))+geom_histogram(bins=20)
p2<-ggplot(data=pima, aes(x=diastolic))+geom_density()

library(gridExtra)
grid.arrange(p1,p2, ncol=1)


#midterm to final regression
data("stat500")

stat500<-data.frame(scale(stat500))
view(stat500)

ggplot(data=stat500, aes(midterm,final))+geom_point()+geom_abline(slope = 1,intercept=0, color='red')+geom_smooth(method='lm',linetype='dashed')

#compute least squares regression
g<-lm(final~midterm,stat500)
#get correlation matrix
cor(stat500)
