#Suggested Exercises Linear Modeling

#1.


# The dataset teengamb concerns a study of teenage gambling in Britain. Make a
# numerical and graphical summary of the data, commenting on any features that you
# find interesting. Limit the output you present to a quantity that a busy reader would
# find sufficient to get a basic understanding of the data.
# 

library(tidyverse)
library(GGally)
df<-teengamb

#check for missing data 
summary(df)

#look at pairs

ggpairs(df)

#strong correlation with income and sex (negative correlation because men are coded 0)

ggplot(df,aes(y=gamble,x=as.factor(sex)))+geom_boxplot()


#check assumptions with boruta method

library(caret)
library(Boruta)

boruta_output<-Boruta(gamble ~.,data=na.omit(df),doTrace=0)

boruta_signif<- getSelectedAttributes(boruta_output,withTentative = TRUE)
print(boruta_signif)

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


p1<-ggplot(data=df,aes(x=income,y=gamble))+geom_point()+geom_smooth(method='lm')

model<-lm(data=df,gamble~income+sex)

summary(model)


#income and sex are clearly the most important predictors when it comes to youth gambling. an R^2 of .5 when accounting for human action is high enough to be considered 
#a good description of the situation.



# The dataset uswages is drawn as a sample from the Current Population Survey in
# 1988. Make a numerical and graphical summary of the data as in the previous
# question.

df<-uswages
summary(uswages)

#noting there is a negative experience value, want to look into that more

sort(uswages$exper) 

#it doesnt seem to be an outlier, maybe its years of primary school not finished? ill leave it for now

#examining data for continuous variables

ggpairs(df[1:3])
#checking for normality

p1<-ggplot(df,aes(wage))+geom_density()+ggtitle('density of wages')+theme(plot.title = element_text(hjust = 0.5))
p1qq<-ggplot(df,aes(sample=wage))+stat_qq()+stat_qq_line()+ggtitle('qq of wages')+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p1qq,nrow=1)

#note that the distribution is not normal. I will try a squares adjustment

p1<-ggplot(df,aes(sqrt(wage)))+geom_density()+ggtitle('density of wages')+theme(plot.title = element_text(hjust = 0.5))
p1qq<-ggplot(df,aes(sample=sqrt(wage)))+stat_qq()+stat_qq_line()+ggtitle('qq of wages')+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p1qq,nrow=1)

#much more normal

#looking at experience

p1<-ggplot(df,aes(exper))+geom_density()+ggtitle('density of wages')+theme(plot.title = element_text(hjust = 0.5))
p1qq<-ggplot(df,aes(sample=exper))+stat_qq()+stat_qq_line()+ggtitle('qq of wages')+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p1qq,nrow=1)

#looking at education

p1<-ggplot(df,aes(educ))+geom_density()+ggtitle('density of wages')+theme(plot.title = element_text(hjust = 0.5))
p1qq<-ggplot(df,aes(sample=educ))+stat_qq()+stat_qq_line()+ggtitle('qq of wages')+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p1qq,nrow=1)
