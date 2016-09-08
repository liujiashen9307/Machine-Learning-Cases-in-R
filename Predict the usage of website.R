data<- read.csv('top_1000_sites.tsv',sep='\t', header=TRUE,stringsAsFactors = FALSE)
summary(data)
######Load Package####
library(ggplot2)
#####Data Cleansing and transforming dummy variables####
#####For Variables that are not completed, we choose to abandon them##
df<-data[,c(4:7)]
df$HasAdvertising<-ifelse(df$HasAdvertising=="Yes",1,0)
#####Explore Dependent/Independent Variables####
p1<-ggplot(df,aes(x=Reach,y=PageViews))+geom_point()
p2<-ggplot(df,aes(x=UniqueVisitors,y=PageViews))+geom_point()
p1
p2

#####Explore data transformation##
p3<-ggplot(df,aes(x=log(Reach),y=log(PageViews)))+geom_point()
p3
p4<-ggplot(df,aes(x=log(UniqueVisitors),y=log(PageViews)))+geom_point()
p4


#####Kappa Test####
Variable<-df[,c(1,2)]
Cor<-cor(Variable)
kappa(Cor,exact = TRUE)
###Very Huge! Way bigger than 1000####

###Method: Only Choose one of two variables##


#####Regression with each variable#####
PV_Reach<-lm(log(df$PageViews)~log(df$UniqueVisitors))
summary(PV_Reach)
PV_UniqueVisitor<-lm(log(df$PageViews)~log(df$UniqueVisitors))
summary(PV_UniqueVisitor)

#####Diagnose Regression and Transform Data####

plot(PV_Reach)

plot(PV_UniqueVisitor)

#####Multiple-Variable Regression#####

Reg<-lm(log(df$PageViews)~log(df$UniqueVisitors)+df$HasAdvertising)

summary(Reg)

#####Regression Diagnosis######

plot(Reg)
