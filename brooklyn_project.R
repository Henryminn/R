install.packages('ggpmisc')
library(tidyverse)
library(dplyr)
library(modeest)
library(xlsx)
library(ggplot2)
library(corrplot)
library(scales)
library(randomForest)
library(modelr)
library(MASS)
library(leaps)
library(ggpmisc)

data<- read.csv('brooklyn.csv')

names(data)
data<-data[,-c(1,7)]
sapply(data,class)
data<-filter(data,data$SALE.PRICE>0)
data<-filter(data,data$YEAR.BUILT!=0 )
summary(data$SALE.PRICE)

fctrs <- which(sapply(data, is.factor)) 
nums <- which(sapply(data, is.numeric)) 

data$ZIP.CODE<-as.factor(data$ZIP.CODE)
data$TAX.CLASS.AT.TIME.OF.SALE<-as.factor(data$TAX.CLASS.AT.TIME.OF.SALE)
data$YEAR.BUILT<-as.factor(data$YEAR.BUILT)
unique(data$YEAR.BUILT)
sum(is.na(data))

unique(data$NEIGHBORHOOD)

numerics <- which(sapply(data, is.numeric)) 
numericsNames <- names(numerics) #saving names vector for use later on
sales2_num <- data[, numericsNames]
cor_num <- cor(sales2_num, use="pairwise.complete.obs") #correlations of all numeric variables
corr_sorted <- as.matrix(sort(cor_num[,'SALE.PRICE'], decreasing = TRUE))
corr_sorted
cor_pos <- names(which(apply(corr_sorted, 1, function(x) abs(x)>0.1)))
cor_num <- cor_num[cor_pos, cor_pos]
corrplot.mixed(cor_num, tl.col="black", tl.pos = "lt")


ggplot(data=data,aes(x=TOTAL.UNITS,y=SALE.PRICE))+
  geom_point(col='blue')+
  geom_smooth(method='lm',se=FALSE,color='black')+
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels = comma)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,250000000))

ggplot(data=data,aes(x=GROSS.SQUARE.FEET,y=SALE.PRICE))+
  geom_point(col='blue')+
  geom_smooth(method='lm',se=FALSE,color='black')+
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels = comma)+
  coord_cartesian(xlim=c(0,350000),ylim=c(0,250000000))

ggplot(data=data,aes(x=RESIDENTIAL.UNITS,y=SALE.PRICE))+
  geom_point(col='blue')+
  geom_smooth(method='lm',se=FALSE,color='black')+
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels = comma)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,100000000))


avg_sales<-data%>%
  group_by(YEAR.BUILT)%>%
  summarize(avg=mean(SALE.PRICE))
ggplot(data = avg_sales, mapping=aes(x = YEAR.BUILT, y = avg,group=1))+
  geom_point()+
  geom_line()+
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  coord_cartesian(ylim=c(0,10000000))+
  scale_y_continuous(breaks = c(0,500000,1000000,2000000,3000000,4000000,10000000),labels = comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=data,aes(x=BUILDING.CLASS.CATEGORY,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,10000000))+
  scale_y_continuous(breaks = c(0,500000,1000000,10000000),labels = comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=data,aes(x=TAX.CLASS.AT.PRESENT,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(breaks = c(0,500000,1000000,5000000),labels = comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data=data,aes(x=NEIGHBORHOOD,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(breaks = c(0,500000,1000000,2000000,3000000,4000000,5000000),labels = comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=data,aes(x=ZIP.CODE,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(breaks = c(0,500000,1000000,2000000,3000000,4000000,5000000),labels = comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


summary(data$YEAR.BUILT)


unique(data$YEAR.BUILT)
set.seed(1)
quick_RF <- randomForest(x=data[1:13652,-c(1,6,7,8,15,17,19,18)], y=data$SALE.PRICE[1:13652], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:10,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


#Linear models
attach(data)
mod<-lm(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+BLOCK+LOT+LAND.SQUARE.FEET+ZIP.CODE,data=data)
summary(mod)

leaps<-regsubsets(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+BLOCK+LOT+LAND.SQUARE.FEET,data=data,nvmax = 10,method = "forward")
summary(leaps)



