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
library(forecast)
library(glmnet)
library(boot)

#Data cleaning
data<- read.csv('brooklyn.csv')
names(data)
data<-data[,-c(1,7)]
sapply(data,class)
write.csv(data,"cleaned_data.csv",row.names=F)
data<-read.csv("cleaned_data.csv")
data<-filter(data,data$SALE.PRICE>0)
data<-filter(data,data$YEAR.BUILT!=0 )
data$ZIP.CODE<-as.factor(data$ZIP.CODE)
data$TAX.CLASS.AT.TIME.OF.SALE<-as.factor(data$TAX.CLASS.AT.TIME.OF.SALE)
data$YEAR.BUILT<-as.factor(data$YEAR.BUILT)
sapply(data,class)

#Correlation matrix
numerics <- which(sapply(data, is.numeric)) 
numericsNames <- names(numerics) #saving names vector for use later on
sales2_num <- data[, numericsNames]
cor_num <- cor(sales2_num, use="pairwise.complete.obs") #correlations of all numeric variables
corr_sorted <- as.matrix(sort(cor_num[,'SALE.PRICE'], decreasing = TRUE))
corr_sorted
cor_pos <- names(which(apply(corr_sorted, 1, function(x) abs(x)>0.1)))
cor_num <- cor_num[cor_pos, cor_pos]
corrplot.mixed(cor_num, tl.col="black", tl.pos = "lt")

#Explanatory data analysis
#Total units vs Sale price
ggplot(data=data,aes(x=TOTAL.UNITS,y=SALE.PRICE))+
  geom_point(col='blue')+
  scale_x_continuous(breaks=c(1,2,5,10),labels=comma,name = "Total Units")+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(1,10))+
  ylim(100000,1000000)+
  ggtitle("Total units vs Sales Price")


#Gross square feet vs Sale price
ggplot(data=data,aes(x=GROSS.SQUARE.FEET,y=SALE.PRICE))+
  geom_point(col='blue')+
  geom_smooth(method='lm',se=FALSE,color='black')+
  scale_x_continuous(breaks=c(100,10000),labels=comma,name="Gross Square feet")+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(500,10000),ylim=c(1000,1000000))+
  ggtitle("Gross Square feet vs Sales Price")

#Residential units vs Sale price
ggplot(data=data,aes(x=RESIDENTIAL.UNITS,y=SALE.PRICE))+
  geom_point(col='blue')+
  scale_x_continuous(breaks=c(1,2,5,10,50,100),labels=comma,name="Residential units")+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(1,10))+
  ylim(100000,1000000)+
  ggtitle("Residential Units vs Sales Price")

#Impact of Year built on Sales
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
  scale_y_continuous(breaks = c(0,500000,1000000,2000000,3000000,4000000,10000000),labels = comma,name="Average Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Impact of Year Built on Sales")

#Building class category vs Sale price
ggplot(data=data,aes(x=BUILDING.CLASS.CATEGORY,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(100,1000000))+
  scale_y_continuous(breaks = c(100,500000,1000000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Tax Class vs Sale price
ggplot(data=data,aes(x=TAX.CLASS.AT.TIME.OF.SALE,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(100,1000000))+
  scale_y_continuous(breaks = c(100,500000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Neighbourhood vs Sale price
ggplot(data=data,aes(x=NEIGHBORHOOD,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(100,1000000))+
  scale_y_continuous(breaks = c(100,500000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Zip code vs Sale price
ggplot(data=data,aes(x=ZIP.CODE,y=SALE.PRICE))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,100000))+
  scale_y_continuous(breaks = c(0,500000,1000000),labels = comma,name="Sales Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Gross square feet vs Sale price based on Tax Clas
attach(data)
ggplot(data=data,aes(x=GROSS.SQUARE.FEET,y=SALE.PRICE))+
  geom_point()+
  facet_grid(.~TAX.CLASS.AT.TIME.OF.SALE)+
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels = comma,name="Sales Price")+
  coord_cartesian(xlim=c(1000,10000),ylim=c(100000,1000000))

#Histogram of Sale price
options(scipen=10000)
ggplot(data, aes(x = SALE.PRICE, fill = ..count..)) +
  geom_histogram(binwidth = 50000) +
  ggtitle("Histogram of SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  coord_cartesian(xlim=c(0,20000000),ylim=c(0,200))+
  scale_x_continuous(breaks=c(1000000,5000000,10000000,20000000),labels=comma)+
  theme(plot.title = element_text(hjust = 0.5))
attach(data)

#Histogram of Sales Price by Neighborhood 
ggplot(data, aes(x =NEIGHBORHOOD , fill = NEIGHBORHOOD )) + 
  geom_bar()+ 
  ggtitle("Histogram of Sales Price by Neighborhood") +
  ylab("Count of houses") +
  xlab("Neighbourhood")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right",axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

#Histogram of Sales Price by Tax Class at time of sale
ggplot(data, aes(x =TAX.CLASS.AT.TIME.OF.SALE , fill = TAX.CLASS.AT.TIME.OF.SALE )) + 
  geom_bar()+ 
  ggtitle("Histogram of Sales Price by Tax Class at time of sale") +
  ylab("Count of houses") +
  xlab("Tax Class at time of Sale")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

#Histogram of Sales Price by Building Class category
ggplot(data, aes(x =BUILDING.CLASS.CATEGORY , fill = BUILDING.CLASS.CATEGORY )) + 
  geom_bar()+ 
  ggtitle("Histogram of Sales Price by Building Class category") +
  ylab("Count of houses") +
  xlab("Building Class Category")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.background = element_rect(fill="grey90",size=0.5, linetype="solid", colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

#Histogram of SalePrice by Tax class at present
ggplot(data, aes(x = SALE.PRICE,fill = TAX.CLASS.AT.PRESENT)) +
  geom_histogram(position = "stack", binwidth = 50000) +
  ggtitle("Histogram of SalePrice by Tax class at present") +
  ylab("Count") +
  xlab("Tax Class at present") + 
  coord_cartesian(xlim=c(0,5000000))+
  scale_x_continuous(breaks = c(0,1000000,3000000,5000000),labels = comma)+
  theme(plot.title = element_text(hjust = 0.5),legend.background = element_rect(fill="grey90",size=0.5, linetype="solid", colour ="black"))

#Histogram of SalePrice by Tax class at time of Sale
ggplot(data, aes(x = SALE.PRICE,fill = TAX.CLASS.AT.TIME.OF.SALE)) +
  geom_histogram(position = "stack", binwidth = 50000) +
  ggtitle("Histogram of SalePrice by Tax class at time of Sale") +
  ylab("Count") +
  xlab("Tax class at time of Sale") + 
  coord_cartesian(xlim=c(0,5000000))+
  scale_x_continuous(breaks = c(0,1000000,3000000,5000000),labels = comma)+
  theme(plot.title = element_text(hjust = 0.5),legend.background = element_rect(fill="grey90",size=0.5, linetype="solid", colour ="black"))
summary(data$TAX.CLASS.AT.TIME.OF.SALE)

#Random forest for variable selection
unique(data$YEAR.BUILT)
set.seed(1)
quick_RF <- randomForest(x=data[1:13652,-c(1,6,7,8,15,17,19,18)], y=data$SALE.PRICE[1:13652], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:10,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase in MSE',title="Variable Importance") + 
  coord_flip() + theme(legend.position="none")

#Linear models
#Forward selection
#With interaction
leaps<-regsubsets(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data,nvmax = 10,method = "forward")
summary(leaps)
regsummary =summary(leaps)
regsummary$rsq
regsummary$adjr2
plot(regsummary$rsq,xlab="Number of variables", ylab="RSQ",type="l")
plot(regsummary$adjr2,xlab="Number of variables", ylab="Adjusted RSQ",type="l")
plot(regsummary$cp,xlab="Number of variables", ylab="Cp",type="l")
plot(regsummary$bic,xlab="Number of variables", ylab="bic",type="l")

plot(leaps,scale ="r2")
plot(leaps,scale ="adjr2")
plot(leaps,scale ="Cp")
plot(leaps,scale ="bic")
points(8, regsummary$adjr2[which.max(regsummary$adjr2)],col='red')

#Without interaction
leaps<-regsubsets(SALE.PRICE~TOTAL.UNITS+GROSS.SQUARE.FEET+RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data,nvmax = 10,method = "forward")
summary(leaps)
regsummary =summary(leaps)
regsummary$rsq
regsummary$adjr2
plot(regsummary$rsq,xlab="Number of variables", ylab="RSQ",type="l")
plot(regsummary$adjr2,xlab="Number of variables", ylab="Adjusted RSQ",type="l")
plot(regsummary$cp,xlab="Number of variables", ylab="Cp",type="l")
plot(regsummary$bic,xlab="Number of variables", ylab="bic",type="l")

#Linear regression
mod<-glm(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data)
mod1<-glm(SALE.PRICE~TOTAL.UNITS+GROSS.SQUARE.FEET+RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data)
cv.error = cv.glm(data,mod,K=10)
cv.error$delta
cv.error1 = cv.glm(data,mod1,K=10)
cv.error1$delta
lmod<-lm(SALE.PRICE~TOTAL.UNITS*GROSS.SQUARE.FEET*RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data)
lmod1<-lm(SALE.PRICE~TOTAL.UNITS+GROSS.SQUARE.FEET+RESIDENTIAL.UNITS+LAND.SQUARE.FEET,data=data)
summary(lmod)
summary(mod1)

#Ridge regression
set.seed(1)
data2<-dplyr::select(data,TOTAL.UNITS,GROSS.SQUARE.FEET,RESIDENTIAL.UNITS,BLOCK,LOT,LAND.SQUARE.FEET,SALE.PRICE)
x=model.matrix(SALE.PRICE~.,data2)[,-7]
y<-data2$SALE.PRICE
train=sample(nrow(data2),nrow(data2)/2)
test = (-train)
ytest=y[test]
ridge_mod = glmnet(x=x[train,],y=y[train],alpha=0,lambda = 10^seq(10,-2,length=100))
ridge.cv =cv.glmnet(x=x[train,],y=y[train],alpha=0)
plot(ridge.cv)
opt_lambda = ridge.cv$lambda.min
y_predicted = predict(ridge_mod, s=opt_lambda,newx = x[test,])

#MSE
mean((y_predicted-ytest)^2)
#R square
TSS = sum((ytest-mean(ytest))^2)
RSS = sum((y_predicted-ytest)^2)
rsq<-1-(RSS/TSS)
rsq

plot(y_predicted, ytest,xlab="Predicted",ylab="Actual") 
abline(0,1)
#Coefficients of Ridge
out =glmnet(x,y,alpha=0,lambda=10^seq(10,-2,length=100))
lasso.coef  <- predict(out, type = 'coefficients', s = opt_lambda)
lasso.coef

#Lasso
set.seed(1)
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = 10^seq(10,-2,length=100))
cv.out=cv.glmnet(x[train,], y[train],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = opt_lambda, newx = x[test,])
#MSE
mean((lasso.pred-ytest)^2)
#R square
TSS = sum((ytest-mean(ytest))^2)
RSS = sum((lasso.pred-ytest)^2)
rsq<-1-(RSS/TSS)
rsq

plot(lasso.pred, ytest,xlab="Predicted",ylab="Actual") 
abline(0,1)
#Coefficinets of Lasso
out =glmnet(x,y,alpha=1,lambda=10^seq(10,-2,length=100))
lasso.coef  <- predict(out, type = 'coefficients', s = bestlam)
lasso.coef




