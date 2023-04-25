
rm(list=ls())
library(rio)
library(moments)
library(dplyr)
library(tidyverse)
library(car)
library(readxl)
df <- read_excel("BigMartSales.xlsx", sheet="Data")
df$age=2023-df$Outlet_Year
# aggregate(df$Item_Sales~df$Outlet_ID,FUN=sum)
# m=lm(df$Item_Sales~df$Outlet_ID+df$Outlet_Type*df$Outlet_ID,data =df)
# summary(m)
# n_distinct(df$Outlet_ID)
# unique(df$Outlet_ID)
# df$Outlet_Size=ifelse(df)
colSums(is.na(df))   
df$Outlet_Size <- ifelse(df$Outlet_Type == "Grocery Store", "Small", df$Outlet_Size)
colSums(is.na(df))  
df$Outlet_Size <- ifelse(df$Outlet_Type == "Supermarket Type2" | df$Outlet_Type == "Supermarket Type3", "Medium", df$Outlet_Size)
colSums(is.na(df))  
colnames(df)=tolower(make.names(colnames(df)))
attach(df)
d=aggregate(item_sales~outlet_id,FUN=sum)
new=d[order(-d$item_sales,d$outlet_id),]
# df$outlet_sales_mean <- merge(df, new, by = "outlet_id")
df$osm=new$item_sales[match(df$outlet_id,new$outlet_id)] # Important function to equivalent of Vlookup
a=median(new$item_sales)
b=mean(new$item_sales)
colSums(is.na(df))
# df$outlet_size<-ifelse(df$osm>b,'Medium','Small')
colSums(is.na(df))
df$outlet_size <- ifelse(is.na(df$outlet_size) | df$outlet_size == 'N', ifelse(df$osm > a, 'Medium', 'Small'), df$outlet_size)
colSums(is.na(df))
str(df)
conv$historysegment=as.factor(conv$historysegment)
df$outlet_id=as.factor(df$outlet_id)
df$outlet_size=as.factor(df$outlet_size)
df$city_type=as.factor(df$city_type)
df$outlet_type=as.factor(df$outlet_type)
library(lattice)
histogram(~item_sales|outlet_type,data=df)
histogram(~item_sales|city_type,data=df)
xyplot(item_sales~outlet_type | city_type*outlet_size, data=df)


df$item_sales=round(df$item_sales)
view(df)
hist(df$item_sales,col='red')
hist(log(item_sales),col='blue')
hist(item_sales*item_sales)
df1 <- subset(df, select = c('outlet_year', 'item_sales', 'age', 'osm'))
library(corrplot)
gil=cor(df1)
corrplot(gil,method="number")
corrplot(gil,method="circle")
m1=lm(log(item_sales)~df$age+df$outlet_size+outlet_size*outlet_type+outlet_type*city_type+outlet_size*outlet_type*city_type,
      data=df)

view(df_subset)
#MODEL1
a1=lm(log(item_sales)~df$age+df$outlet_type+df$city_type,data=df)
summary(a1)

library(car)
vif(a1)

library(MASS)
alias(a1)

# Model2
a3=glm(item_sales~df$age+df$outlet_id+df$outlet_type+df$city_type+df$outlet_size,data=df,family=quasipoisson(link=log))
summary(a3)

# #Model 3 Since There is high collinearity between factor variables i.e they are almost linear i cant use outlet_ID,
# Outlet_type so it's not possible to capture all effect through fixed effect in a single model for that reason a new model to capture effects is made

a2=glm(item_sales~df$age+df$outlet_type+df$city_type+df$outlet_size,data=df,family=quasipoisson(link=log))
summary(a2)

library(AER)
dispersiontest(a3)
library(lme4)

	a4 <- glmer(item_sales ~   age+(1 | city_type/outlet_id) + (1 | outlet_type/outlet_id)  , data = df, family = poisson(link = log))
summary(a4)
ranef(a4)
#Model 4
library(lme4)

a4 <- glmer(item_sales ~   age+(1 | city_type/outlet_id) + (1 | outlet_type/outlet_id)  , data = df, family = poisson(link = log))
summary(a4)
ranef(a4)

#Model 5  
a5 <- glmer(item_sales ~ age+(1| outlet_id)+(1 | city_type) + (1 | outlet_type)  , data = df, family = poisson(link = log))
summary(a5)
ranef(a5)

#THERE IS CORRELATION BETWEEN STORE TYPE AND STORE ID 
library(stargazer)
stargazer(a2,a3,a4,a5,type='text',single.row =TRUE)


# Extract the random effects from a4 and a5 using ranef()
re_a4 <- ranef(a4)
re_a5 <- ranef(a5)

dispersiontest(a2)
dispersiontest(a3)


# Convert to dataframes
df_a4 <- as.data.frame(re_a4)
df_a5 <- as.data.frame(re_a5)

# Add model names as a column
df_a4$model <- "a4"
df_a5$model <- "a5"

# Combine the dataframes
df_combined <- rbind(df_a4, df_a5)

dispersiontest(a4)
