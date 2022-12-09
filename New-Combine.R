rm(list = ls())
library(dplyr)
library(haven)
library(tidyverse)
library(janitor)
library(vtree)
library(dplyr)
library(psych)
library(ggplot2)

data1<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2000.csv")
data2<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2005.csv")
data3<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2010.csv")
data4<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2016.csv")
data1$Year<- 2000; data2$Year<- 2005; data3$Year<- 2010; data4$Year<- 2016
data<-rbind(data1, data2, data3, data4)
glimpse(data)
write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\Newdata.csv")

view(priceNames)
data<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Newdata.csv")
attach(data)
data<-data[, -c(1,2)]
glimpse(data)

shared_budget <- spread(data[,c(1,2,5,6)], foodcode1, W)

shared_budget[is.na(shared_budget)] = 0

#shared_budget$RowSum<-rowSums(shared_budget[,3:12])

glimpse(shared_budget)
write.csv(shared_budget, "G:\\Aniee\\Agriculture\\Final CSV\\Newbadget_shared.csv")


priceNames <- spread(data[,c(1,2,3,6)], foodcode1, p_in)

priceNames[is.na(priceNames)] = 0

#priceNames$RowSum<-rowSums(priceNames[,3:12])

glimpse(priceNames)
write.csv(priceNames, "G:\\Aniee\\Agriculture\\Final CSV\\NewpriceNames.csv")

d1<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2000.csv")
d2<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2005.csv")
d3<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2010.csv")
d4<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2016.csv")
d1$Year<- 2000; d2$Year<- 2005; d3$Year<- 2010; d4$Year<- 2016
d<-rbind(d1, d2, d3, d4)
glimpse(d)
write.csv(d, "G:\\Aniee\\Agriculture\\Final CSV\\NewTotal_Expend.csv")

dat1<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\NewpriceNames.csv")
dat2<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Newbadget_shared.csv")
dat3<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\NewTotal_Expend.csv")
dat1<-dat1[,-1]; dat2<-dat2[,-1]; dat3<-dat3[,-c(1,2)]
glimpse(dat1)
glimpse(dat2)
glimpse(dat3)

D<-dat1 %>% right_join(dat2, by=c("hhcode","Year"))
glimpse(D)
names(D) <- gsub(".x", ".p", names(D))
names(D) <- gsub(".y", ".s", names(D))
Data<-D %>% right_join(dat3, by=c("hhcode","Year"))


glimpse(Data)
attach(Data)
view(Data)
write.csv(Data, "G:\\Aniee\\Agriculture\\Final CSV\\NewFinal_Data_with_0.csv")

Data[Data == 0]<-NA
Data<-na.omit(Data)
glimpse(Data)

write.csv(Data, "G:\\Aniee\\Agriculture\\Final CSV\\NewFinal_Data.csv")



