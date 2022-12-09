rm(list = ls())
library(dplyr)
library(tidyverse)
library(janitor)
library(vtree)
library(psych)
library(ggplot2)
library(haven)
library(forcats)


data1<-read_dta("G:\\Aniee\\Agriculture\\Data\\2016(new)\\HH_SEC_9A2.dta")
data2<-read_dta("G:\\Aniee\\Agriculture\\Data\\2016(new)\\HH_SEC_9B2.dta")
attach(data1)
attach(data2)
glimpse(data1)
glimpse(data2)
data1<-rename(data1, hhcode = hhold, week = day, foodcode = item, quantity = s9a2q02, unite = s9a2q03, value = s9a2q04, source = s9a2q05)
data2<-rename(data2, hhcode = hhold, foodcode = s9bq01, quantity = s9bq02, unite = s9b2q03, value = s9bq04, source = s9bbq05)
data1<-data1[,c(1,2,3,4,5,7)]
data2<-data2[,c(3,4,5,6,7,9)]
data1$week<-as.numeric(data1$week)
data2$week<-as.numeric(data2$week)

data1$week<-recode(factor(data1$week), `1` = "first week", `2` = "first week", `3` = "first week", 
                 `4` = "first week", `5` = "first week", `6` = "first week", `7` = "first week")
data1$week<-recode(factor(data1$week), `8` = "second week", `9` = "second week", `10` = "second week", 
                 `11` = "second week", `12` = "second week", `13` = "second week", `14` = "second week")
data2$week<-recode(factor(data2$week), `1` = "first week", `2` = "second week")

data1$foodcode<-as.numeric(data1$foodcode)
data2$foodcode<-as.numeric(data2$foodcode)
glimpse(data1)
glimpse(data2)
data<-rbind(data1, data2)
glimpse(data)
attach(data)

write.csv(data, "G:\\Aniee\\Agriculture\\Data\\2016(new)\\Results\\Con_New_2016.csv")


rm(list = ls())
data<-read.csv("G:\\Aniee\\Agriculture\\Data\\2016(new)\\Results\\Con_New_2016.csv")
which(data$foodcode == 10)

glimpse(data)
which(is.na(data$foodcode))
data<-na.omit(data)

i1<-which(data$foodcode == 61) 
i2<-which(data$foodcode == 62) 
i3<-which(data$foodcode == 63) 
i4<-which(data$foodcode == 163) 
i5<-which(data$foodcode == 164) 
i6<-which(data$foodcode == 165) 
i7<-which(data$foodcode == 191) 
i8<-which(data$foodcode == 192) 
i9<-which(data$foodcode == 193) 
i10<-which(data$foodcode == 201) 
i11<-which(data$foodcode == 203) 
I<-c(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11)

data$quantity[-I]<-data$quantity[-I]/1000

which(is.na(data$foodcode))
data<-na.omit(data)
#data$value<-data$value/100

fd <- data %>% arrange(desc(hhcode))
index<-c(length(data$hhcode):1)
data<-fd[index, -c(1,3)]

glimpse(data)
data<-na.omit(data)

data$expend<-(data$value)
data$q<-(data$quantity)
data$p<-round(data$expend/data$q, 4)
data$p[data$p== Inf]<-0
#data$p[data$p == NA]<-0

data[is.na(data)]<-0
which(data$p == 0)
which(is.na(data$p))
which(is.na(data$q))

#data<-na.omit(data)
which(is.na(data))
glimpse(data)
attach(data)
sum(p)
which(data == 0)
#data[data == 0]<-NA
#data<-na.omit(data)




data$foodcode1 <- ifelse(foodcode<=24, "Cereal", 
                         ifelse(foodcode >29 & foodcode<=36, "Pulse", 
                                ifelse(foodcode >39 & foodcode<=58, "Fish",
                                       ifelse(foodcode >59 & foodcode<=63, "Eggs", 
                                              ifelse(foodcode >69 & foodcode<=77, "Meat", 
                                                     ifelse(foodcode >79 & foodcode<=97, "Vegetables", 
                                                            ifelse(foodcode >99 & foodcode<=106, "Milk", 
                                                                   ifelse(foodcode >109 & foodcode<=114, "Sweetmeat", 
                                                                          ifelse(foodcode >119 & foodcode<=126, "Edible Oil", 
                                                                                 ifelse(foodcode >129 & foodcode<=148, "Fruits",
                                                                                        ifelse(foodcode >149 & foodcode<=156, "Drinks", 
                                                                                               ifelse(foodcode >159 & foodcode<=166, "Suger and molasses", 
                                                                                                      ifelse(foodcode >169 & foodcode<=175, "Miscellaneous food", 
                                                                                                             ifelse(foodcode >179 & foodcode<=195, "Food outside", 
                                                                                                                    ifelse(foodcode >199 & foodcode<=204, "Tobacco & Tobacco Products",
                                                                                                                           ifelse(foodcode >209 & foodcode<=223, "Spices", 
                                                                                                                                  ifelse(foodcode >229 & foodcode<=237, "Betel Leaf and Chewgoods", 
                                                                                                                                         NA)))))))))))))))))



foodcode11 = recode(data$foodcode1, "Drinks"= "Other", "Eggs"= "Other", "Food outside"= "Other", 
                    "Miscellaneous food"= "Other","Sweetmeat"= "Other", "Suger and molasses"= "Other", 
                    "Tobacco & Tobacco Products"= "Other", "Betel Leaf and Chewgoods" = "Other")
#View(foodcode11)
data$foodcode1<-foodcode11
which(is.na(data))
#View(data)

data<-na.omit(data)
attach(data)
glimpse(data)

write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\data_New2016.csv")

data<-read.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\data_New2016.csv")

#write_dta(data, "G:\\Aniee\\Agriculture\\Data\\2016(new)\\data_New2016.dta")

h_base0<-data%>%
  group_by(hhcode, foodcode, foodcode1) %>% 
  summarise(p_0 = sum(p), q_0  = sum(q), e_0 = sum(expend))
h_base0$q_0<-(h_base0$q_0*30)/14
h_base0$e_0<-(h_base0$e_0*30)/14
glimpse(h_base0)

h_base1<-h_base0%>%
  group_by(hhcode) %>% 
  summarise(foodcode, foodcode1, p_0, q_0, e_0, W_0 = e_0/sum(e_0))
glimpse(h_base1)

h_base2<-h_base1%>%
  group_by(hhcode,  foodcode1) %>% 
  summarise(foodcode, w = e_0/sum(e_0), p_0, q_0, e_0, W_0)
glimpse(h_base2)

h_base3<-h_base2%>%
  group_by(hhcode,  foodcode1) %>% 
  summarise(p_in  = sum(w*p_0), q_in = sum(w*q_0), W = sum(W_0))
glimpse(h_base3)

h_base4<-data%>%
  group_by(hhcode) %>% 
  summarise(total_expnd = sum(expend))
h_base4$total_expnd<-(h_base4$total_expnd*30)/14
glimpse(h_base4)

write.csv(h_base1, "G:\\Aniee\\Agriculture\\Final CSV\\small_wi_New2016.csv")
write.csv(h_base3, "G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2016.csv")
write.csv(h_base4, "G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2016.csv")

