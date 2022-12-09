library(haven)
library(dplyr)
library(tidyverse)
library(forcats)

data<-read_dta("G:\\Aniee\\Agriculture\\Data\\2000\\hh_s9a_food02.dta")
attach(data)
glimpse(data)


fc<-factor(foodcode)

df1 <- data %>%
  filter(day %in% c(1,2,3,4,5,6,7)) %>%
  droplevels()
df1$day
df2 <- data %>%
  filter(day %in% c(8,9,10,11,12,13,14)) %>%
  droplevels()

df1$day<-recode(factor(df1$day), `1` = "first week", `2` = "first week", `3` = "first week", 
                `4` = "first week", `5` = "first week", `6` = "first week", `7` = "first week")
df2$day<-recode(factor(df2$day), `8` = "second week", `9` = "second week", `10` = "second week", 
                `11` = "second week", `12` = "second week", `13` = "second week", `14` = "second week")


df3=df1%>%group_by(foodcode) %>%summarize(sum_quantity=sum(quantity), sum_value=sum(value),na.rm = TRUE)
df3
?summarize

dat<-append(df1, df2)
length(dat)
write_dta(dat, "G:\\Aniee\\Agriculture\\Data\\2000\\dat.dta", label = attr(dat, "label"))
View(data1)
data11<-rbind(df1,df2)
data11<-rename(data11, week = day)

data2<-read_dta("G:\\Aniee\\Agriculture\\Data\\2000\\hh_s9b_food03.dta")
attach(data2)

df21 <- data2 %>%
  filter(week %in% c(1,2,3,4,5,6,7)) %>%
  droplevels()
df1$day
df22 <- data2 %>%
  filter(week %in% c(8,9,10,11,12,13,14)) %>%
  droplevels()

df21$week<-recode(factor(df21$week), `1` = "first week", `2` = "first week", `3` = "first week", 
                  `4` = "first week", `5` = "first week", `6` = "first week", `7` = "first week")
df22$week<-recode(factor(df22$week), `8` = "second week", `9` = "second week", `10` = "second week", 
                  `11` = "second week", `12` = "second week", `13` = "second week", `14` = "second week")
data22<-rbind(df21,df22)


head(data11)
head(data22)
d<-rbind(as.data.frame(data11), as.data.frame(data22))
class(data11)
class(data22)

?append
d<-append(as.data.frame(data11), as.data.frame(data22))
labels(data11$foodcode)
labels(data22$foodcode)
library(haven)
?zap_labels

d<-rbind(zap_labels(data11), zap_labels(data22))



rm(list = ls())
library(dplyr)
library(tidyverse)
library(janitor)
library(vtree)
library(psych)
library(ggplot2)
library(haven)
library(forcats)


data<-read.csv("G:\\Aniee\\Agriculture\\Data\\2000\\Con2000.csv")


which(is.na(data$foodcode))

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


fd <- data %>% arrange(desc(hhcode))
index<-c(length(data$hhcode):1)
data<-fd[index, -c(1,3,4,6,7,11)]
glimpse(data)
#data<-na.omit(data)

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

data$foodcode1<-foodcode11
which(is.na(data))
#data[is.na(data)] = 0

data<-na.omit(data)
attach(data)
glimpse(data)

write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\data_New2000.csv")

data<- read.csv("G:\\Aniee\\Agriculture\\Final CSV\\data_New2000.csv")

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

#rho <- log(data$p/h_base3$p_in)
write.csv(h_base1, "G:\\Aniee\\Agriculture\\Final CSV\\small_wi_New2000.csv")
write.csv(h_base3, "G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2000.csv")
write.csv(h_base4, "G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2000.csv")

