library(haven)
library(dplyr)

data1<-read_dta("G:\\Aniee\\Agriculture\\Data\\2010\\rt015.dta")
data2<-read_dta("G:\\Aniee\\Agriculture\\Data\\2010\\rt016.dta")
glimpse(data1)
glimpse(data2)
quantity<-sum(c(data1$s09a1d01, data1$s09a1d02, data1$s09a1d03,data1$s09a1d04,data1$s09a1d05,data1$s09a1d06,data1$s09a1d07,
              data1$s09a1d08,data1$s09a1d09,data1$s09a1d10,data1$s09a1d11,data1$s09a1d12,data1$s09a1d13,data1$s09a1d14))

data11<-data.frame("psu" = data1$psu, "hhold"= data1$hhold, "item"= data1$item, "quantity" = rowSums(data1[,c(5,9,13,17,21,25,29,33,37,41,45,49,53,57)]),
                   "value" = rowSums(data1[,c(7,11,15,19,23,27,31,35,39,43,47,51,55,59)]))
glimpse(data11)

data22<-data.frame("psu" = data2$psu, "hhold"= data2$hhold, "item"= data2$item, "quantity" = rowSums(data2[,c(5,9)]),
                   "Value" = rowSums(data2[,c(7,11)]))
glimpse(data22)

#data111<-data.frame("psu" = data1$psu, "hhold"= data1$hhold, "item"= data1$item, "quantity" = rowSums(data1[,c(5,9,13,17,21,25,29,33,37,41,45,49,53,57)]),
                  # "value (Taka)" = rowSums(data1[,c(7,11,15,19,23,27,31,35,39,43,47,51,55,59)]), "Unit" = data1$s09a1_40 )
#glimpse(data111)

#data222<-data.frame("psu" = data2$psu, "hhold"= data2$hhold, "item"= data2$item, "quantity" = rowSums(data2[,c(5,9)]),
                  # "value (Taka)" = rowSums(data2[,c(7,11)]), "Unit" = data2$s09b1w_4 )
#glimpse(data222)

#data00<-rbind(data111,data222)
#glimpse(data00)
#write_dta(data00, "G:\\Aniee\\Agriculture\\Data\\2010\\Food.dta", version = 14)

data<-rbind(data11,data22)
glimpse(data)
view(data2)

write_dta(data)
write.csv(data, "G:\\Aniee\\Agriculture\\Data\\2010\\NewCon2010.csv")

rm(list = ls())
library(dplyr)
library(tidyverse)
library(janitor)
library(vtree)
library(psych)
library(ggplot2)
library(haven)
library(forcats)


data<-read.csv("G:\\Aniee\\Agriculture\\Data\\2010\\Con2010.csv")
glimpse(data)
#view(data)
data<-data[,-c(2,7,8,9)]

which(data$foodcode == 10)
data$foodcode[data$foodcode == 10] <- NA
data$foodcode[data$foodcode == 30] <- NA
data$foodcode[data$foodcode == 40] <- NA
data$foodcode[data$foodcode == 60] <- NA
data$foodcode[data$foodcode == 70] <- NA
data$foodcode[data$foodcode == 80] <- NA
data$foodcode[data$foodcode == 100] <- NA
data$foodcode[data$foodcode == 110] <- NA
data$foodcode[data$foodcode == 120] <- NA
data$foodcode[data$foodcode == 130] <- NA
data$foodcode[data$foodcode == 150] <- NA
data$foodcode[data$foodcode == 160] <- NA
data$foodcode[data$foodcode == 170] <- NA
data$foodcode[data$foodcode == 180] <- NA
data$foodcode[data$foodcode == 200] <- NA
data$foodcode[data$foodcode == 210] <- NA
data$foodcode[data$foodcode == 230] <- NA
index<-which(is.na(data$foodcode))

data<-data[-index,]
glimpse(data)
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
#data<-na.omit(data)
data$value<-data$value/100

fd <- data %>% arrange(desc(hhcode))
index<-c(length(data$hhcode):1)
data<-fd[index, ]
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
View(data)
write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\data_New2010.csv")

data<- read.csv("G:\\Aniee\\Agriculture\\Final CSV\\data_New2010.csv")


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
View(h_base3)

h_base4<-data%>%
  group_by(hhcode) %>% 
  summarise(total_expnd = sum(expend))
h_base4$total_expnd<-(h_base4$total_expnd*30)/(14*10)
glimpse(h_base4)
view(h_base4)
#rho <- log(data$p/h_base3$p_in)
write.csv(h_base1, "G:\\Aniee\\Agriculture\\Final CSV\\small_wi_New2010.csv")
write.csv(h_base3, "G:\\Aniee\\Agriculture\\Final CSV\\Index and WI_New2010.csv")
write.csv(h_base4, "G:\\Aniee\\Agriculture\\Final CSV\\Total_Expend_New2010.csv")
