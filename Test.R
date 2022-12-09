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

write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\2000Raw.csv")
glimpse(data)

rm(list = ls())
library(dplyr)
library(tidyverse)
library(janitor)
library(vtree)
library(psych)
library(ggplot2)
library(haven)
library(forcats)


data<-read.csv("G:\\Aniee\\Agriculture\\Data\\2005\\uncn2005.csv")
glimpse(data)
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


#data<-na.omit(data)
data$Year <- 2005
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
data<-na.omit(data)

fd <- data %>% arrange(desc(hhcode))
index<-c(length(data$hhcode):1)
data<-fd[index, -c(1,3)]
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

write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\2005Raw.csv")
glimpse(data)



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




write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\2010Raw.csv")
glimpse(data)


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



data$Year<-2016
data<-data[,-1]
write.csv(data, "G:\\Aniee\\Agriculture\\Final CSV\\2016Raw.csv")
glimpse(data)



data1<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\2000Raw.csv")
data2<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\2005Raw.csv")
data3<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\2010Raw.csv")
data4<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\2016Raw.csv")
data<-rbind(data1,data2,data3,data4)
glimpse(data)
data<-na.omit(data)

Cereal<-data$p[which(data$foodcode1 == "Cereal")]
adf.test(Cereal)
Pulse<-data$p[which(data$foodcode1 == "Pulse")]
adf.test(Pulse)
Fish<-data$p[which(data$foodcode1 == "Fish")]
adf.test(Fish)
Eggs<-data$p[which(data$foodcode1 == "Eggs")]
adf.test(Eggs)
Meat<-data$p[which(data$foodcode1 == "Meat")]
adf.test(Meat)
Vegetables<-data$p[which(data$foodcode1 == "Vegetables")]
adf.test(Vegetables)
Milk<-data$p[which(data$foodcode1 == "Milk")]
adf.test(Milk)
Sweetmeat<-data$p[which(data$foodcode1 == "Sweetmeat")]
adf.test(Sweetmeat)
Edible_Oil<-data$p[which(data$foodcode1 == "Edible Oil")]
adf.test(Edible_Oil)
Fruits<-data$p[which(data$foodcode1 == "Fruits")]
adf.test(Fruits)
Drinks<-data$p[which(data$foodcode1 == "Drinks")]
adf.test(Drinks)
Suger_and_molasses<-data$p[which(data$foodcode1 == "Suger and molasses")]
adf.test(Suger_and_molasses)
Miscellaneous_food<-data$p[which(data$foodcode1 == "Miscellaneous food")]
adf.test(Miscellaneous_food)
Food_outside<-data$p[which(data$foodcode1 == "Food outside")]
adf.test(Food_outside)
Tobacco_&_Tobacco Products<-data$p[which(data$foodcode1 == "Tobacco & Tobacco Products")]
adf.test(Tobacco_&_Tobacco Products)
Spices<-data$p[which(data$foodcode1 == "Spices")]
adf.test(Spices)
Betel_Leaf_and_Chewgoods<-data$p[which(data$foodcode1 == "Betel Leaf and Chewgoods")]
adf.test(Betel_Leaf_and_Chewgoods)
