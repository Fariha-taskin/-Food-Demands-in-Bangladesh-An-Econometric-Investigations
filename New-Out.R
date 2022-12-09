rm(list = ls())
library(dplyr)
library(tidyverse)
library(janitor)
library(vtree)
library(psych)
library(ggplot2)
library(haven)
library(forcats)
library(pastecs)
library(data.table)
library(cowplot)
library(ggpubr)
library(moments)
library(GMCM)
library(moments)
library(fmsb)

data<- read.csv("G:\\Aniee\\Agriculture\\Final CSV\\NewFinal_Data_with_0.csv")
glimpse(data)

d1<- data[which(data$Year == 2000), ]
a1 <- boxplot.stats(d1$Cereal.s)$out;     a11<-which(d1$Cereal.s %in% a1)
a2 <- boxplot.stats(d1$Edible.Oil.s)$out; a22<-which(d1$Edible.Oil.s %in% a2)
a3 <- boxplot.stats(d1$Fish.s)$out;       a33<-which(d1$Fish.s %in% a3)
a4 <- boxplot.stats(d1$Fruits.s)$out;     a44<-which(d1$Fruits.s %in% a4)
a5 <- boxplot.stats(d1$Meat.s)$out;       a55<-which(d1$Meat.s %in% a5)
a6 <- boxplot.stats(d1$Milk.s)$out;       a66<-which(d1$Milk.s %in% a6)
a7 <- boxplot.stats(d1$Other.s)$out;      a77<-which(d1$Other.s %in% a7)
a8 <- boxplot.stats(d1$Pulse.s)$out;      a88<-which(d1$Pulse.s %in% a8)
a9 <- boxplot.stats(d1$Spices.s)$out;     a99<-which(d1$Spices.s %in% a9)
a0 <- boxplot.stats(d1$Vegetables.s)$out; a00<-which(d1$Vegetables.s %in% a0)

aa1 <- boxplot.stats(d1$Cereal.p)$out;     aa11<-which(d1$Cereal.p %in% aa1)
aa2 <- boxplot.stats(d1$Edible.Oil.p)$out; aa22<-which(d1$Edible.Oil.p %in% aa2)
aa3 <- boxplot.stats(d1$Fish.p)$out;       aa33<-which(d1$Fish.p %in% aa3)
aa4 <- boxplot.stats(d1$Fruits.p)$out;     aa44<-which(d1$Fruits.p %in% aa4)
aa5 <- boxplot.stats(d1$Meat.p)$out;       aa55<-which(d1$Meat.p %in% aa5)
aa6 <- boxplot.stats(d1$Milk.p)$out;       aa66<-which(d1$Milk.p %in% aa6)
aa7 <- boxplot.stats(d1$Other.p)$out;      aa77<-which(d1$Other.p %in% aa7)
aa8 <- boxplot.stats(d1$Pulse.p)$out;      aa88<-which(d1$Pulse.p %in% aa8)
aa9 <- boxplot.stats(d1$Spices.p)$out;     aa99<-which(d1$Spices.p %in% aa9)
aa0 <- boxplot.stats(d1$Vegetables.p)$out; aa00<-which(d1$Vegetables.p %in% aa0)

a000<-c(a11,a22,a33,a44,a55,a66,a77,a88,a99,a00, aa11,aa22,aa33,aa44,aa55,aa66,aa77,aa88,aa99,aa00)
d11<-d1[-a000,]
write.csv(d11, "G:\\Aniee\\Agriculture\\Final CSV\\Out_Newdata2000.csv")

d2<- data[which(data$Year == 2005), ]
b1 <- boxplot.stats(d2$Cereal.s)$out;     b11<-which(d2$Cereal.s %in% b1)
b2 <- boxplot.stats(d2$Edible.Oil.s)$out; b22<-which(d2$Edible.Oil.s %in% b2)
b3 <- boxplot.stats(d2$Fish.s)$out;       b33<-which(d2$Fish.s %in% b3)
b4 <- boxplot.stats(d2$Fruits.s)$out;     b44<-which(d2$Fruits.s %in% b4)
b5 <- boxplot.stats(d2$Meat.s)$out;       b55<-which(d2$Meat.s %in% b5)
b6 <- boxplot.stats(d2$Milk.s)$out;       b66<-which(d2$Milk.s %in% b6)
b7 <- boxplot.stats(d2$Other.s)$out;      b77<-which(d2$Other.s %in% b7)
b8 <- boxplot.stats(d2$Pulse.s)$out;      b88<-which(d2$Pulse.s %in% b8)
b9 <- boxplot.stats(d2$Spices.s)$out;     b99<-which(d2$Spices.s %in% b9)
b0 <- boxplot.stats(d2$Vegetables.s)$out; b00<-which(d2$Vegetables.s %in% b0)

bb1 <- boxplot.stats(d2$Cereal.p)$out;     bb11<-which(d2$Cereal.p %in% bb1)
bb2 <- boxplot.stats(d2$Edible.Oil.p)$out; bb22<-which(d2$Edible.Oil.p %in% bb2)
bb3 <- boxplot.stats(d2$Fish.p)$out;       bb33<-which(d2$Fish.p %in% bb3)
bb4 <- boxplot.stats(d2$Fruits.p)$out;     bb44<-which(d2$Fruits.p %in% bb4)
bb5 <- boxplot.stats(d2$Meat.p)$out;       bb55<-which(d2$Meat.p %in% bb5)
bb6 <- boxplot.stats(d2$Milk.p)$out;       bb66<-which(d2$Milk.p %in% bb6)
bb7 <- boxplot.stats(d2$Other.p)$out;      bb77<-which(d2$Other.p %in% bb7)
bb8 <- boxplot.stats(d2$Pulse.p)$out;      bb88<-which(d2$Pulse.p %in% bb8)
bb9 <- boxplot.stats(d2$Spices.p)$out;     bb99<-which(d2$Spices.p %in% bb9)
bb0 <- boxplot.stats(d2$Vegetables.p)$out; bb00<-which(d2$Vegetables.p %in% bb0)

b000<-c(b11,b22,b33,b44,b55,b66,b77,b88,b99,b00,bb11,bb22,bb33,bb44,bb55,bb66,bb77,bb88,bb99,bb00)
d22<-d2[-b000,]
write.csv(d22, "G:\\Aniee\\Agriculture\\Final CSV\\Out_Newdata2005.csv")

d3<- data[which(data$Year == 2010), ]
c1 <- boxplot.stats(d3$Cereal.s)$out;     c11<-which(d3$Cereal.s %in% c1)
c2 <- boxplot.stats(d3$Edible.Oil.s)$out; c22<-which(d3$Edible.Oil.s %in% c2)
c3 <- boxplot.stats(d3$Fish.s)$out;       c33<-which(d3$Fish.s %in% c3)
c4 <- boxplot.stats(d3$Fruits.s)$out;     c44<-which(d3$Fruits.s %in% c4)
c5 <- boxplot.stats(d3$Meat.s)$out;       c55<-which(d3$Meat.s %in% c5)
c6 <- boxplot.stats(d3$Milk.s)$out;       c66<-which(d3$Milk.s %in% c6)
c7 <- boxplot.stats(d3$Other.s)$out;      c77<-which(d3$Other.s %in% c7)
c8 <- boxplot.stats(d3$Pulse.s)$out;      c88<-which(d3$Pulse.s %in% c8)
c9 <- boxplot.stats(d3$Spices.s)$out;     c99<-which(d3$Spices.s %in% c9)
c0 <- boxplot.stats(d3$Vegetables.s)$out; c00<-which(d3$Vegetables.s %in% c0)

cc1 <- boxplot.stats(d3$Cereal.p)$out;     cc11<-which(d3$Cereal.p %in% cc1)
cc2 <- boxplot.stats(d3$Edible.Oil.p)$out; cc22<-which(d3$Edible.Oil.p %in% cc2)
cc3 <- boxplot.stats(d3$Fish.p)$out;       cc33<-which(d3$Fish.p %in% cc3)
cc4 <- boxplot.stats(d3$Fruits.p)$out;     cc44<-which(d3$Fruits.p %in% cc4)
cc5 <- boxplot.stats(d3$Meat.p)$out;       cc55<-which(d3$Meat.p %in% cc5)
cc6 <- boxplot.stats(d3$Milk.p)$out;       cc66<-which(d3$Milk.p %in% cc6)
cc7 <- boxplot.stats(d3$Other.p)$out;      cc77<-which(d3$Other.p %in% cc7)
cc8 <- boxplot.stats(d3$Pulse.p)$out;      cc88<-which(d3$Pulse.p %in% cc8)
cc9 <- boxplot.stats(d3$Spices.p)$out;     cc99<-which(d3$Spices.p %in% cc9)
cc0 <- boxplot.stats(d3$Vegetables.p)$out; cc00<-which(d3$Vegetables.p %in% cc0)

c000<-c(c11,c22,c33,c44,c55,c66,c77,c88,c99,c00,cc11,cc22,cc33,cc44,cc55,cc66,cc77,cc88,cc99,cc00)
d33<-d3[-c000,]
write.csv(d33, "G:\\Aniee\\Agriculture\\Final CSV\\Out_Newdata2010.csv")

d4<- data[which(data$Year == 2016), ]
e1 <- boxplot.stats(d3$Cereal.s)$out;     e11<-which(d4$Cereal.s %in% e1)
e2 <- boxplot.stats(d4$Edible.Oil.s)$out; e22<-which(d4$Edible.Oil.s %in% e2)
e3 <- boxplot.stats(d4$Fish.s)$out;       e33<-which(d4$Fish.s %in% e3)
e4 <- boxplot.stats(d4$Fruits.s)$out;     e44<-which(d4$Fruits.s %in% e4)
e5 <- boxplot.stats(d4$Meat.s)$out;       e55<-which(d4$Meat.s %in% e5)
e6 <- boxplot.stats(d4$Milk.s)$out;       e66<-which(d4$Milk.s %in% e6)
e7 <- boxplot.stats(d4$Other.s)$out;      e77<-which(d4$Other.s %in% e7)
e8 <- boxplot.stats(d4$Pulse.s)$out;      e88<-which(d4$Pulse.s %in% e8)
e9 <- boxplot.stats(d4$Spices.s)$out;     e99<-which(d4$Spices.s %in% e9)
e0 <- boxplot.stats(d4$Vegetables.s)$out; e00<-which(d4$Vegetables.s %in% e0)

ee1 <- boxplot.stats(d3$Cereal.p)$out;     ee11<-which(d4$Cereal.p %in% ee1)
ee2 <- boxplot.stats(d4$Edible.Oil.p)$out; ee22<-which(d4$Edible.Oil.p %in% ee2)
ee3 <- boxplot.stats(d4$Fish.p)$out;       ee33<-which(d4$Fish.p %in% ee3)
ee4 <- boxplot.stats(d4$Fruits.p)$out;     ee44<-which(d4$Fruits.p %in% ee4)
ee5 <- boxplot.stats(d4$Meat.p)$out;       ee55<-which(d4$Meat.p %in% ee5)
ee6 <- boxplot.stats(d4$Milk.p)$out;       ee66<-which(d4$Milk.p %in% ee6)
ee7 <- boxplot.stats(d4$Other.p)$out;      ee77<-which(d4$Other.p %in% ee7)
ee8 <- boxplot.stats(d4$Pulse.p)$out;      ee88<-which(d4$Pulse.p %in% ee8)
ee9 <- boxplot.stats(d4$Spices.p)$out;     ee99<-which(d4$Spices.p %in% ee9)
ee0 <- boxplot.stats(d4$Vegetables.p)$out; ee00<-which(d4$Vegetables.p %in% ee0)

e000<-c(e11,e22,e33,e44,e55,e66,e77,e88,e99,e00,ee11,ee22,ee33,ee44,ee55,ee66,ee77,ee88,ee99,ee00)
d44<-d4[-e000,]
write.csv(d44, "G:\\Aniee\\Agriculture\\Final CSV\\Out_Newdata2016.csv")

dataf<-rbind(d11, d22, d33, d44)
glimpse(dataf)
write.csv(dataf, "G:\\Aniee\\Agriculture\\Final CSV\\Out_Newdata.csv")

p11<-ggplot(d11, aes(x=Cereal.p)) + labs(y="Count", x = "Cereal")+ ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 5) 
p12<-ggplot(d22, aes(x=Cereal.p)) + labs(y="Count", x = "Cereal")+ ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 5) 
p13<-ggplot(d33, aes(x=Cereal.p)) + labs(y="Count", x = "Cereal")+ ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 15) 
p14<-ggplot(d44, aes(x=Cereal.p)) + labs(y="Count", x = "Cereal")+ ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150) 
p111<-ggarrange(p11, p12, p13, p14, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p111, top = text_grob("Histogram of Prices of Cereal for different years", 
                                      color = "Blue", face = "bold", size = 14))

p21<-ggplot(d11, aes(x=Edible.Oil.p)) + labs(y="Count", x = "Edible.Oil")+ ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 30) 
p22<-ggplot(d22, aes(x=Edible.Oil.p)) + labs(y="Count", x = "Edible.Oil")+ ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 20) 
p23<-ggplot(d33, aes(x=Edible.Oil.p)) + labs(y="Count", x = "Edible.Oil")+ ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10) 
p24<-ggplot(d44, aes(x=Edible.Oil.p)) + labs(y="Count", x = "Edible.Oil")+ ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150) 
p222<-ggarrange(p21, p22, p23, p24, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p222, top = text_grob("Histogram of Prices of Edible Oil for different years", 
                                      color = "Blue", face = "bold", size = 14))

p31<-ggplot(d11, aes(x=Fish.p )) + labs(y="Count", x = "Fish") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10) 
p32<-ggplot(d22, aes(x=Fish.p )) + labs(y="Count", x = "Fish") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 30) 
p33<-ggplot(d33, aes(x=Fish.p )) + labs(y="Count", x = "Fish") + ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 8) 
p34<-ggplot(d44, aes(x=Fish.p )) + labs(y="Count", x = "Fish") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150) 
p333<-ggarrange(p31, p32, p33, p34, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p333, top = text_grob("Histogram of Prices of Fish for different years", 
                                      color = "Blue", face = "bold", size = 14))
p41<-ggplot(d11, aes(x=Fruits.p)) + labs(y="Count", x = "Fruits") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10) 
p42<-ggplot(d22, aes(x=Fruits.p)) + labs(y="Count", x = "Fruits") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 20) 
p43<-ggplot(d33, aes(x=Fruits.p)) + labs(y="Count", x = "Fruits") + ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 3) 
p44<-ggplot(d44, aes(x=Fruits.p)) + labs(y="Count", x = "Fruits") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 100) 
p444<-ggarrange(p41, p42, p43, p44, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p444, top = text_grob("Histogram of Prices of Fruits for different years", 
                                      color = "Blue", face = "bold", size = 14))

p51<-ggplot(d11, aes(x=Meat.p)) + labs(y="Count", x = "Meat") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 20) 
p52<-ggplot(d22, aes(x=Meat.p)) + labs(y="Count", x = "Meat") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 20) 
p53<-ggplot(d33, aes(x=Meat.p)) + labs(y="Count", x = "Meat") + ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 15) 
p54<-ggplot(d44, aes(x=Meat.p)) + labs(y="Count", x = "Meat") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150)
p555<-ggarrange(p51, p52, p53, p54, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p555, top = text_grob("Histogram of Prices of Meat for different years", 
                                      color = "Blue", face = "bold", size = 14))

p61<-ggplot(d11, aes(x=Milk.p)) + labs(y="Count", x = "Milk") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 60) 
p62<-ggplot(d22, aes(x=Milk.p)) + labs(y="Count", x = "Milk") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 100) 
p63<-ggplot(d33, aes(x=Milk.p)) + labs(y="Count", x = "Milk") + ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10)
p64<-ggplot(d44, aes(x=Milk.p)) + labs(y="Count", x = "Milk") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150) 
p666<-ggarrange(p61, p62, p63, p64, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p666, top = text_grob("Histogram of Prices of Milk for different years", 
                                      color = "Blue", face = "bold", size = 14))

p71<-ggplot(d11, aes(x=Other.p)) + labs(y="Count", x = "Other") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 200) 
p72<-ggplot(d22, aes(x=Other.p)) + labs(y="Count", x = "Other") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150) 
p73<-ggplot(d33, aes(x=Other.p)) + labs(y="Count", x = "Other") + ggtitle("2010")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10)
p74<-ggplot(d44, aes(x=Other.p)) + labs(y="Count", x = "Other") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 200) 
p777<-ggarrange(p71, p72, p73, p74, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p777, top = text_grob("Histogram of Prices of Other for different years", 
                                      color = "Blue", face = "bold", size = 14))

p81<-ggplot(d11, aes(x=Pulse.p)) + labs(y="Count", x = "Pulse") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10) 
p82<-ggplot(d22, aes(x=Pulse.p)) + labs(y="Count", x = "Pulse") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10)
p83<-ggplot(d33, aes(x=Pulse.p)) + labs(y="Count", x = "Pulse") + ggtitle("2010")+ 
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10) 
p84<-ggplot(d44, aes(x=Pulse.p)) + labs(y="Count", x = "Pulse") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150) 
p888<-ggarrange(p81, p82, p83, p84, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p888, top = text_grob("Histogram of Prices of Pulse for different years", 
                                      color = "Blue", face = "bold", size = 14))

p91<-ggplot(d11, aes(x=Spices.p)) + labs(y="Count", x = "Spices") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 30) 
p92<-ggplot(d22, aes(x=Spices.p)) + labs(y="Count", x = "Spices") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ xlim(0, 5000)+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 50)
p93<-ggplot(d33, aes(x=Spices.p)) + labs(y="Count", x = "Spices") + ggtitle("2010")+ 
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 15) 
p94<-ggplot(d44, aes(x=Spices.p)) + labs(y="Count", x = "Spices") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150)
p999<-ggarrange(p91, p92, p93, p94, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p999, top = text_grob("Histogram of Prices of Spices for different years", 
                                      color = "Blue", face = "bold", size = 14))

p101<-ggplot(d11, aes(x=Vegetables.p)) + labs(y="Count", x = "Vegetables") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 2) 
p102<-ggplot(d22, aes(x=Vegetables.p)) + labs(y="Count", x = "Vegetables") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 10) 
p103<-ggplot(d33, aes(x=Vegetables.p)) + labs(y="Count", x = "Vegetables") + ggtitle("2010")+ 
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 2)
p104<-ggplot(d44, aes(x=Vegetables.p)) + labs(y="Count", x = "Vegetables") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 50) 
p000<-ggarrange(p101, p102, p103, p104, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(p000, top = text_grob("Histogram of Prices of Vegetables for different years", 
                                      color = "Blue", face = "bold", size = 14))

t_e1<-ggplot(d11, aes(x=total_expnd)) + labs(y="Count", x = "Total Expenditure") + ggtitle("2000")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 150)
t_e2<-ggplot(d22, aes(x=total_expnd)) + labs(y="Count", x = "Total Expenditure") + ggtitle("2005")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 200)
t_e3<-ggplot(d33, aes(x=total_expnd)) + labs(y="Count", x = "Total Expenditure") + ggtitle("2010")+ 
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 250)
t_e4<-ggplot(d44, aes(x=total_expnd)) + labs(y="Count", x = "Total Expenditure") + ggtitle("2016")+
  theme(axis.text.x = element_text(size=10, face="bold", colour = "black"), 
        axis.text.y = element_text(size=10, face="bold", colour = "black"))+
  font("xlab", size = 12, color = "blue", face="bold")+ 
  geom_histogram(alpha=0.3, fill='blue', colour='black', binwidth= 550)
t_e<-ggarrange(t_e1, t_e2, t_e3, t_e4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2) 
annotate_figure(t_e, top = text_grob("Histogram of Total Expenditure of Food different years", 
                                     color = "Blue", face = "bold", size = 14))
