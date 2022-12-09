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
library(micEconAids)

data<- read.csv("G:\\Aniee\\Agriculture\\Final CSV\\NewFinal_Data.csv")
glimpse(data)

d1<- boxplot.stats(data$Cereal.s)$out;      d11<-which(data$Cereal.s %in% d1)
d2 <- boxplot.stats(data$Edible.Oil.s)$out; d22<-which(data$Edible.Oil.s %in% d2)
d3 <- boxplot.stats(data$Fish.s)$out;       d33<-which(data$Fish.s %in% d3)
d4 <- boxplot.stats(data$Fruits.s)$out;     d44<-which(data$Fruits.s %in% d4)
d5 <- boxplot.stats(data$Meat.s)$out;       d55<-which(data$Meat.s %in% d5)
d6 <- boxplot.stats(data$Milk.s)$out;       d66<-which(data$Milk.s %in% d6)
d7 <- boxplot.stats(data$Other.s)$out;      d77<-which(data$Other.s %in% d7)
d8 <- boxplot.stats(data$Pulse.s)$out;      d88<-which(data$Pulse.s %in% d8)
d9 <- boxplot.stats(data$Spices.s)$out;     d99<-which(data$Spices.s %in% d9)
d0 <- boxplot.stats(data$Vegetables.s)$out; d00<-which(data$Vegetables.s %in% d0)
d000<-c(d11,d22,d33,d44,d55,d66,d77,d88,d99,d00)
d11<-data[-d000,]
write.csv(d11, "G:\\Aniee\\Agriculture\\Final CSV\\Out_New_Data.csv")



#======================================Model Estimation=======================================#
rm(list = ls())
data<-read.csv("G:\\Aniee\\Agriculture\\Final CSV\\Out_New_Data.csv")
glimpse(data)
attach(data)
priceNames<-c("Cereal.p", "Edible.Oil.p", "Fish.p", "Fruits.p", "Meat.p", "Milk.p", "Other.p", "Pulse.p", "Spices.p", "Vegetables.p")
shareNames<-c("Cereal.s", "Edible.Oil.s", "Fish.s", "Fruits.s", "Meat.s", "Milk.s", "Other.s", "Pulse.s", "Spices.s", "Vegetables.s")

laaidsresult <- aidsEst( priceNames, shareNames, "total_expnd", data = data, priceIndex = "P")
print(laaidsresult)
summary(laaidsresult)
checkConsist(laaidsresult, observedShares = TRUE)


diag(elas(laaidsresult, method = "AIDS")$marshall)
diag(elas(laaidsresult, method = "AIDS")$hicks)

pMeans<- colMeans(data[, priceNames])
wMeans<- colMeans(data[, shareNames])
aidsResultElas<- aidsElas(coef(laaidsresult), prices = pMeans, shares = wMeans)
#print(xtable((aidsResultElas)),type='html')

print(aidsResultElas)

gamma<-laaidsresult$coef$gamma
constant<-laaidsresult$coef$alpha
stonePrice<-laaidsresult$coef$beta
R_sq<-summary(laaidsresult)$r2
coeffi<-rbind(constant, gamma, stonePrice, R_sq)
write.csv(coeffi, "G:\\Aniee\\Agriculture\\Final CSV\\Coeffi_New.csv")
stat<-summary(laaidsresult)$coef$stat
write.csv(stat, "G:\\Aniee\\Agriculture\\Final CSV\\stat_New.csv")

marshall <- aidsResultElas$marshall
hicksian <- aidsResultElas$hicks
Uncompensated <- diag(marshall)
Compensated <- diag(hicksian)
Elasticity <- aidsResultElas$exp

p_elasti<-data.frame(Uncompensated, Compensated, "Expenditure Elasticity" = Elasticity)
write.csv(p_elasti, "G:\\Aniee\\Agriculture\\Final CSV\\price_elasticity_New.csv")

write.csv(marshall, "G:\\Aniee\\Agriculture\\Final CSV\\marshall_New.csv")

write.csv(hicksian, "G:\\Aniee\\Agriculture\\Final CSV\\hicksian_New.csv")

#=====================================Extra========================================#
aidsresult <- aidsEst( priceNames, shareNames, "total_expnd", data = data, method = "IL")
print(aidsresult)
summary(aidsresult)
checkConsist(aidsresult, observedShares = TRUE)
AIC(laaidsresult,aidsresult)
aidsresultHom <- aidsEst( priceNames, shareNames, "total_expnd", data = data, method = "IL", sym = FALSE)

priceNameshom<-c("Cereal.p", "Edible.Oil.p", "Fish.p", "Fruits.p", "Meat.p", "Milk.p", "Pulse.p", "Spices.p", "Vegetables.p")
shareNameshom<-c("Cereal.s", "Edible.Oil.s", "Fish.s", "Fruits.s", "Meat.s", "Milk.s", "Pulse.s", "Spices.s", "Vegetables.s")

laaidsresulthom <- aidsEst( priceNameshom, shareNameshom, "total_expnd", data = data, priceIndex = "P")
print(laaidsresulthom)
summary(laaidsresulthom)
checkConsist(laaidsresulthom, observedShares = TRUE)
lrtest(laaidsresult,laaidsresulthom)
