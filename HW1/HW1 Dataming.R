library(tidyverse)
library(arules)
library(here)
library(dplyr)
setwd("C:/Users/emant/Desktop/MSA/AA 502 Analytics Methods/Data Mining/HW1")
orderData <- read.csv("orderData.csv")
orderData <- orderData %>%
  mutate(ID = paste(orderNo, seatNo, sep = ""))  # Create a new ID combining orderNo and seatNo

temp.dat <- orderData %>% select(ID,item)

trans.dat <- as(split(temp.dat$item, temp.dat$ID), "transactions")
trans.dat@itemInfo$labels

any(is.na(orderData))
orderData$
length(unique(temp.dat$ID))
nrow(orderData)/3
rules <- apriori(trans.dat, parameter = list(supp = 0.00001, conf = 0.00001, target = "rules"))

inspect <- inspect(rules)
lhs <- inspect$lhs

rhs <- inspect$rhs

unique(rhs)
vec <- rep(0,length(lhs))
  for (i in 1:length(lhs)) {
  if(lhs[i] == "{}" | grepl(",",lhs[i])){
    vec[i] = 0
  }
    else vec[i] = 1
  }

unique(lhs[which(vec == 1)]) %>% sort()

entrees <- c("{Filet Mignon}",
            "{Pork Tenderloin}",
            "{Duck Breast}",
            "{Salmon}",
            "{Swordfish}",
            "{Sea Bass}",
            "{Pork Chop}",
            "{Roast Chicken}")


wines <- c("{Duckhorn Chardonnay}",
           "{Total Recall Chardonnay}",
           "{Brancott Pinot Grigio}",
           "{Oyster Bay Sauvignon Blanc}",
           "{Innocent Bystander Sauvignon Blanc}",
           "{Cantina Pinot Bianco}",
           "{Adelsheim Pinot Noir}",
           "{Blackstone Merlot}",
           "{Echeverria Gran Syrah}",
           "{Louis Rouge}",
           "{Single Vineyard Malbec}",
           "{Helben Blanc}")


inspect <- data.frame(lhs = inspect$lhs,rhs = inspect$rhs,support = inspect$support,confidence = inspect$confide)


for (i in entrees) { 
  ruleDF <- inspect %>% filter(lhs == i,rhs %in% wines) %>% arrange(-confidence)
  print(sum(ruleDF$confidence))
  print(ruleDF)
  }
