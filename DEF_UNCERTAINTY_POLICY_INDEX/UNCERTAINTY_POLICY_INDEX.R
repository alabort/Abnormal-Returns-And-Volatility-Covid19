rm(list=ls()) 

#set working directoy
setwd("/Users/Alan/Desktop/DEF_UNCERTAINTY_POLICY_INDEX")

#import the data that have been downloaded from https://www.policyuncertainty.com/
US<-read.csv("US.csv", header = TRUE, sep = ";", dec = ".") 
US$Date<-as.character(US$Date) 
US$Date<-as.Date(US$Date,format="%d/%m/%Y") 
US <- xts(US[,2], order.by=US[,1]) 
names(US) = "United States"

CH<-read.csv("CH.csv", header = TRUE, sep = ";", dec = ".") 
CH$Date<-as.character(CH$ï..Date) 
CH$Date<-as.Date(CH$Date,format="%d/%m/%Y") 
CH <- xts(CH[,2], order.by=CH[,5]) 
names(CH)= "China"

EU<-read.csv("EU.csv", header = TRUE, sep = ";", dec = ".") 
EU$Date<-as.character(EU$ï..Date) 
EU$Date<-as.Date(EU$Date,format="%d/%m/%Y") 
EU <- EU[1:403,]
EU <- xts(EU, order.by=EU[,5]) 
names(EU)= c("ll", "Germany", "Italy", "United Kingdom", "Date")

WL<-read.csv("WL.csv", header = TRUE, sep = ";", dec = ".") 
WL$Date<-as.character(WL$ï..Date) 
WL$Date<-as.Date(WL$Date,format="%d/%m/%Y")
WL <- WL[1:283,]
WL <- xts(WL[,2], order.by=WL[,3]) 
names(WL)= "World"

#select time period of the graph
h = na.omit(merge(US$`United States`, CH$China, EU$Germany, EU$Italy, EU$`United Kingdom`, WL$World))
startDate<-as.Date("2019-07-01") 
endDate = as.Date("2020-07-30")
result2 = window(h, start =startDate, end = endDate)

#plot the graph
plot(as.zoo(result2), main = "Uncertainty Policy Index")