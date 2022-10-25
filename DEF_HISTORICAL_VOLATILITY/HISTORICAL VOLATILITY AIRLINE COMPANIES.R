
rm(list=ls()) 

library(quantmod) 
library(reshape2)
library(car)


#define tickers
tickers = c("RYA.L", "EZJ.L", "LHA.DE", "AF.PA", "FIA1S.HE", "600115.SS", "600221.SS", "200152.SZ", "601111.SS", "AAL", "DAL", "LUV", "UAL", "ALK", "^GSPC")

#set working directory
setwd("/Users/Alan/Desktop")

#definition of the start and the end of the 2020 period 
startEvent <- as.Date("2020-01-01") 
endEvent <- as.Date("2020-06-30") 

#definition of the start and the end of the 2019 period
startDate <-as.Date("2019-01-01") 
endEstimation <- as.Date("2019-12-31")

#download data from Yahoo finance
getSymbols(tickers, src = "yahoo", from = startDate, to = endEvent, auto.assign = TRUE)

#calculate the log-returns
diffRYA<-diff(log(RYA.L$RYA.L.Adjusted)) 
diffEZJ<-diff(log(EZJ.L$EZJ.L.Adjusted)) 
diffLHA<-diff(log(LHA.DE$LHA.DE.Adjusted)) 
diffAFR<-diff(log(AF.PA$AF.PA.Adjusted)) 
diffFIA<-diff(log(FIA1S.HE$FIA1S.HE.Adjusted)) 
diffCEA<-diff(log(`600115.SS`$'600115.SS.Adjusted')) 
diffHAI<-diff(log(`600221.SS`$'600221.SS.Adjusted'))
diffSHA<-diff(log(`200152.SZ`$'200152.SZ.Adjusted')) 
diffAIR<-diff(log(`601111.SS`$'601111.SS.Adjusted')) 
diffAAL<-diff(log(AAL$AAL.Adjusted)) 
diffDAL<-diff(log(DAL$DAL.Adjusted)) 
diffLUV<-diff(log(LUV$LUV.Adjusted)) 
diffUAL<-diff(log(UAL$UAL.Adjusted)) 
diffALK<-diff(log(ALK$ALK.Adjusted))

#select the log-returns of the 2019 period
RYASubset<- window(diffRYA, start = startDate, end = endEstimation) 
EZJSubset<- window(diffEZJ, start = startDate, end = endEstimation) 
LHASubset<- window(diffLHA, start = startDate, end = endEstimation) 
AFRSubset<- window(diffAFR, start = startDate, end = endEstimation)
FIASubset<- window(diffFIA, start = startDate, end = endEstimation) 
CEASubset<- window(diffCEA, start = startDate, end = endEstimation) 
HAISubset<- window(diffHAI, start = startDate, end = endEstimation) 
SHASubset<- window(diffSHA, start = startDate, end = endEstimation) 
AIRSubset<- window(diffAIR, start = startDate, end = endEstimation)
AALSubset<- window(diffAAL, start = startDate, end = endEstimation) 
DALSubset<- window(diffDAL, start = startDate, end = endEstimation) 
LUVSubset<- window(diffLUV, start = startDate, end = endEstimation) 
UALSubset<- window(diffUAL, start = startDate, end = endEstimation)
ALKSubset<- window(diffALK, start = startDate, end = endEstimation) 

#select the log-returns of the 2020 period
RYASubset2<- window(diffRYA, start = startEvent, end = endEvent) 
EZJSubset2<- window(diffEZJ, start = startEvent, end = endEvent) 
LHASubset2<- window(diffLHA, start = startEvent, end = endEvent) 
AFRSubset2<- window(diffAFR, start = startEvent, end = endEvent)
FIASubset2<- window(diffFIA, start = startEvent, end = endEvent) 
CEASubset2<- window(diffCEA, start = startEvent, end = endEvent) 
HAISubset2<- window(diffHAI, start = startEvent, end = endEvent) 
SHASubset2<- window(diffSHA, start = startEvent, end = endEvent) 
AIRSubset2<- window(diffAIR, start = startEvent, end = endEvent)
AALSubset2<- window(diffAAL, start = startEvent, end = endEvent) 
DALSubset2<- window(diffDAL, start = startEvent, end = endEvent) 
LUVSubset2<- window(diffLUV, start = startEvent, end = endEvent) 
UALSubset2<- window(diffUAL, start = startEvent, end = endEvent)
ALKSubset2<- window(diffALK, start = startEvent, end = endEvent) 

#define a function in orer to create a dataframe with stacked data to compute Levene's test
func1 <- function(sample1,sample2) {
  res <- data.frame(
    GroupID=as.factor(c(rep(1, length(sample1)), rep(2, length(sample2)))),
    DV=c(sample1, sample2)
  )   
}   

#previous function was applied to the different subsets to create the data frames
longRYA <- func1(RYASubset, RYASubset2)
longEZJ <- func1(EZJSubset, EZJSubset2)
longLHA <- func1(LHASubset, LHASubset2)
longAFR <- func1(AFRSubset, AFRSubset2)
longFIA <- func1(FIASubset, FIASubset2)
longCEA <- func1(CEASubset, CEASubset2)
longHAI <- func1(HAISubset, HAISubset2)
longSHA <- func1(SHASubset, SHASubset2)
longAIR <- func1(AIRSubset, AIRSubset2)
longAAL <- func1(AALSubset, AALSubset2)
longDAL <- func1(DALSubset, DALSubset2)
longLUV <- func1(LUVSubset, LUVSubset2)
longUAL <- func1(UALSubset, UALSubset2)
longALK <- func1(ALKSubset, ALKSubset2)

#computation of Levene's test
RYA2 <- leveneTest(RYA.L.Adjusted~GroupID, longRYA)
EZJ2 <- leveneTest(EZJ.L.Adjusted~GroupID, longEZJ)
LHA2 <- leveneTest(LHA.DE.Adjusted~GroupID, longLHA)
AFR2 <- leveneTest(AF.PA.Adjusted~GroupID, longAFR)
FIA2 <- leveneTest(FIA1S.HE.Adjusted~GroupID, longFIA)
CEA2 <- leveneTest(X600115.SS.Adjusted~GroupID, longCEA)
HAI2 <- leveneTest(X600221.SS.Adjusted~GroupID, longHAI)
SHA2 <- leveneTest(X200152.SZ.Adjusted~GroupID, longSHA)
AIR2 <- leveneTest(X601111.SS.Adjusted~GroupID,longAIR)
AAL2 <- leveneTest(AAL.Adjusted~GroupID, longAAL)
DAL2 <- leveneTest(DAL.Adjusted~GroupID, longDAL)
LUV2 <- leveneTest(LUV.Adjusted~GroupID, longLUV)
UAL2 <- leveneTest(UAL.Adjusted~GroupID, longUAL)
ALK2 <- leveneTest(ALK.Adjusted~GroupID, longALK)

#export of the data on standard deviations and Levene's tests with p-values
table1 <- data.frame(Column1 = c("RYA", "EZJ", "LHA", "AFR", "FIA", "CEA", "HAI", "SHA", "AIR", "AAL", "DAL", "LUV", "UAL", "ALK"),
                    Column2 = c(sd(RYASubset$RYA.L.Adjusted, na.rm = TRUE), sd(EZJSubset$EZJ.L.Adjusted, na.rm = TRUE), sd(LHASubset$LHA.DE.Adjusted, na.rm = TRUE), sd(AFRSubset$AF.PA.Adjusted, na.rm = TRUE), sd(FIASubset$FIA1S.HE.Adjusted, na.rm = TRUE), sd(CEASubset$`600115.SS.Adjusted`, na.rm = TRUE), sd(HAISubset$`600221.SS.Adjusted`, na.rm = TRUE), sd(SHASubset$`200152.SZ.Adjusted`, na.rm = TRUE), sd(AIRSubset$`601111.SS.Adjusted`, na.rm = TRUE), sd(AALSubset$AAL.Adjusted, na.rm = TRUE), sd(DALSubset$DAL.Adjusted, na.rm = TRUE), sd(LUVSubset$LUV.Adjusted, na.rm = TRUE), sd(UALSubset$UAL.Adjusted, na.rm = TRUE), sd(ALKSubset$ALK.Adjusted, na.rm = TRUE)),
                     Column3 = c(sd(RYASubset2$RYA.L.Adjusted, na.rm = TRUE), sd(EZJSubset2$EZJ.L.Adjusted, na.rm = TRUE), sd(LHASubset2$LHA.DE.Adjusted, na.rm = TRUE), sd(AFRSubset2$AF.PA.Adjusted, na.rm = TRUE), sd(FIASubset2$FIA1S.HE.Adjusted, na.rm = TRUE), sd(CEASubset2$`600115.SS.Adjusted`, na.rm = TRUE), sd(HAISubset2$`600221.SS.Adjusted`, na.rm = TRUE), sd(SHASubset2$`200152.SZ.Adjusted`, na.rm = TRUE), sd(AIRSubset2$`601111.SS.Adjusted`, na.rm = TRUE), sd(AALSubset2$AAL.Adjusted, na.rm = TRUE), sd(DALSubset2$DAL.Adjusted, na.rm = TRUE), sd(LUVSubset2$LUV.Adjusted, na.rm = TRUE), sd(UALSubset2$UAL.Adjusted, na.rm = TRUE), sd(ALKSubset2$ALK.Adjusted, na.rm = TRUE)),
                     Column4 = c(as.numeric(RYA2[1,2]), as.numeric(EZJ2[1,2]), as.numeric(LHA2[1,2]), as.numeric(AFR2[1,2]), as.numeric(FIA2[1,2]), as.numeric(CEA2[1,2]), as.numeric(HAI2[1,2]), as.numeric(SHA2[1,2]), as.numeric(AIR2[1,2]), as.numeric(AAL2[1,2]), as.numeric(DAL2[1,2]), as.numeric(LUV2[1,2]), as.numeric(UAL2[1,2]), as.numeric(ALK2[1,2])),
                     Column5 = c(as.numeric(RYA2[1,3]), as.numeric(EZJ2[1,3]), as.numeric(LHA2[1,3]), as.numeric(AFR2[1,3]), as.numeric(FIA2[1,3]), as.numeric(CEA2[1,3]), as.numeric(HAI2[1,3]), as.numeric(SHA2[1,3]), as.numeric(AIR2[1,3]), as.numeric(AAL2[1,3]), as.numeric(DAL2[1,3]), as.numeric(LUV2[1,3]), as.numeric(UAL2[1,3]), as.numeric(ALK2[1,3])))

write.csv(table1, "/Users/Alan/Desktop/PRAT/DEF/table2.csv", row.names = FALSE )#define export directory

