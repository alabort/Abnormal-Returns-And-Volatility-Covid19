library(quantmod) 
library(reshape2)
library(car)

#define tickers
tickers = c("^GSPC", "^RUA", "^FTSE", "^GDAXI", "^FTAS", "000001.SS", "^SSE50")

#set working directory
setwd("/Users/Alan/Desktop/THESIS/PRACTICAL_PART/DEF_HISTORICAL_VOLATILITY")

#import indices not available on Yahoo finance - this data has been downloaded
#from Investing.com and was saved as a csv file
MIBALLSHARE <- read.csv("FTSEMIBALLSHARE.csv", header = TRUE, sep = ";", dec = ",") 
MIBALLSHARE$Date <- as.character(MIBALLSHARE$Date) 
MIBALLSHARE$Date <- as.Date(MIBALLSHARE$Date,format="%d.%m.%Y") 
MIBALLSHARE <- xts(MIBALLSHARE[,-1], order.by=MIBALLSHARE$Date) 

MIB <- read.csv("FTSEMIB.csv", header = TRUE, sep =";", dec ="," ) 
MIB$Date <-as.character(MIB$Date) 
MIB$Date <-as.Date(MIB$Date,format="%d.%m.%Y")
MIB <- xts(MIB[,-1], order.by=MIB[,1]) 

CLALL <- read.csv("CLALL.csv", header = TRUE, sep = ";", dec = ",") 
CLALL$Date <- as.character(CLALL$Date) 
CLALL$Date <- as.Date(CLALL$Date,format="%d.%m.%Y") 
CLALL <- xts(CLALL[,-1], order.by=CLALL[,1])

#definition of the start and the end of the 2020 period 
startEvent <- as.Date("2020-01-01") 
endEvent <- as.Date("2020-06-30") 

#definition of the start and the end of the 2019 period
startDate<-as.Date("2019-01-01") 
endEstim <- as.Date("2019-12-31")

#download data from Yahoo finance
getSymbols(tickers, src = "yahoo", from = startDate, to = endEvent, auto.assign = TRUE)

#calculate the log-returns
diffGSPC<-diff(log(GSPC$GSPC.Adjusted)) 
diffFTSE<-diff(log(FTSE$FTSE.Adjusted)) 
diffGDAXI<-diff(log(GDAXI$GDAXI.Adjusted)) 
diffMIB<-diff(log(MIB[,1])) 
diffMIBALLSHARE<-diff(log(MIBALLSHARE[,1])) 
diffRUA<-diff(log(RUA$RUA.Adjusted)) 
diffCLALL<-diff(log(CLALL[,1]))
diffFTAS<-diff(log(FTAS$FTAS.Adjusted))
diffSSEC<-diff(log(`000001.SS`$'000001.SS.Adjusted'))
diffSSE50<-diff(log(SSE50$SSE50.Adjusted))

#select the log-returns of the 2019 period
GSPCSubset<- window(diffGSPC, start = startDate, end = endEstim) 
FTSESubset<- window(diffFTSE, start = startDate, end = endEstim) 
GDAXISubset<- window(diffGDAXI, start = startDate, end = endEstim)
MIBSubset<- window(diffMIB, start = startDate, end = endEstim)
MIBALLSHARESubset<- window(diffMIBALLSHARE, start = startDate, end = endEstim) 
RUASubset<- window(diffRUA, start = startDate, end = endEstim) 
CLALLSubset<- window(diffCLALL, start = startDate, end = endEstim)
FTASSubset<- window(diffFTAS, start = startDate, end = endEstim) 
SSECSubset<- window(diffSSEC, start = startDate, end = endEstim)
SSE50Subset<- window(diffSSE50, start = startDate, end = endEstim)

#select the log-returns of the 2020 period
GSPCSubset2<- window(diffGSPC, start = startEvent, end = endEvent) 
FTSESubset2<- window(diffFTSE, start = startEvent, end = endEvent) 
GDAXISubset2<- window(diffGDAXI, start = startEvent, end = endEvent)
MIBSubset2<- window(diffMIB, start = startEvent, end = endEvent)
MIBALLSHARESubset2<- window(diffMIBALLSHARE, start = startEvent, end = endEvent) 
RUASubset2<- window(diffRUA, start = startEvent, end = endEvent) 
CLALLSubset2<- window(diffCLALL, start = startEvent, end = endEvent)
FTASSubset2<- window(diffFTAS, start = startEvent, end = endEvent) 
SSECSubset2<- window(diffSSEC, start = startEvent, end = endEvent)
SSE50Subset2<- window(diffSSE50, start = startEvent, end = endEvent)

#define a function in orer to create a dataframe with stacked data to compute Levene's test
func1 <- function(sample1,sample2) {
  res <- data.frame(
    GroupID <- as.factor(c(rep(1, length(sample1)), rep(2, length(sample2)))),
    DV <- c(sample1, sample2)
  )   
}   

#previous function was applied to the different subsets to create the data frames
longGSPC <- func1(GSPCSubset, GSPCSubset2)
longFTSE <- func1(FTSESubset, FTSESubset2)
longGDAXI <- func1(GDAXISubset, GDAXISubset2)
longMIB <- func1(MIBSubset,MIBSubset2)
longMIBALLSHARE <- func1(MIBALLSHARESubset, MIBALLSHARESubset2)
longRUA <- func1(RUASubset, RUASubset2)
longCLALL <- func1(CLALLSubset, CLALLSubset2)
longFTAS <- func1(FTASSubset, FTASSubset2)
longSSEC <- func1(SSECSubset, SSECSubset2)
longSSE50 <- func1(SSE50Subset, SSE50Subset2)
colnames(longGSPC) <- c("IDnumber", "Returns")
colnames(longFTSE) <- c("IDnumber", "Returns")
colnames(longGDAXI) <- c("IDnumber", "Returns")
colnames(longMIB) <- c("IDnumber", "Returns")
colnames(longMIBALLSHARE) <- c("IDnumber", "Returns")
colnames(longRUA) <- c("IDnumber", "Returns")
colnames(longCLALL) <- c("IDnumber", "Returns")
colnames(longFTAS) <- c("IDnumber", "Returns")
colnames(longSSEC) <- c("IDnumber", "Returns")
colnames(longSSE50) <- c("IDnumber", "Returns")

#computation of Levene's test
GSPC2 <- leveneTest(Returns~IDnumber, longGSPC)
FTSE2 <- leveneTest(Returns~IDnumber, longFTSE)
GDAXI2 <- leveneTest(Returns~IDnumber, longGDAXI)
MIB2 <- leveneTest(Returns~IDnumber, longMIB)
MIBALLSHARE2 <- leveneTest(Returns~IDnumber, longMIBALLSHARE)
RUA2 <- leveneTest(Returns~IDnumber, longRUA)
CLALL2 <- leveneTest(Returns~IDnumber, longCLALL)
FTAS2 <- leveneTest(Returns~IDnumber, longFTAS)
SSEC2 <- leveneTest(Returns~IDnumber,longSSEC)
SSE502 <- leveneTest(Returns~IDnumber, longSSE50)

#export of the data on standard deviations and Levene's tests with p-values
table1 <- data.frame(Column1 = c("S&P 500", "FTSE 100", "DAX", "FTSE MIB", "FTSE MIB ALL SHARE", "RUSSEL 3000", "CLASSIC ALL SHARE", "FTAS", "SSEC", "SSE50"), 
                     Column2 = c(sd(GSPCSubset$GSPC.Adjusted, na.rm = TRUE), sd(FTSESubset$FTSE.Adjusted, na.rm = TRUE), sd(GDAXISubset$GDAXI.Adjusted, na.rm = TRUE), sd(MIBSubset[,1], na.rm = TRUE), sd(MIBALLSHARESubset[,1], na.rm = TRUE), sd(RUASubset$RUA.Adjusted, na.rm = TRUE), sd(CLALLSubset[,1], na.rm = TRUE), sd(FTASSubset$FTAS.Adjusted, na.rm = TRUE), sd(SSECSubset$`000001.SS.Adjusted`, na.rm = TRUE), sd(SSE50Subset$SSE50.Adjusted, na.rm = TRUE)),
                     Column3 = c(sd(GSPCSubset2$GSPC.Adjusted, na.rm = TRUE), sd(FTSESubset2$FTSE.Adjusted, na.rm = TRUE), sd(GDAXISubset2$GDAXI.Adjusted, na.rm = TRUE), sd(MIBSubset2[,1], na.rm = TRUE), sd(MIBALLSHARESubset2[,1], na.rm = TRUE), sd(RUASubset2$RUA.Adjusted, na.rm = TRUE), sd(CLALLSubset[,1], na.rm = TRUE), sd(FTASSubset2$FTAS.Adjusted, na.rm = TRUE), sd(SSECSubset2$`000001.SS.Adjusted`, na.rm = TRUE), sd(SSE50Subset2$SSE50.Adjusted, na.rm = TRUE)),
                     Column4 = c(as.numeric(GSPC2[1,2]), as.numeric(FTSE2[1,2]), as.numeric(GDAXI2[1,2]), as.numeric(MIB2[1,2]), as.numeric(MIBALLSHARE2[1,2]), as.numeric(RUA2[1,2]), as.numeric(CLALL2[1,2]),as.numeric(FTAS2[1,2]), as.numeric(SSEC2[1,2]), as.numeric(SSE502[1,2])),
                     Column5 = c(as.numeric(GSPC2[1,3]), as.numeric(FTSE2[1,3]), as.numeric(GDAXI2[1,3]), as.numeric(MIB2[1,3]), as.numeric(MIBALLSHARE2[1,3]), as.numeric(RUA2[1,3]), as.numeric(CLALL2[1,3]),as.numeric(FTAS2[1,3]), as.numeric(SSEC2[1,3]), as.numeric(SSE502[1,3])))

write.csv(table1, "/Users/Alan/Desktop/PRAT/DEF/table1.csv", row.names = FALSE )#define export directory












