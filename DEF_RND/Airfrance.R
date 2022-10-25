rm(list=ls()) 

########################################### MAIN PART ##########################################################
# import necessary libraries 
library("RND")
library("tidyverse")
library("moments")
library("xts")
require(lubridate)

#change the working directory
setwd("/Users/Alan/Desktop/THESIS/PRACTICAL_PART/DEF_RND_COMPANIES")

#select the date of the desired RND (it is necessary to change the date also one more time later in row 68)
Selected_Date <- "16/04/2020"
Selected_Date2 <- "16/04/20"

#import the prices of the underlying and filter the price of the wanted day
Price <-  read.csv("AFS0.csv", header = TRUE, sep = ";", dec=".")
Exact2<- Price %>% filter(Price$Timestamp == Selected_Date) 
s0 = as.numeric(Exact2$Trade.Close)

#computation of the time to maturity for for both maturities 
te1 = as.numeric((as.Date("2020-09-18")-as.Date(Selected_Date, format = "%d/%m/%y"))/365) 
te2 = as.numeric((as.Date("2020-12-18")-as.Date(Selected_Date, format = "%d/%m/%y"))/365) 

#import the LIBOR rates for euro and filter the rates for the selected date 
#the 1M, 3M, 6M and 12M rates have been downloaded from FRED
Rates <- read.csv("EUR.csv", header = TRUE, sep = ";", dec=".")
Exact <- Rates %>% filter(Rates$ï..Data == Selected_Date)

#the package and the function that has been used allows to sum months to dates
ad <- ymd(as.Date(Selected_Date2, format = "%d/%m/%y"), tz = "US/Pacific")   
marketDates <- c(ad,  ad + months(1), ad + months(3), ad + months(6), ad + years(1))

# substring help to delete the "UTC"/time zone after the dates
marketDates <- as.Date(substring(marketDates, 1, 10))

# the data is in percentage so it should be converted to decimal by multiplying by 0.01: 
marketRates <- c(0.0, Exact$X1M, Exact$X3M, Exact$X6M, Exact$X12M) * 0.01
numRates <- length(marketRates)
marketData.xts <- as.xts(marketRates, order.by = marketDates)

#Creating a function in order to create a xts object with the rates that will be useful for the interpolation 
#of the rates
func1 <- function(anchorDate, plusYears)
{  
  endDate <- anchorDate + years(plusYears)
  numDays <- endDate - anchorDate  
  xts.termStruct <- xts(rep(NA, numDays + 1), as.Date(anchorDate) + 0:numDays)
  return(xts.termStruct)
}

#Creation of the xts object
termStruct <- func1(ad, 2)
for(i in (1:numRates)) termStruct[marketDates[i]] <- marketData.xts[marketDates[i]]
termStruct.spline.interpolate <- na.approx(termStruct)
termStruct.spline.interpolate <- as.data.frame(termStruct.spline.interpolate)

#linear interpolation of rates
r1 <- as.numeric(as.vector(termStruct.spline.interpolate["2020-09-18", 1]))
r2 <- as.numeric(as.vector(termStruct.spline.interpolate["2020-12-18", 1]))

#Import option data form the CSV file
#The following data has been downloaded in accordance with the library from the database Eikon
Main_table <- read.csv("AF.csv", header = TRUE, sep = ",", dec=".")
#remember to change the date in the following line, the format should be X.dd.mm.yyyy
Date_table <- data.frame(Main_table$X16.04.2020, Main_table$Strike, Main_table$Expiry.Date, Main_table$X.NAME..y)
Date_table2 <- Date_table[complete.cases(Date_table), ]
Date_table_put <- Date_table2 %>% filter(Main_table.X.NAME..y== "PUT ")
Date_table_call <- Date_table2 %>% filter(Main_table.X.NAME..y== "CALL")
colnames(Date_table_call) <- c("Price", "Strike", "Expiry.date", "Type")
colnames(Date_table_put) <- c("Price", "Strike", "Expiry.date", "Type")

#filter the put and calls according to the chosen maturities
Put_mat1 <- Date_table_put %>% filter(Expiry.date == "18/09/2020")
Call_mat1 <- Date_table_call %>% filter(Expiry.date == "18/09/2020")
Put_mat2 <- Date_table_put %>% filter(Expiry.date == "18/12/2020")
Call_mat2 <- Date_table_call %>% filter(Expiry.date == "18/12/2020")

Put_mat1 <- Put_mat1[order(as.numeric(as.vector(Put_mat1$Price))),]
Call_mat1 <- Call_mat1[order(-as.numeric(as.vector(Call_mat1$Price))),]
Put_mat2 <- Put_mat2[order(as.numeric(as.vector(Put_mat2$Price))),]
Call_mat2 <- Call_mat2[order(-as.numeric(as.vector(Call_mat2$Price))),]

Put_mat1[,2] <- as.numeric(as.vector(Put_mat1$Strike))
Put_mat2[,2] <- as.numeric(as.vector(Put_mat2$Strike))
Call_mat1[,2] <- as.numeric(as.vector(Call_mat1$Strike))
Call_mat2[,2] <- as.numeric(as.vector(Call_mat2$Strike))

#calculation of forward price
f1=s0*exp(r1*te1)
f2=s0*exp(r2*te2)

#filter options according to their moneyness
Call_mat1 <- cbind(Call_mat1, xfcs = (as.numeric(as.vector(Call_mat1$Strike))/(f1)))
Call_mat1 <- Call_mat1 %>% filter(xfcs> 0.5 & xfcs <1.5)
Call_mat2 <- cbind(Call_mat2, xfcs = as.numeric(as.vector(Call_mat2$Strike))/(f2))
Call_mat2 <- Call_mat2 %>% filter(xfcs> 0.5 & xfcs <1.5)
Put_mat1 <- cbind(Put_mat1, xfcs = as.numeric(as.vector(Put_mat1$Strike))/(f1))
Put_mat1 <- Put_mat1 %>% filter(xfcs> 0.5 & xfcs <1.5)
Put_mat2 <- cbind(Put_mat2, xfcs = as.numeric(as.vector(Put_mat2$Strike))/(f2))
Put_mat2 <- Put_mat2 %>% filter(xfcs> 0.5 & xfcs <1.5)

xls <-c(as.numeric(as.vector(Put_mat1$Strike)),as.numeric(as.vector(Put_mat1$Strike)))
Call9m <- Call_mat1  %>% filter(Strike %in% xls[duplicated(xls)])
Put9m <- Put_mat1 %>% filter(Strike %in% xls[duplicated(xls)])
xls2 <-c(Call_mat2$Strike,Put_mat2$Strike)
Call1y <- Call_mat2  %>% filter(Strike %in% xls2[duplicated(xls2)])
Put1y <- Put_mat2 %>% filter(Strike %in% xls2[duplicated(xls2)])

#extract the parameters that will be useful to plot RNDs
optim.obj.no.init1 = extract.am.density(r = r1, te = te1, s0 = s0, market.calls = Call9m$Price, market.puts = Put9m$Price, strikes = Call9m$Strike, lambda = 1, hessian.flag = FALSE)
optim.obj.no.init2 = extract.am.density(r = r2, te = te2, s0 = s0, market.calls = Call1y$Price, market.puts = Put1y$Price, strikes = Call1y$Strike, lambda = 1, hessian.flag = FALSE)

#insert parameters extracted with the previous function in multiple variables
o1 = optim.obj.no.init1
o2 = optim.obj.no.init2

#calculate density for each price
x = 0:20
y.1 = dmln.am(x = x, p.1 = o1$p.1, p.2 = o1$p.2, u.1 = o1$u.1, u.2 = o1$u.2, u.3 = o1$u.3,  sigma.1 = o1$sigma.1, sigma.2 = o1$sigma.2, sigma.3 = o1$sigma.3 )
y.2 = dmln.am(x = x, p.1 = o2$p.1, p.2 = o2$p.2, u.1 = o2$u.1, u.2 = o2$u.2, u.3 = o2$u.3,  sigma.1 = o2$sigma.1, sigma.2 = o2$sigma.2, sigma.3 = o2$sigma.3 )

#plot RND
matplot(x,cbind(y.1, y.2) ,type="l", col=c("black", "blue"), xlab="Prices", ylab="Density", 
        lwd=c(2,2,2,2,2), lty = c(1,1,1,1,1), cex.axis = 1.25, cex.lab = 1.25, main= Selected_Date)

legend("topright", legend=c("18-09-2020", "18-12-2020"), col=c("black","blue","red"), 
       lwd = c(2,2,2,2,2), lty = c(1,1,1,1,1), bty="n", cex=1.25)

########################### SUMMARY STATISTICS ############################################################################

#MATURITY 1
#Calculation of the mean of the RND function (using the statistical properties of the density functions)
n <- length(y.1)                     
df <- mean(diff(x))                 
y.unit <- sum(y.1) * df               
df <- df / y.unit                       
x.mean <- sum(y.1 * x) * df
y.mean <- y.1[length(x[x < x.mean])] 

#Calculation of the mode of the RND function (using the statistical properties of the density functions)
x.mode <- x[i.mode <- which.max(y.1)]
y.mode <- y.1[i.mode]

#Calculation of the median of the RND function (using the statistical properties of the density functions)
y.cs <- cumsum(y.1)                  
x.med <- x[i.med <- length(y.cs[2*y.cs <= y.cs[n]])] 
y.med <- y.1[i.med]  

#Calculation of the IQR of the RND function (using the statistical properties of the density functions)
x.fq <- x[i.fq <- length(y.cs[2*y.cs <= 0.5*y.cs[n]])] 
x.tq <- x[i.tq <- length(y.cs[2*y.cs <= 1.5*y.cs[n]])] 
IQR= x.tq-x.fq

#MATURITY 2
#Calculation of the mean of the RND function (using the statistical properties of the density functions)
n2 <- length(y.2)                     
df2 <- mean(diff(x))                 
y.unit2 <- sum(y.2) * df2               
df2 <- df2 / y.unit2                       
x.mean2 <- sum(y.2 * x) * df2
y.mean2 <- y.2[length(x[x < x.mean2])]

#Calculation of the mode of the RND function (using the statistical properties of the density functions)
x.mode2 <- x[i.mode2 <- which.max(y.2)]
y.mode2 <- y.2[i.mode2]   

#Calculation of the median of the RND function (using the statistical properties of the density functions)
y.cs2 <- cumsum(y.2)                  
x.med2 <- x[i.med2 <- length(y.cs2[2*y.cs2 <= y.cs2[n2]])] 
y.med2 <- y.2[i.med2] 

#Calculation of the IQR of the RND function (using the statistical properties of the density functions)
x.fq2 <- x[i.fq2 <- length(y.cs2[2*y.cs2 <= 0.5*y.cs2[n2]])] 
x.tq2 <- x[i.tq2 <- length(y.cs2[2*y.cs2 <= 1.5*y.cs2[n2]])] 
IQR2= x.tq2-x.fq2

#Change the number of the variable in order to be able to export the data as CSV
variable06 <- data.frame(Column1 = c(x.mean, x.med, x.mode, o1$sigma.1, o1$sigma.2, o1$sigma.3, IQR, o1$p.1, o1$p.2, 1-o1$p.1-o1$p.2, 
                                     x.mean2, x.med2, x.mode2, o2$sigma.1, o2$sigma.2, o2$sigma.3, IQR2, o2$p.1, o2$p.2, 1- o2$p.1- o2$p.2))



################### EXPORT DATA AS CSV ###############################################################################################
out <- data.frame(Column1 = variable01,
                  Column2 = variable02,
                  Column3 = variable03,
                  Column4 = variable04,
                  Column5 = variable05,
                  Column6 = variable06)
write.csv(out, "/Users/Alan/Desktop/df.csv", row.names = FALSE )

