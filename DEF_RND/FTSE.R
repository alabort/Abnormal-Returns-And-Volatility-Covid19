rm(list=ls()) 

########################################### MAIN PART ##########################################################
# import necessary libraries 
library("RND")
library("tidyverse")
library("moments")
library("xts")
require(lubridate)

#change the working directory
setwd("/Users/Alan/Desktop/THESIS/PRACTICAL_PART/DEF_RND_INDICES")

#select the date of the desired RND (it is necessary to change the date also one more time later in row 67)
Selected_Date <- "25/03/2020"
Selected_Date2 <- "25/03/20"

#import the prices of the underlying and filter the price of the wanted day
Price <-  read.csv("UK_PRICE.csv", header = TRUE, sep = ";", dec=".")
Exact2<- Price %>% filter(Price$ï..Timestamp == Selected_Date) 
s0 = as.numeric(Exact2$Trade.Close)

#computation of the time to maturity for for both maturities 
te1 = as.numeric((as.Date("2020-09-18")-as.Date(Selected_Date, format = "%d/%m/%y"))/365) 
te2 = as.numeric((as.Date("2020-12-18")-as.Date(Selected_Date, format = "%d/%m/%y"))/365)

#import the LIBOR rates for british pound and filter the rates for the selected date 
#the 1M, 3M, 6M and 12M rates have been downloaded from FRED
Rates <- read.csv("uk.csv", header = TRUE, sep = ";", dec=".")
Exact <- Rates %>% filter(Rates$ï..Date == Selected_Date)

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
termStruct <- func1(ad, 3)
for(i in (1:numRates)) termStruct[marketDates[i]] <- marketData.xts[marketDates[i]]
linear.interpolate <- na.approx(termStruct)
linear.interpolate <- as.data.frame(linear.interpolate)

#linear interpolation of rates
r1 <- as.numeric(as.vector(linear.interpolate["2020-09-18", 1]))
r2 <- as.numeric(as.vector(linear.interpolate["2020-12-18", 1]))

#Import option data form the CSV file
#The following data has been downloaded in accordance with the library from the database Eikon
Main_table <- read.csv("FTSE.csv", header = TRUE, sep = ",", dec=".")#changeee
Date_table <- data.frame(Main_table$X25.03.2020, Main_table$Strike, Main_table$Expiry.Date, Main_table$X.NAME.)
#remember to change the date in the following line, the format should be X.dd.mm.yyyy
Date_table2 <- Date_table[complete.cases(Date_table), ]
Date_table_put <- Date_table2 %>% filter(Main_table.X.NAME.== "PUT ")
Date_table_call <- Date_table2 %>% filter(Main_table.X.NAME.== "CALL")
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


#the yield of FTSE has been downloaded from this website https://siblisresearch.com/data/ftse-all-total-return-dividend/
y1 =0.048
y2 =0.048

#calculation of forward price
f1=s0*exp(r1*te1)
f2=s0*exp(r2*te2)

#filter options according to their moneyness
Call_mat1 <- cbind(Call_mat1, xfcs = as.numeric(as.vector(Call_mat1$Strike))/(f1))
Call_mat1 <- Call_mat1 %>% filter(xfcs> 0.5 & xfcs <1.5)
Call_mat2 <- cbind(Call_mat2, xfcs = as.numeric(as.vector(Call_mat2$Strike))/(f2))
Call_mat2 <- Call_mat2 %>% filter(xfcs> 0.5 & xfcs <1.5)
Put_mat1 <- cbind(Put_mat1, xfcs = as.numeric(as.vector(Put_mat1$Strike))/(f1))
Put_mat1 <- Put_mat1 %>% filter(xfcs> 0.5 & xfcs <1.5)
Put_mat2 <- cbind(Put_mat2, xfcs = as.numeric(as.vector(Put_mat2$Strike))/(f2))
Put_mat2 <- Put_mat2 %>% filter(xfcs> 0.5 & xfcs <1.5)


#estract the parameters that will be useful to plot RNDs
MLN1 <- extract.mln.density(r = r1, y = y1, te = te1, s0 = s0, market.calls = as.numeric(as.vector(Call_mat1$Price)), call.strikes = as.numeric(as.vector(Call_mat1$Strike)), 
                            call.weights = 1, market.puts = as.numeric(as.vector(Put_mat1$Price)), put.strikes =  as.numeric(as.vector(Put_mat1$Strike)), 
                            put.weights = 1, lambda = 1, hessian.flag = FALSE)

MLN2 <- extract.mln.density(r = r2, y = y2, te = te2, s0 = s0, market.calls = as.numeric(as.vector(Call_mat2$Price)), call.strikes = as.numeric(as.vector(Call_mat2$Strike)), 
                            call.weights = 1, market.puts = as.numeric(as.vector(Put_mat2$Price)), put.strikes =  as.numeric(as.vector(Put_mat2$Strike)), 
                            put.weights = 1, lambda = 1, hessian.flag = F)


#insert parameters extracted with the previous function in multiple variables - maturity 1
mln.alpha.1 = MLN1$alpha.1
mln.meanlog.1 = MLN1$meanlog.1
mln.meanlog.2 = MLN1$meanlog.2
mln.sdlog.1 = MLN1$sdlog.1
mln.sdlog.2 = MLN1$sdlog.2

#insert parameters extracted with the previous function in multiple variables - maturity 2
mln2.alpha.1 = MLN2$alpha.1
mln2.meanlog.1 = MLN2$meanlog.1
mln2.meanlog.2 = MLN2$meanlog.2
mln2.sdlog.1 = MLN2$sdlog.1
mln2.sdlog.2 = MLN2$sdlog.2

#calculate density for each price - maturity 1
k = 0:10000
dx = dmln(x = k, alpha.1 = mln.alpha.1, meanlog.1 = mln.meanlog.1,
          meanlog.2 = mln.meanlog.2,
          sdlog.1 = mln.sdlog.1, sdlog.2 = mln.sdlog.2)

#calculate density for each price - maturity 1
dx2 = dmln(x = k, alpha.1 = mln2.alpha.1, meanlog.1 = mln2.meanlog.1,
          meanlog.2 = mln2.meanlog.2,
          sdlog.1 = mln2.sdlog.1, sdlog.2 = mln2.sdlog.2)

#plot RND
matplot(k,cbind(dx, dx2), type="l", col=c("black", "blue"), xlab="Prices", ylab="Density", 
        lwd=c(2,2,2,2,2), lty = c(1,1,1,1,1), cex.axis = 1.25, cex.lab = 1.25, main= Selected_Date)

legend("topleft", legend=c("21-08-2020", "18-12-2020"), col=c("black","blue","red"), 
       lwd = c(2,2,2,2,2), lty = c(1,1,1,1,1), bty="n", cex=1.25)

########################### SUMMARY STATISTICS ############################################################################

#MATURITY 1
#Calculation of the mean of the RND function (using the statistical properties of the density functions)
n <- length(dx)                     
df <- mean(diff(k))                 
y.unit <- sum(dx) * df               
df <- df / y.unit                       
x.mean <- sum(dx * k) * df
y.mean <- dx[length(k[k < x.mean])] 

#Calculation of the mode of the RND function (using the statistical properties of the density functions)
x.mode <- k[i.mode <- which.max(dx)]
y.mode <- dx[i.mode]

#Calculation of the median of the RND function (using the statistical properties of the density functions)
y.cs <- cumsum(dx)                  
x.med <- k[i.med <- length(y.cs[2*y.cs <= y.cs[n]])] 
y.med <- dx[i.med]

#Calculation of the IQR of the RND function (using the statistical properties of the density functions)
x.fq <- k[i.fq <- length(y.cs[2*y.cs <= 0.5*y.cs[n]])] 
x.tq <- k[i.tq <- length(y.cs[2*y.cs <= 1.5*y.cs[n]])] 
IQR= x.tq-x.fq

#MATURITY 2
#Calculation of the mean of the RND function (using the statistical properties of the density functions)
n2 <- length(dx2)                     
df2 <- mean(diff(k))                 
y.unit2 <- sum(dx2) * df2               
df2 <- df2 / y.unit2                       
x.mean2 <- sum(dx2 * k) * df2
y.mean2 <- dx2[length(k[k < x.mean2])] 

#Calculation of the mode of the RND function (using the statistical properties of the density functions)
x.mode2 <- k[i.mode2 <- which.max(dx2)]
y.mode2 <- dx2[i.mode2]    

#Calculation of the median of the RND function (using the statistical properties of the density functions)
y.cs2 <- cumsum(dx2)                  
x.med2 <- k[i.med2 <- length(y.cs2[2*y.cs2 <= y.cs2[n2]])] 
y.med2 <- dx2[i.med2]

#Calculation of the IQR of the RND function (using the statistical properties of the density functions)
x.fq2 <- k[i.fq2 <- length(y.cs2[2*y.cs2 <= 0.5*y.cs2[n2]])] 
x.tq2 <- k[i.tq2 <- length(y.cs2[2*y.cs2 <= 1.5*y.cs2[n2]])] 
IQR2= x.tq2-x.fq2

#Change the number of the variable in order to be able to export the data as CSV
variable05 <- data.frame(Column1 = c(x.mean, x.med, x.mode, mln.sdlog.1, mln.sdlog.2, IQR, mln.alpha.1, 1- mln.alpha.1, x.mean2, x.med2, x.mode2, mln2.sdlog.1, mln.sdlog.2, IQR2, mln2.alpha.1, 1- mln2.alpha.1))



################### EXPORT DATA AS CSV ###############################################################################################
out <- data.frame(Column1 = variable01,
                  Column2 = variable02,
                  Column3 = variable03,
                  Column4 = variable04,
                  Column5 = variable05,
                  Column6 = variable06)
write.csv(out, "/Users/Alan/Desktop/df.csv", row.names = FALSE )

