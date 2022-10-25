
#activate the libraries that are necessary for the evnt study
library("xts")
library("magrittr")
library("estudy2")

#set the working directory
setwd("C://Users/Alan/Desktop/THESIS/PRACTICAL_PART/DEF_EVENT_STUDY_ON_INDICES")


########################### CAR for INDICES ON YAHOO ###################################################################
#this process has been used for all the indices except Classic All Share, FTSE MIB, FTSE All Share Italia
#they were not available on Yahoo finance

rm(list=ls())
#import the data of the FTSE ALL WORLD INDEX downloaded from Investing and calculate logatithmic returns 
#for the event study on other indices change the ticker

rates_indx<-read.csv("FTSEALLWORLD.csv", header = TRUE, sep = ";", dec = ",") 
rates_indx$Date<-as.character(rates_indx$Date) 
rates_indx$Date<-as.Date(rates_indx$Date,format="%d.%m.%Y") 
rates_indx <- xts(rates_indx[,-1], order.by=rates_indx[,1]) 

rates_indx <- get_rates_from_prices(rates_indx,
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")

#select tickers of the companies to download (then change with also other tikers)
tickers <- c("^IBEX", "^IBEX")

# download the data - apply the market model - perform parametric tests 
# for a different time frame of event window change the following function in the indicated points
h1 <- get_prices_from_tickers(tickers,
                                             start = as.Date("2019-01-01"),
                                             end = as.Date("2020-05-22"),
                                             quote = "Close",
                                             retclass = "zoo") %>%
  get_rates_from_prices(quote = "Close",
                        multi_day = TRUE,
                        compounding = "continuous") %>%
  apply_market_model(regressor = rates_indx,
                     same_regressor_for_all = TRUE,
                     market_model = "sim",
                     estimation_method = "ols",
                     estimation_start = as.Date("2019-01-01"),
                     estimation_end = as.Date("2020-01-19")) %>%
  car_brown_warner_1985(car_start = as.Date("2020-03-23"),#change date here
                        car_end = as.Date("2020-04-17"))#change date here

#download the data - apply the market model - perform non parametric test
# for a different time frame of event window change the following function in the indicated points
h2 <- get_prices_from_tickers(tickers,
                              start = as.Date("2019-01-01"),
                              end = as.Date("2020-05-22"),
                              quote = "Close",
                              retclass = "zoo") %>%
  get_rates_from_prices(quote = "Close",
                        multi_day = TRUE,
                        compounding = "continuous") %>%
  apply_market_model(regressor = rates_indx,
                     same_regressor_for_all = TRUE,
                     market_model = "sim",
                     estimation_method = "ols",
                     estimation_start = as.Date("2019-01-01"),
                     estimation_end = as.Date("2020-01-19")) %>%
  car_rank_test(car_start = as.Date("2020-03-23"),#change date here
                   car_end = as.Date("2020-04-17"))#change date here

#convert in data frame 
df1 <- as.data.frame(h1)
df2 <- as.data.frame(h2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )

########################### CAR for INDICES NOT ON YAHOO ###################################################################
#this process has been used for the indices  Classic All Share, FTSE MIB, FTSE All Share Italia
#they were not available on Yahoo finance

rm(list=ls())

#import the data of the FTSE ALL WORLD INDEX downloaded from Investing and calculate logatithmic returns 
#for the event study on other indices change the name of the file
rates_indx<-read.csv("FTSEALLWORLD.csv", header = TRUE, sep = ";", dec = ",") 
rates_indx$Date<-as.character(rates_indx$Date) 
rates_indx$Date<-as.Date(rates_indx$Date,format="%d.%m.%Y") 
rates_indx <- xts(rates_indx[,-1], order.by=rates_indx[,1]) 

rates_indx <- get_rates_from_prices(rates_indx,
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")

#import the data of FTSEMIBALLSHARE 
#change the name of the file for other indices
import1<-read.csv("SMSI.csv", header = TRUE, sep = ";", dec = ".") 
import1$Date<-as.character(import1$ï..Date) 
import1$Date<-as.Date(import1$Date,format="%d.%m.%Y") 
import1 <- xts(import1[,2], order.by=import1[,3]) 
import1 <- cbind(import1,import1)

# apply the market model - perform parametric tests 
# for a different time frame of event window change the following function in the indicated points
h1 <- get_rates_from_prices(import1,
                            quote = "Close",
                            multi_day = TRUE,
                            compounding = "continuous") %>%
  apply_market_model(regressor = rates_indx,
                     same_regressor_for_all = TRUE,
                     market_model = "sim",
                     estimation_method = "ols",
                     estimation_start = as.Date("2019-01-01"),
                     estimation_end = as.Date("2020-01-19")) %>%
  car_brown_warner_1985(car_start = as.Date("2020-03-23"),
                        car_end = as.Date("2020-04-17"))

# apply the market model - perform non parametric test
# for a different time frame of event window change the following function in the indicated points
h2 <- get_rates_from_prices(import1,
                        quote = "Close",
                        multi_day = TRUE,
                        compounding = "continuous") %>%
  apply_market_model(regressor = rates_indx,
                     same_regressor_for_all = TRUE,
                     market_model = "sim",
                     estimation_method = "ols",
                     estimation_start = as.Date("2019-01-01"),
                     estimation_end = as.Date("2020-01-19")) %>%
  car_rank_test(car_start = as.Date("2020-03-23"),
                car_end = as.Date("2020-04-17"))

#convert in data frame 
df1 <- as.data.frame(h1)
df2 <- as.data.frame(h2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )






  
 