#activate the libraries that are necessary for the evnt study
library("xts")
library("magrittr")
library("estudy2")

#set the working directory
setwd("C://Users/Alan/Desktop/THESIS/PRACTICAL_PART/DEF_EVENT_STUDY_ON_COMPANIES")

########################### EVENT STUDY - CAR OF CHINESE AND AMERICAN COMPANIES ###################################################################
rm(list=ls()) 
#download data of the S&P 500 from Yahoo Finance and calculate logatithmic returns 
#for the event study on other companies change the ticker
rates_indx <- get_prices_from_tickers("^GSPC",
                                      start = as.Date("2019-01-01"),
                                      end = as.Date("2020-04-18"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
  get_rates_from_prices(quote = "Close",
                        multi_day = TRUE,
                        compounding = "continuous")

#select tickers of the companies to download (then change with also other tikers)
tickers <- c("ALK", "ALK")

# download the data - apply the market model - perform parametric tests 
# for a different time frame of event window change the following function in the indicated points
h1 <- get_prices_from_tickers(tickers,
                              start = as.Date("2019-01-01"),
                              end = as.Date("2020-04-18"),
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
  car_brown_warner_1985(car_start = as.Date("2020-03-23"), #change date here
                        car_end = as.Date("2020-04-17")) #change date here

#download the data - apply the market model - perform non parametric test
# for a different time frame of event window change the following function in the indicated points
h2 <- get_prices_from_tickers(tickers,
                              start = as.Date("2019-01-01"),
                              end = as.Date("2020-04-18"),
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
df1 <- as.data.frame(h2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )


########################### EVENT STUDY - CAR OF EUROPEAN COMPANIES ###################################################################
rm(list=ls()) 
#import the data of the STOXX downloaded from Investing and calculate logatithmic returns 
#for the event study on other companies change the ticker
rates_indx<-read.csv("STOXX.csv", header = TRUE, sep = ";", dec = ",") 
rates_indx$Date<-as.character(rates_indx$Date) 
rates_indx$Date<-as.Date(rates_indx$Date,format="%d.%m.%Y") 
rates_indx <- xts(rates_indx[,-1], order.by=rates_indx[,1]) 

rates_indx <- get_rates_from_prices(rates_indx,
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")

#select tickers of the companies to download (then change with also other tikers)
tickers <- c("RYA.L", "RYA.L")

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
  car_brown_warner_1985(car_start = as.Date("2020-01-20"),#change date here
                        car_end = as.Date("2020-04-18"))#change date here

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
  car_rank_test(car_start = as.Date("2020-01-20"),#change date here
                   car_end = as.Date("2020-04-18"))#change date here

#convert in data frame 
df1 <- as.data.frame(h1)
df1 <- as.data.frame(h2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )


  
 