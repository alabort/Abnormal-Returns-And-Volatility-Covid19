
#activate the libraries that are necessary for the evnt study
library("xts")
library("magrittr")
library("estudy2")

#set the working directory
setwd("C://Users/Alan/Desktop/THESIS/PRACTICAL_PART/DEF_EVENT_STUDY_ON_COMPANIES")

######################################### EVENT STUDY ON AMERICAN COMPANIES ##########################################################

#download data of the S&P 500 from Yahoo Finance and calculate logatithmic returns
rates_indx <- get_prices_from_tickers("^GSPC",
                                      start = as.Date("2019-01-01"),
                                      end = as.Date("2020-04-18"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
  get_rates_from_prices(quote = "Close",
                        multi_day = TRUE,
                        compounding = "continuous")

#select tickers of the companies to download
tickers <- c("AAL", "DAL", "LUV", "UAL", "ALK")

# download the data - apply the market model - perform parametric tests
var1 <- get_prices_from_tickers(tickers,
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
  parametric_tests(event_start = as.Date("2020-01-20"),
                   event_end = as.Date("2020-04-18")) 

#download the data - apply the market model - perform non parametric test
var2 <- get_prices_from_tickers(tickers,
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
  generalized_sign_test(event_start = as.Date("2020-01-20"),
                        event_end = as.Date("2020-04-18"))

#convert in data frame 
df1 <- as.data.frame(var1)
df1 <- as.data.frame(var2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )


######################################### EVENT STUDY ON EUROPEAN COMPANIES ##########################################################

#import data of the STOXX. The data has been downloaded from Investing.com 
#And calculate logatithmic returns
rates_indx<-read.csv("STOXX.csv", header = TRUE, sep = ";", dec = ",") 
rates_indx$Date<-as.character(rates_indx$Date) 
rates_indx$Date<-as.Date(rates_indx$Date,format="%d.%m.%Y") 
rates_indx <- xts(rates_indx[,-1], order.by=rates_indx[,1]) 

rates_indx <- get_rates_from_prices(rates_indx,
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")

#select tickers of the companies to download
tickers <- c("RYA.L", "EZJ.L", "LHA.DE", "AF.PA", "FIA1S.HE")

# download the data - apply the market model - perform parametric tests
var1 <- get_prices_from_tickers(tickers,
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
  parametric_tests(event_start = as.Date("2020-01-20"),
                     event_end = as.Date("2020-04-18")) 

#download the data - apply the market model - perform non parametric test
var2 <- get_prices_from_tickers(tickers,
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
  generalized_sign_test(event_start = as.Date("2020-01-20"),
                        event_end = as.Date("2020-04-18"))


#convert in data frame 
df1 <- as.data.frame(var1)
df1 <- as.data.frame(var2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )

######################################### EVENT STUDY ON CHINESE COMPANIES ##########################################################

#download data of the S&P 500 from Yahoo Finance and calculate logatithmic returns
rates_indx <- get_prices_from_tickers("^SSE180",
                                      start = as.Date("2019-01-01"),
                                      end = as.Date("2020-04-18"),
                                      quote = "Close",
                                      retclass = "zoo") %>%
  get_rates_from_prices(quote = "Close",
                        multi_day = TRUE,
                        compounding = "continuous")

#select tickers of the companies to download
tickers <- c("600115.SS", "600221.SS", "200152.SZ", "601111.SS")

# download the data - apply the market model - perform parametric tests
var1 <- get_prices_from_tickers(tickers,
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
  parametric_tests(event_start = as.Date("2020-01-20"),
                   event_end = as.Date("2020-04-18")) 

#download the data - apply the market model - perform non parametric test
var2 <- get_prices_from_tickers(tickers,
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
  generalized_sign_test(event_start = as.Date("2020-01-20"),
                        event_end = as.Date("2020-04-18"))

#convert in data frame 
df1 <- as.data.frame(var1)
df1 <- as.data.frame(var2)

#export as csv
write.csv(df1, "/Users/Alan/Desktop/table1.csv", row.names = FALSE )
write.csv(df2, "/Users/Alan/Desktop/table2.csv", row.names = FALSE )





  
 