################################################################################################################
# R Script for time series forecast
# Author: 	Prashant Patil
# Date:		02-12-2019
# Execute: Steps to execute. 
# {path to R install}\bin\Rscript.exe  {path to script}\diy_forecast_Script.R
# e.g. C:\temp> "C:\Program Files\R\R-3.5.0\bin\"Rscript.exe C:\EDX_DataScience\diy_forecast_Script.R
# Script reports RMSE comparison for 3 stocks for all methods 
################################################################################################################

#############################################################
# Install required packages
#############################################################
repos_link <- "http://cran.us.r-project.org"

if(!require(tidyverse)) install.packages("tidyverse", repos = repos_link)
if(!require(caret)) install.packages("caret", repos = repos_link)
if(!require(forecast)) install.packages("forecast", repos = repos_link)
if(!require(nnet)) install.packages("nnet", repos = repos_link)
library(tidyverse)
library(caret)
library(forecast)
library(nnet)

### Downloading from Kaggle requires account with them, so download steps are not included with this script. The csv file downloaded from Kaggle is included with submission. 
### Download the all_stocks_2006-01-01_to_2018-01-01.csv from gitHub repo and save it in your local machine. Save this file to local directory and change the value of localDirectory variable for the script to work as expected.
localDirectory <- "C:\\temp\\DIYProject\\"
fName <- "all_stocks_2006-01-01_to_2018-01-01.csv"
## Load data in allStocks dataframe. Stock specific data will be used as needed
allStocks <- read.csv(file.path(localDirectory,fName))

### Select data only for AAPL stock
aaplData <- allStocks %>% filter(Name=="AAPL")
head(aaplData)

trainIndex <- nrow(aaplData)-300;
testIndex <- trainIndex+1;
trainData <- aaplData[1:trainIndex,]
testData <- aaplData[testIndex:nrow(aaplData),]

#### 1. Mean Forecast: 
fit_meanf <- meanf(trainData[,5], h = 300)
pred_meanf <- as.numeric(fit_meanf$mean)
RMSE(testData$Close,pred_meanf)


#### 2. Naive Prediction: 
fit_naive <- naive(trainData[,5], h = 300)
pred_naive <- as.numeric(fit_naive$mean)
RMSE(testData$Close,pred_naive)

#### 3. Random Walk Forest - Drift Method: 
fit_rwf <- rwf(trainData[,5], h = 300, drift = TRUE)
pred_rwf <- as.numeric(fit_rwf$mean)
RMSE(testData$Close,pred_rwf)

#### 4. Time series Linear Regression: 
fit_lm <- tslm(as.ts(trainData[, 5]) ~ trend)
pred_lm <- as.numeric(forecast(fit_lm, h = 300)$mean)
RMSE(testData$Close,pred_lm)

#### 5. Exponential Smoothing State Space model: 
fit_ets <- trainData[,5] %>% ets(model="MMZ")
pred_ets <- as.numeric(forecast(fit_ets, h = 300)$mean)
RMSE(testData$Close,pred_ets)

#### 6. Neutral Networks: 
fit_NeuNet <- nnetar(as.ts(trainData[,5]))
pred_nn <- as.numeric(forecast(fit_NeuNet, h = 300)$mean)
RMSE(testData$Close,pred_nn)

### Save Current Results to a dataframe
rmse_results <- data_frame(Method = "Mean Forecast", RMSE_AAPL = RMSE(testData$Close,pred_meanf))

rmse_results <- bind_rows(rmse_results,data_frame(Method="Naive Prediction",  
                        RMSE_AAPL = RMSE(testData$Close,pred_naive)))

rmse_results <- bind_rows(rmse_results,data_frame(Method="Random Walk Forest",  
                        RMSE_AAPL = RMSE(testData$Close,pred_rwf)))									 

rmse_results <- bind_rows(rmse_results,data_frame(Method="Time series Linear Regression",  
                        RMSE_AAPL = RMSE(testData$Close,pred_lm)))	

rmse_results <- bind_rows(rmse_results,data_frame(Method="Exponential Smoothing",  
                        RMSE_AAPL = RMSE(testData$Close,pred_ets)))	

rmse_results <- bind_rows(rmse_results,data_frame(Method="Neutral Network",  
                        RMSE_AAPL = RMSE(testData$Close,pred_nn)))

## Stock 2 - Walmart data load and predict
wmtData <- allStocks %>% filter(Name=="WMT")
trainIndex <- nrow(wmtData)-300;
testIndex <- trainIndex+1;
trainData <- wmtData[1:trainIndex,]
testData <- wmtData[testIndex:nrow(wmtData),]

fit_meanf <- meanf(trainData[,5], h = 300)
pred_meanf <- as.numeric(fit_meanf$mean)

fit_naive <- naive(trainData[,5], h = 300)
pred_naive <- as.numeric(fit_naive$mean)

fit_rwf <- rwf(trainData[,5], h = 300, drift = TRUE)
pred_rwf <- as.numeric(fit_rwf$mean)

fit_lm <- tslm(as.ts(trainData[, 5]) ~ trend)
pred_lm <- as.numeric(forecast(fit_lm, h = 300)$mean)

fit_ets <- trainData[,5] %>% ets(model="MMZ")
pred_ets <- as.numeric(forecast(fit_ets, h = 300)$mean)

fit_NeuNet <- nnetar(as.ts(trainData[,5]))
pred_nn <- as.numeric(forecast(fit_NeuNet, h = 300)$mean)

### Save RMSEs to local DF 								 
rmse_WMT <- data_frame(RMSE_WMT = RMSE(testData$Close,pred_meanf))

rmse_WMT <- bind_rows(rmse_WMT,
                          data_frame(RMSE_WMT = RMSE(testData$Close,pred_naive)))

rmse_WMT <- bind_rows(rmse_WMT,
                          data_frame(RMSE_WMT = RMSE(testData$Close,pred_rwf)))
						  
rmse_WMT <- bind_rows(rmse_WMT,
                          data_frame(RMSE_WMT = RMSE(testData$Close,pred_lm))) 

rmse_WMT <- bind_rows(rmse_WMT,
                          data_frame(RMSE_WMT = RMSE(testData$Close,pred_ets))) 

rmse_WMT <- bind_rows(rmse_WMT,
                          data_frame(RMSE_WMT = RMSE(testData$Close,pred_nn))) 

## Stock 3 - Disney data load and predict

disData <- allStocks %>% filter(Name=="DIS")
trainIndex <- nrow(disData)-300;
testIndex <- trainIndex+1;
trainData <- disData[1:trainIndex,]
testData <- disData[testIndex:nrow(disData),]

fit_meanf <- meanf(trainData[,5], h = 300)
pred_meanf <- as.numeric(fit_meanf$mean)

fit_naive <- naive(trainData[,5], h = 300)
pred_naive <- as.numeric(fit_naive$mean)

fit_rwf <- rwf(trainData[,5], h = 300, drift = TRUE)
pred_rwf <- as.numeric(fit_rwf$mean)

fit_lm <- tslm(as.ts(trainData[, 5]) ~ trend)
pred_lm <- as.numeric(forecast(fit_lm, h = 300)$mean)

fit_ets <- trainData[,5] %>% ets(model="MMZ")
pred_ets <- as.numeric(forecast(fit_ets, h = 300)$mean)

fit_NeuNet <- nnetar(as.ts(trainData[,5]))
pred_nn <- as.numeric(forecast(fit_NeuNet, h = 300)$mean)

### Save RMSEs to local DF 
rmse_DIS <- data_frame(RMSE_DIS = RMSE(testData$Close,pred_meanf))

rmse_DIS <- bind_rows(rmse_DIS,
                          data_frame(RMSE_DIS = RMSE(testData$Close,pred_naive)))

rmse_DIS <- bind_rows(rmse_DIS,
                          data_frame(RMSE_DIS = RMSE(testData$Close,pred_rwf)))
						  
rmse_DIS <- bind_rows(rmse_DIS,
                          data_frame(RMSE_DIS = RMSE(testData$Close,pred_lm))) 

rmse_DIS <- bind_rows(rmse_DIS,
                          data_frame(RMSE_DIS = RMSE(testData$Close,pred_ets)))
						  
rmse_DIS <- bind_rows(rmse_DIS,
                          data_frame(RMSE_DIS = RMSE(testData$Close,pred_nn))) 

rmse_results$RMSE_DIS <- rmse_DIS$RMSE_DIS
rmse_results$RMSE_WMT <- rmse_WMT$RMSE_WMT

### Display RMSEs for all 3 stocks
rmse_results
