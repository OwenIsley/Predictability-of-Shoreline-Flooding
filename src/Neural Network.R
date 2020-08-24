# Owen Alexander Isley
# Professor Pawlicki
# Lake Ontario Independent Study
# 15 April 2020
# Correlation Plot

library(xlsx)
library(corrplot)
library(readxl)
library(tidyverse)
library(neuralnet)
library(GGally)
library(dplyr)

getwd()

########## Downloading data ##########
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists("./data/repdata-data-StormData.csv")) {
  if (!file.exists("./data/repdata-data-StormData.csv.bz2")) {
    download.file(fileUrl, destfile = "./data/repdata-data-StormData.csv.bz2", method="curl")
  }
  bunzip2("./data/repdata-data-StormData.csv.bz2", "./data/repdata-data-StormData.csv")
}

noaa_full <- read.csv("./data/repdata-data-StormData.csv")
dim(noaa_full)
head(noaa_full)

########## Keeping only the fields pertinent to our analysis  ##########
noaa <- noaa_full %>% select(BGN_DATE, BGN_TIME, COUNTY, COUNTYNAME, STATE, EVTYPE, END_DATE, END_TIME, LENGTH, WIDTH, F, MAG, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, LATITUDE, LONGITUDE)
tail(noaa)

# Filter out only Lake Ontario data
# An approximate latitude and longitude "box" for Lake Ontario was estimated using https://www.latlong.net/ (43 <= lat <= 44.1) (76 <= lon <= 80)
# **NOAA's formatting for latitude: e.g. 43.07 => 4307
# **NOAA's formatting for longitude: longitude values are reported as POSITIVE despite truly being negative coordinates! e.g. -76.47 => 7647 Beware!
ontario <- noaa %>% filter(LATITUDE >= 4300, LATITUDE <= 4410, LONGITUDE <= 8000, LONGITUDE <= 7600)
dim(ontario)
head(ontario)
tail(ontario)

write.xlsx(ontario, "C:/Users/owena/OneDrive/Desktop/Lake Ontario Floods/Source Code and Processed Data Sets/Ontario_Export.xlsx")

# Get prepared (post-processed) data set
ontario_master <- read_excel("Merged Final.xlsx")
dim(ontario_master)
head(ontario_master)

#
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", width=0.5, ylab="Number of Features")
}
data_types(ontario_master)


O <- cor(ontario_master)
O

corrplot(O, method="circle")
corrplot(O, method="number")
ggpairs(ontario_master, title = "Scatterplot Matrix of the Features")

## Creating index variable 

# Read the Data
data = ontario_master

# Random sampling 80/20
samplesize = 0.80 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

## Scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

## Fit neural network 
# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)

# plot neural network
plot(NN)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(NN)

## Prediction using neural network
predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)

plot(datatest$NUMBER_DANGEROUS_FLOODS, predict_testNN, col='blue', pch=16, ylab = "predicted number dangerous floods", xlab = "actual number dangerous floods")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN


# 
# # ## Cross validation of neural network model
# samplesize = 0.90 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# ## Fit neural network 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# # fit neural network
# set.seed(2)
# NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)
# ## Prediction using neural network
# predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
# predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# RMSE.NN
# 
# # ## Cross validation of neural network model
# samplesize = 0.80 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# ## Fit neural network 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# # fit neural network
# set.seed(2)
# NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)
# ## Prediction using neural network
# predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
# predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# RMSE.NN
# 
# # ## Cross validation of neural network model
# samplesize = 0.70 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# ## Fit neural network 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# # fit neural network
# set.seed(2)
# NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)
# ## Prediction using neural network
# predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
# predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# RMSE.NN
# 
# # ## Cross validation of neural network model
# samplesize = 0.60 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# ## Fit neural network 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# # fit neural network
# set.seed(2)
# NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)
# ## Prediction using neural network
# predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
# predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# RMSE.NN
# 
# # ## Cross validation of neural network model
# samplesize = 0.50 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# ## Fit neural network 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# # fit neural network
# set.seed(2)
# NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)
# ## Prediction using neural network
# predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
# predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# RMSE.NN
# 
# # ## Cross validation of neural network model
# samplesize = 0.40 * nrow(data)
# set.seed(80)
# index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# # Create training and test set
# datatrain = data[ index, ]
# datatest = data[ -index, ]
# ## Scale data for neural network
# max = apply(data , 2 , max)
# min = apply(data, 2 , min)
# scaled = as.data.frame(scale(data, center = min, scale = max - min))
# ## Fit neural network 
# # creating training and test set
# trainNN = scaled[index , ]
# testNN = scaled[-index , ]
# # fit neural network
# set.seed(2)
# NN = neuralnet(NUMBER_DANGEROUS_FLOODS ~ MONTH + OVER_BASIN_PRECIP + OVERLAKE_PRECIP + OVERLAND_TEMP + OVERLAKE_TEMP + WATER_SURFACE_TEMP + WATER_LEVEL + NUMBER_PRECIP_STORMS + SUM_F_SCALE + SUM_MAGNITUDES + SUM_FATALITIES_INJURIES + SUM_DAMAGE, trainNN, hidden = 9, linear.output = T)
# ## Prediction using neural network
# predict_testNN = compute(NN, testNN[,c("MONTH", "OVER_BASIN_PRECIP","OVERLAKE_PRECIP","OVERLAND_TEMP","OVERLAKE_TEMP","WATER_SURFACE_TEMP","WATER_LEVEL","NUMBER_PRECIP_STORMS","SUM_F_SCALE","SUM_MAGNITUDES","SUM_FATALITIES_INJURIES","SUM_DAMAGE")])
# predict_testNN = (predict_testNN$net.result * (max(data$NUMBER_DANGEROUS_FLOODS) - min(data$NUMBER_DANGEROUS_FLOODS))) + min(data$NUMBER_DANGEROUS_FLOODS)
# # Calculate Root Mean Square Error (RMSE)
# RMSE.NN = (sum((datatest$NUMBER_DANGEROUS_FLOODS - predict_testNN)^2) / nrow(datatest)) ^ 0.5
# RMSE.NN
