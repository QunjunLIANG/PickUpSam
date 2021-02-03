# initial the function of demean 
DeMean <- function(rawVect) {
  outVect <- rawVect - mean(rawVect)
  return(outVect)
}
ExtractTC <- function(rawData, stageName, modulator) {
  if (modulator==1) {
    outTC <- rawData %>%
      filter(Stage==stageName) %>%
      select(onset, duration) %>%
      mutate(thirdCol=modulator)
  }else{
    outTC <- rawData %>%
      filter(Stage==stageName) %>%
      select(onset, duration, modulator) %>%
      mutate(thirdCol=DeMean(modulator)) %>%
      select(onset, duration, thirdCol)
  }
  
}
# load data
library(tidyverse)
library(data.table)
library(stringr)
data.raw <- data.table(readr::read_csv('raw_data/sub01_1_playerMoveRecord.csv',
                                         skip = 1))
data.raw$StartTime <- data.raw$StartTime %>%
  str_replace_all(pattern = ':', replacement = ".") %>%
  as.POSIXct(format = '%H.%M.%OS')
data.raw$EndTime <- data.raw$EndTime %>%
  str_replace_all(pattern = ':', replacement = ".") %>%
  as.POSIXct(format = '%H.%M.%OS')
data.raw[,'onset'] <- map_dbl(data.raw$StartTime,
                              ~difftime(.x, data.raw$StartTime[1],
                                        units = 'secs'))
data.raw[,'duration'] <- map_dbl(data.raw$EndTime,
                                 ~difftime(.x, data.raw$StartTime[1],
                                           units = 'secs')) %>%
                              map2_dbl(., data.raw$onset, ~.x-.y)

intervalTC <- data.raw %>%
  filter(Stage=="Interval") %>%
  select(onset, duration) %>%
  mutate(thirdCol=1)
encodeTC <- data.raw %>%
  filter(Stage=="Encode") %>%
  select(onset, duration) %>%
  mutate(thirdCol=1)
navigateTC <- data.raw %>%
  filter(Stage=="Navigate") %>%
  select(onset, duration) %>%
  mutate(thirdCol=1)
navigateTC_speed <- data.raw %>%
  filter(Stage=="Interval") %>%
  select(onset, duration, CarSpeed)
navigateTC_distance <- data.raw %>%
  filter(Stage=="Interval") %>%
  select(onset, duration, TargetPos)
navigate_PI_TC <- data.raw %>%
  filter(Stage=="Navigate", Condition==0) %>%
  select(onset, duration) %>%
  mutate(thirdCol=1)
navigate_LP1_TC <- data.raw %>%
  filter(Stage=="Navigate", Condition==1) %>%
  select(onset, duration) %>%
  mutate(thirdCol=1)
navigate_LP2_TC <- data.raw %>%
  filter(Stage=="Navigate", Condition==2) %>%
  select(onset, duration) %>%
  mutate(thirdCol=1)
navigate_PI_TC_speed <- data.raw %>%
  filter(Stage=="Navigate", Condition==0) %>%
  select(onset, duration, CarSpeed)
navigate_LP1_TC_speed <- data.raw %>%
  filter(Stage=="Navigate", Condition==1) %>%
  select(onset, duration, CarSpeed)
navigate_LP2_TC_speed <- data.raw %>%
  filter(Stage=="Navigate", Condition==2) %>%
  select(onset, duration, CarSpeed)
navigate_PI_TC_dis <- data.raw %>%
  filter(Stage=="Navigate", Condition==0) %>%
  select(onset, duration, TargetPos)
navigate_LP1_TC_dis <- data.raw %>%
  filter(Stage=="Navigate", Condition==1) %>%
  select(onset, duration, TargetPos)
navigate_LP2_TC_dis <- data.raw %>%
  filter(Stage=="Navigate", Condition==2) %>%
  select(onset, duration, TargetPos) %>%
  mutate(thirdCol=DeMean(TargetPos)) %>%
  select(onset, duration, thirdCol)