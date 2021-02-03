# initial the function of demean 
DeMean    <- function(rawVect) {
  outVect <- rawVect - mean(rawVect)
  outVect <- round(outVect, digits = 3)
  return(outVect)
}
ExtractTC <- function(rawData, stageName, condition, modulator) {
  if (condition == -1) { # condition equal -1 means all condition
    if (modulator==1) {
      outTC <- rawData %>%
        filter(Stage==stageName) %>%
        select(onset, duration) %>%
        mutate(thirdCol=modulator)
    }else{
      outTC <- rawData %>%
        filter(Stage==stageName) %>%
        select(onset, duration, modulator) %>%
        mutate(thirdCol=DeMean(get(modulator))) %>%
        select(onset, duration, thirdCol)
    }
  }else{
    if (modulator==1) {
      outTC <- rawData %>%
        filter(Stage==stageName, Condition==condition) %>%
        select(onset, duration) %>%
        mutate(thirdCol=modulator)
    }else{
      outTC <- rawData %>%
        filter(Stage==stageName, Condition==condition) %>%
        select(onset, duration, modulator) %>%
        mutate(thirdCol=DeMean(get(modulator))) %>%
        select(onset, duration, thirdCol)
    }
  }
  
  return(outTC)
}
ShapeData <- function(dataPath) {
  data_tmp <- readr::read_csv(dataPath, skip = 1)
  data_tmp$StartTime <- data_tmp$StartTime %>%
    str_replace_all(pattern = ':', replacement = ".") %>%
    as.POSIXct(format = '%H.%M.%OS')
  data_tmp$EndTime <- data_tmp$EndTime %>%
    str_replace_all(pattern = ':', replacement = ".") %>%
    as.POSIXct(format = '%H.%M.%OS')
  data_tmp[,'onset'] <- map_dbl(data_tmp$StartTime,
                                   ~difftime(.x, data_tmp$StartTime[1],
                                             units = 'secs')) %>%
                                  round(., digits = 3)
  data_tmp[,'duration'] <- map_dbl(data_tmp$EndTime,
                                      ~difftime(.x, data_tmp$StartTime[1],
                                                units = 'secs')) %>%
                            map2_dbl(., data_tmp$onset, ~.x-.y) %>%
                            round(., digits = 3)
  return(data_tmp)
}
# set environment variables ----
tcDir <- 'TC_extraction'
behaDir <- 'raw_data'
# load data ----
library(tidyverse)
library(data.table)
library(stringr)
for (fileInd in list.files(behaDir)) {
  # obtain the subject name and run index
  subName_tmp <- stringr::str_extract(fileInd, pattern = 'sub..')
  subRun_tmp <- unlist(strsplit(x = fileInd, split = '_'))[2]
  # generate the output directory
  outDir_tmp <- paste0(tcDir, '/', subName_tmp)
  if (!dir.exists(outDir_tmp)) {
    dir.create(outDir_tmp, recursive = T)
  }
  # load the data
  dataPath_tmp <- paste0(behaDir,'/',fileInd)
  subData_tmp <- ShapeData(dataPath_tmp)
  # extraction TC
  ## all stage without modulator
  for (stageInd in unique(subData_tmp$Stage)) {
    TC_tmp <- ExtractTC(subData_tmp, stageInd, condition = -1, 1)
    write.table(TC_tmp, 
                file = paste0(outDir_tmp,'/',subName_tmp,'_'
                              ,subRun_tmp,'_',stageInd,'TC.txt'),
                row.names = F, col.names = F)
  }
  ## navigation with modulator
  for (modInd in c('CarSpeed', "TargetPos")) {
    TC_tmp <- ExtractTC(subData_tmp, 'Navigate', -1, modInd)
    write.table(TC_tmp, 
                file = paste0(outDir_tmp,'/',subName_tmp,'_'
                              ,subRun_tmp,'_','NavigateTC_',modInd,'.txt'),
                row.names = F, col.names = F)
  }
  ## encode with TargetPos
  TC_tmp <- ExtractTC(subData_tmp, 'Encode', -1, "TargetPos")
  write.table(TC_tmp, 
              file = paste0(outDir_tmp,'/',subName_tmp,'_'
                            ,subRun_tmp,'_','EncodeTC_TargetPos.txt'),
              row.names = F, col.names = F)
  ## navigation under conditions
  for (condInd in 0:2) {
    TC_tmp <- ExtractTC(subData_tmp, 'Navigate', condInd, 1)
    write.table(TC_tmp, 
                file = paste0(outDir_tmp,'/',subName_tmp,'_'
                              ,subRun_tmp,'_','NavigateTC_cond',condInd,'.txt'),
                row.names = F, col.names = F)
  }
}