# Global Environments
dataDir <- 'raw_data/'

# load data ----
library(data.table)
data_raw_all <- data.table()
for (fileInd in list.files(dataDir)) {
  subName_tmp <- stringr::str_extract(fileInd, pattern = 'sub..')
  subRun_tmp <- unlist(strsplit(x = fileInd, split = '_'))[2]
  subData_tmp <- readr::read_csv(paste0(dataDir, fileInd), skip = 1)
  subData_tmp <- data.table(subName_tmp, subRun_tmp, subData_tmp)
  subData_colname <- colnames(subData_tmp)
  subData_colname[1] <- "subject"
  subData_colname[2] <- 'run'
  colnames(subData_tmp) <- subData_colname
  data_raw_all <- rbind(data_raw_all, subData_tmp)
}
head(data_raw_all)

# reshape data for behaviorial analysis ----
data_beha <- data_raw_all[Stage == 'Navigate']
head(data_beha, n = 20)
## summarize behavior data
psych::describe(data_beha)
psych::describe(data_beha[, .(StopPos)])
### data distribution
hist(x = data_beha$TargetPos, xlab = 'Target Position', breaks = 3, main = 'Distribution of Tagets\' Position')
hist(x = data_beha$CarSpeed, xlab = 'Car Speed', breaks = 2, main = 'Distribution of Car Speed')
hist(x = data_beha$StopPos, xlab = 'Stop Position', main = 'Distribution of Subjects\' Stop Position')
hist(x = data_beha$RelativeErrorDis, xlab = 'Error Distance', main = 'Distribution of Error Distance')
table(data_beha$Insinght)

# 