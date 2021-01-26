# Global Environments
dataDir <- 'raw_data/'
scaleDataFile <- 'PickUpSam_postScanning_Questionnaires.csv'
.# load data ----
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

# reshape data for behavioral analysis ----
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
data_beha[, "RealError"] <- abs(data_beha$RelativeErrorDis)

# reshape data of questionnaires ----
library(data.table)
library(tidyverse)
data_ques <- data.table(readr::read_csv(scaleDataFile, col_names = T))
## reverse the score of SOSBS
data_ques$SOSBS2 <-  8 - data_ques$SOSBS2
data_ques$SOSBS6 <-  8 - data_ques$SOSBS6
data_ques$SOSBS8 <-  8 - data_ques$SOSBS8
data_ques$SOSBS10 <-  8 - data_ques$SOSBS10
data_ques$SOSBS11 <-  8 - data_ques$SOSBS11
data_ques$SOSBS13 <-  8 - data_ques$SOSBS13
data_ques$SOSBS15 <-  8 - data_ques$SOSBS15
## calculate the total score of SOSBS and GRiPS
data_ques %<>% 
  select(starts_with('SOS')) %>%
  mutate(data_ques, SOSBS_total=apply(., 1, sum))
data_ques %<>% 
  select(starts_with('GRi')) %>%
  mutate(data_ques, GRiPS_total=apply(., 1, sum))
## combine behavoral and questionnarie data
data_total <- data_ques %>%
  select(subject, gender, SOSBS_total, GRiPS_total) %>%
  left_join(data_beha, ., by='subject')
## actually, the target position and carspeed is divided into conditions
data_total[, Speed := ifelse(CarSpeed>7, 'fast', 'slow')]
data_total[, Destination := ifelse(TargetPos>50, "far", "near")]

##############################################
#
# Universal Analysis
#
##############################################

# the correlation of SOSBS and Error Distance ----
ggstatsplot::ggscatterstats(
  data = data_total %>%
    group_by(subject) %>%
    summarise_each(funs(mean)),
  x = SOSBS_total,
  y = RealError
)

# the correlation of GRiPs and Error Distance ----
ggstatsplot::ggscatterstats(
  data = data_total %>%
    group_by(subject) %>%
    summarise_each(funs(mean)),
  x = GRiPS_total,
  y = RealError
)
ggstatsplot::ggscatterstats(
  data = data_total %>%
    group_by(subject) %>%
    summarise_each(funs(mean)),
  x = GRiPS_total,
  y = RelativeErrorDis
)

# analyze the error distance in different runs ----
library(tidyverse)
library(ggplot2)
ggstatsplot::ggbetweenstats(
  data = data_beha %>%
    group_by(subject, run) %>%
    summarise_each(funs(mean)),
  x = run,
  y = RealError
)
ggstatsplot::ggwithinstats(
  data = data_beha %>%
    group_by(subject, run) %>%
    summarise_each(funs(mean)),
  x = run,
  y = RealError
)

# analyze the error distance in different genders ----
library(tidyverse)
library(ggplot2)
ggstatsplot::ggbetweenstats(
  data = data_total,
  x = gender,
  y = RealError
)
## t test
data_beha %>%
  group_by(subject) %>%
  summarise_each(funs(mean)) %>%
  mutate(sex=data_ques$gender) %>%
  t.test(RealError ~ sex, .)

# analyze the error distance in different conditions ---
library(tidyverse)
library(ggplot2)
ggstatsplot::ggbetweenstats(
  data = data_beha %>%
    group_by(subject, Condition) %>%
    summarise_each(funs(mean)),
  x = Condition,
  y = RealError
)

# the correlation of target position and Error Distance in continuous type  ----
library(tidyverse)
library(ggplot2)
ggstatsplot::ggscatterstats(
  data = data_beha,
  x = TargetPos,
  y = RealError
)
## T test to exam the statistical significant of correlation
data_beha %>%
  select(subject, TargetPos, RealError) %>%
  group_by(subject) %>%
  summarise(corr_data=cor(TargetPos, RealError)) %>%
  .[,2] %>%
  t.test()

# the correlation of car speed and Error distance in continuous type ----
library(tidyverse)
library(ggplot2)
ggstatsplot::ggscatterstats(
  data = data_beha,
  x = CarSpeed,
  y = RealError
)
## T test to exam the statistical significant of correlation
data_beha %>%
  select(subject, CarSpeed, RealError) %>%
  group_by(subject) %>%
  summarise(corr_data=cor(CarSpeed, RealError)) %>%
  .[,2] %>%
  t.test()

# the difference of Error Distance in various Speed conditions ----
library(tidyverse)
library(ggplot2)
ggstatsplot::ggbetweenstats(
  data = data_total %>%
    group_by(subject, Speed) %>%
    summarise_each(funs(mean)),
  x = Speed,
  y = RealError
)

# the difference of Error Distance in various Target conditions ----
library(tidyverse)
library(ggplot2)
ggstatsplot::ggbetweenstats(
  data = data_total %>%
    group_by(subject, Destination) %>%
    summarise_each(funs(mean)),
  x = Destination,
  y = RealError
)

###############################################
#
# Linear Mixed-effect Model - in Speed and Destination
#
###############################################
library(lmerTest)
library(bruceR)
# build the alternative models ----
model.alt1 <- lmer(
  data = data_total,
  formula = RealError ~ Condition + Speed + Destination +
    (1 + Speed|subject) + (1|run) + (1|Trial),
  control = lmerControl('bobyqa')
)
model.alt2 <- lmer(
  data = data_total,
  formula = RealError ~ Condition + Speed + Destination +
    (1 + Speed|subject) + (1|run),
  control = lmerControl('bobyqa')
)
model.alt3 <- lmer(
  data = data_total,
  formula = RealError ~ Condition + Speed + Destination +
    (1 + Speed|subject),
  control = lmerControl('bobyqa')
)
model.alt4 <- lmer(
  data = data_total,
  formula = RealError ~ Condition*Speed*Destination +
    (1 + Speed|subject),
  control = lmerControl('bobyqa')
)

## model comparison 
anova(model.alt1, model.alt2, model.alt3, model.alt4)
bruceR::model_summary(
  model_list = list(model.alt1, model.alt2, model.alt3, model.alt4))
###############################################
#
# Linear Mixed-effect Model old - in CarSpeed and TargetPos
#
###############################################

library(lmerTest)
library(bruceR)
# model setting ----
Model.alt1 <- lmer(data = data_total,
                   formula = RealError ~ TargetPos*CarSpeed*Condition + 
                     (1 | subject) + (1|run),
                   control = lmerControl('bobyqa'))
Model.alt2 <- lmer(data = data_total,
                   formula = RealError ~ TargetPos*CarSpeed*Condition +
                     (1|subject),
                   control = lmerControl('bobyqa'))
Model.alt3 <- lmer(data = data_total,
                   formula = RealError ~ TargetPos + CarSpeed + Condition +
                     (1 | subject) + (1 | run) + (1 | Trial),
                   control = lmerControl('bobyqa'))
Model.alt4 <- lmer(data = data_total,
                   formula = RealError ~ TargetPos*CarSpeed*Condition
                   + (1 | subject) + (1 |run) + (1|Trial),
                   control = lmerControl('bobyqa'))

## model comparison
bruceR::model_summary(model_list = list(Model.alt1, Model.alt2,
                                        Model.alt3, Model.alt4))
anova(Model.alt1, Model.alt2,
      Model.alt3, Model.alt4)

bruceR::model_summary(model_list = list(Model.alt1, Model.alt2,
                                        Model.alt3, Model.alt4,
                                        model.alt1, model.alt2,
                                        model.alt3, model.alt4))
