# ----------------------------------------------
#
# 2017 Same-Sex Marriage Law Survey in Australia
# Angus Robertson
# December 2017
#
# ----------------------------------------------

## Initialisation
rm(list=ls())

library(tidyverse)
library(ggplot2)

survey.df <- read_excel("./data/australian_marriage_law_postal_survey_2017_-_response_final.xls", sheet = "Table 2", col_names = FALSE)

## Cleaning
survey.df <- survey.df %>% 
  select(c(X__1, X__2, X__4, X__11, X__13, X__15)) %>%
  rename(
    Electorate = X__1,
    Response_Yes = X__2,
    Response_No = X__4, 
    Response_Unclear = X__11, 
    Response_None = X__13, 
    Response_Total = X__15) %>%
  slice(c(9:55,59:95,99:128,132:142,146:161,165:169,173:174,178:179))

survey.df$State <- NA
survey.df[1:47,]$State <- 'NSW'
survey.df[48:84,]$State <- 'VIC'
survey.df[85:114,]$State <- 'QLD'
survey.df[115:125,]$State <- 'SA'
survey.df[126:141,]$State <- 'WA'
survey.df[142:146,]$State <- 'TAS'
survey.df[147:148,]$State <- 'NT'
survey.df[149:150,]$State <- 'ACT'

survey.df$State <- as.factor(survey.df$State)
survey.df$Response_Yes <- as.integer(survey.df$Response_Yes)
survey.df$Response_No <- as.integer(survey.df$Response_No)
survey.df$Response_Unclear <- as.integer(survey.df$Response_Unclear)
survey.df$Response_None <- as.integer(survey.df$Response_None)
survey.df$Response_Total <- as.integer(survey.df$Response_Total)

write.csv(survey.df, './data_clean/surveyresponsescleaned.csv')
