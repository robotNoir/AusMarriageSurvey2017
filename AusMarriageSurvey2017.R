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
relgiousaffiliation.df <- read_csv("~/AusMarriageSurvey2017/data/2016Census_G14_AUS_CED.csv")
CEDtoElectorate <- read_excel("~/AusMarriageSurvey2017/data/2016Census_geog_desc_1st_and_2nd_release.xlsx", sheet = "2016_ASGS_Non-ABS_Structures")

## Cleaning
  # Survey data
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
  
  survey.df <- survey.df %>%
    mutate_at(vars(Response_Yes:Response_None), funs(. / Response_Total))
  
  write.csv(survey.df, './data_clean/surveyresponsescleaned.csv')
  
  # CED-to-Electorate 
  CEDtoElectorate <- CEDtoElectorate %>% 
    filter(ASGS_Structure == "CED") %>%
    select(c(Census_Code_2016, Census_Name_2016)) %>%
    rename(CED_CODE_2016 = Census_Code_2016, Electorate = Census_Name_2016)
  
  # Relgious Affiliations
  relgiousaffiliation.df <- relgiousaffiliation.df %>%
    select(CED_CODE_2016, ends_with("_P")) %>%
    select(1:3,7,22:25,27,29:30,34,35) %>%
    mutate(
      Buddhism_P = Buddhism_P / Tot_P,
      Christianity_Anglican_P = Christianity_Anglican_P / Tot_P,
      Christianity_Catholic_P = Christianity_Catholic_P / Tot_P,
      Hinduism_P = Hinduism_P / Tot_P,
      Islam_P = Islam_P / Tot_P,
      Judaism_P = Judaism_P / Tot_P,
      Othr_Rel_Sikhism_P = Othr_Rel_Sikhism_P / Tot_P,
      SB_OSB_NRA_NR_P = SB_OSB_NRA_NR_P / Tot_P,
      Religious_affiliation_ns_P = Religious_affiliation_ns_P / Tot_P,
      Other_Christianity = (Christianity_Tot_P - Christianity_Anglican_P - Christianity_Catholic_P) / Tot_P,
      Other_Religions = (Other_Religions_Tot_P - Othr_Rel_Sikhism_P) / Tot_P
    ) %>%
    left_join(CEDtoElectorate, by ="CED_CODE_2016") %>%
    select(-CED_CODE_2016, -Christianity_Tot_P, -Other_Religions_Tot_P, -Tot_P) %>%
    rename(
      Buddhism = Buddhism_P,
      Anglican = Christianity_Anglican_P,
      Catholic = Christianity_Catholic_P,
      Hinduism = Hinduism_P, 
      Islam = Islam_P,
      Judaism = Judaism_P,
      Sikhism = Othr_Rel_Sikhism_P,
      Secular_and_Other_Beliefs = SB_OSB_NRA_NR_P,
      Affiliation_Not_Stated = Religious_affiliation_ns_P) %>%
    gather(Religious_Affiliation, Religious_Fraction, Buddhism:Other_Religions) %>%
    filter(!is.na(Religious_Fraction)) %>%
    semi_join(survey.df, by="Electorate")
    
  relgiousaffiliation.df$Religious_Affiliation <- as.factor(relgiousaffiliation.df$Religious_Affiliation)
  
  write.csv(relgiousaffiliation.df, './data_clean/religiousaffiliationscleaned.csv')
  
  # Catholic vs. Yes vote 
  relgiousaffiliation.df %>%
      ggplot(aes(x=Religious_Affiliation, y=Religious_Fraction)) +
      geom_boxplot(alpha=0.6) +
      theme_bw()
  
  relgiousaffiliation.df %>%
    left_join(survey.df, by="Electorate") %>%
    group_by(Religious_Affiliation) %>%
    #filter(Religious_Affiliation == "Secular_and_Other_Beliefs") %>%
    ggplot(aes(x=Religious_Fraction, y=Response_Yes, color=Religious_Affiliation)) +
    geom_point(alpha=0.6) +
    geom_smooth(alpha=0.6) +
    facet_grid(Religious_Affiliation ~ .) +
    theme_minimal()
