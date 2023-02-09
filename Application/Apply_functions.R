# load dependencies
library(worldfootballR)
library(dplyr)
library(tidyverse)

# load custom functions
temp = list.files(path = "./Scripts/", pattern = "*.R")
sapply(paste0("./Scripts/", temp), source)

# get training data
get_training_data(country = "ENG", sex = "M", tier = "1st", season_end_year = "2023", scouting_period = "2022-2023 Premier League")

# get matchday data
#get_matchday_data(country = "GER", sex = "M", tier = "1st", season_end_year = "2023", scouting_period = "2022-2023 Bundesliga")

# kfold testing
#combine_training_data()
#k_fold_testing(country = "GER", sex = "M", tier = "1st", impute_method = "median", impute_with_training = T)
#k_fold_testing_pca(country = "GER", sex = "M", tier = "1st", impute_method = "RF", impute_with_training = T, pca = F)

# train full models and predict next matchday
#predict_matchday(impute_method = "median")
