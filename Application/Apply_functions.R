# load dependencies
library(worldfootballR)
library(dplyr)
library(tidyverse)

# load custom functions
temp = list.files(path = "./Scripts/", pattern = "*.R")
sapply(paste0("./Scripts/", temp), source)

# get training data
get_training_data(country = "ENG", sex = "M", tier = "1st", season_end_year = "2023", scouting_period = "2022-2023 Premier League")

#get_matchday_data(country = "ENG", sex = "M", tier = "1st", season_end_year = "2023", scouting_period = "2022-2023 Premier League")

# combine training data
#combine_training_data()

# kfold testing
#k_fold_testing(country = "GER", sex = "M", tier = "1st", impute_method = "median")

# train full model
#impute_train_full_model(impute_method = "median", country = "GER", sex = "M", tier = "1st")

# predict next matchday
#predict_matchday(impute_method = "median")
