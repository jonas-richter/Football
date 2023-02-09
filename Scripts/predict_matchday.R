predict_matchday = function(country,
                            sex,
                            tier,
                            season_end_year,
                            scouting_period,
                            impute_method = "RF"){

  # read matchday data
  temp = list.files(path = paste0("./Output", "/Matchday_data/") ,pattern="*.csv")
  matchday = lapply(paste0("./Output", "/Matchday_data/", temp), read.csv)
  # combine data
  matchday_c = plyr::rbind.fill(matchday)
  # drop unnecessary cols
  matchday_drop = dplyr::select(.data = matchday_c, -c("X", "MatchURL"))

  # league subset
  league_subset = ifelse(country == "GER" & sex =="M" & tier =="1st", 1,
                         ifelse(country == "GER" & sex =="F" & tier =="1st", 2,
                                ifelse(country == "GER" & sex =="M" & tier =="2nd", 3,
                                       ifelse(country == "ENG" & sex =="M" & tier =="1st", 4,
                                              ifelse(country == "ENG" & sex =="F" & tier =="1st", 5,
                                                     ifelse(country == "ENG" & sex =="M" & tier =="2nd", 6,
                                                            ifelse(country == "ITA" & sex =="M" & tier =="1st", 7,
                                                                   ifelse(country == "ITA" & sex =="F" & tier =="1st", 8,
                                                                          ifelse(country == "ITA" & sex =="M" & tier =="2nd", 9,
                                                                                 ifelse(country == "ESP" & sex =="M" & tier =="1st", 10,
                                                                                        ifelse(country == "ESP" & sex =="F" & tier =="1st", 11,
                                                                                               ifelse(country == "ESP" & sex =="M" & tier =="2nd", 12,
                                                                                                      ifelse(country == "FRA" & sex =="M" & tier =="1st", 13,
                                                                                                             ifelse(country == "FRA" & sex =="F" & tier =="1st", 14,
                                                                                                                    ifelse(country == "FRA" & sex =="M" & tier =="2nd", 15,
                                                                                                                           ifelse(country == "all" & sex == "all" & tier == "all", 99, 0))))))))))))))))




  # read data
  temp = list.files(path = "./Output/Training_data/", pattern = "*.csv")
  training = lapply(paste0("./Output/Training_data/", temp), read.csv)
  # combine data
  training_c = plyr::rbind.fill(training)
  # drop unnecessary cols
  training_c = dplyr::select(.data = training_c, -c("X", "MatchURL"))
  # convert Outcome to factor
  training_c$Outcome = as.factor(training_c$Outcome)
  # create new folder
  dir.create(path = paste0("./Output/Training_data/", 'ground_truth_combined'))
  # write combined training data
  write.csv(training_c, file = paste0("./Output/Training_data/", 'ground_truth_combined/training_combined.csv'))

  # convert -Inf and Inf to NA
  training_c[training_c == -Inf] <- NA
  # possibly subset by league/sex/tier
  if(league_subset <= 15){
    training_c = training_c[training_c$league %in% league_subset, ]
  }
  # drop cols that have ONLY NAs
  training_c = training_c[colSums(!is.na(training_c)) > 0]

  # intersect
  matchday_intersect = matchday_drop[,intersect(colnames(matchday_drop), colnames(training_c))]
  training_intersect = training_c[,intersect(colnames(training_c), colnames(matchday_c))]

  ### RF
  if(impute_method == "RF"){
    # train
    if(any(apply(training_intersect, 2, function(x) is.na(x)))){
      Acc_points = training_intersect$Acc_points
      training_intersect_impute = randomForest::rfImpute(dplyr::select(training_intersect, -Acc_points), y = Acc_points)
      training_intersect_impute = data.frame(training_intersect_impute, Outcome = training_c$Outcome, Score = training_c$Score)
    } else {
      training_intersect_impute = data.frame(training_intersect, Outcome = training_c$Outcome, Score = training_c$Score)
    }
  }

  ### Mean
  if(impute_method == "mean"){
    # make num
    training_intersect_num = sapply(training_intersect, as.numeric)

    ## compute col mean to replace NAs
    # train
    for(i in 1:ncol(training_intersect_num)){
      training_intersect_num[is.na(training_intersect_num[,i]), i] <- mean(training_intersect_num[,i], na.rm = TRUE)
    }
    training_intersect_impute = data.frame(training_intersect_num, Outcome = training_c$Outcome, Score = training_c$Score)
  }

  ### Median
  if(impute_method == "median"){
    # make num
    training_intersect_num = sapply(training_intersect, as.numeric)

    ## compute col mean to replace NAs
    # train
    for(i in 1:ncol(training_intersect_num)){
      training_intersect_num[is.na(training_intersect_num[,i]), i] <- median(training_intersect_num[,i], na.rm = TRUE)
    }
    training_intersect_impute = data.frame(training_intersect_num, Outcome = training_c$Outcome, Score = training_c$Score)
  }

  # write data
  write.csv(training_intersect_impute, file = './Output/Training_data/train_test_imputed/GT_full_imputed.csv')


  ### Random Forest
  # train
  Outcome = training_intersect_impute$Outcome
  Outcome_model = randomForest::randomForest(x = dplyr::select(training_intersect_impute, -c(Outcome, Score)), y = Outcome)

  # save model
  saveRDS(Outcome_model, file = './Output/Full_model/RF_trainFull.rds')


  ### Regression
  # train
  Score = training_intersect_impute$Score
  #data_xgboost = as.matrix(dplyr::select(training_intersect_impute, -c(Outcome, Score)))
  #Outcome_model = xgboost::xgboost(data = data_xgboost, label = training_intersect_impute$Score, max.depth = 3, eta = 0.5, nthread = 7, nrounds  = 1, verbose = 1)
  Outcome_model = randomForest::randomForest(x = dplyr::select(training_intersect_impute, -c(Outcome, Score)), y = training_intersect_impute$Score)

  # save model
  saveRDS(Outcome_model,file = './Output/Full_model/RF_regression_trainFull.rds')


  ### naivebayes
  # train
  Outcome = training_intersect_impute$Outcome
  Outcome_model = naivebayes::naive_bayes(training_intersect_impute$Outcome ~ ., data = dplyr::select(training_intersect_impute, -c(Outcome, Score)), usekernel = T)

  # save model
  saveRDS(Outcome_model,file = './Output/Full_model/naivebayes_trainFull.rds')


  ### impute NAs in matchday data

  # drop unnecessary cols
  training_subset = training_intersect
  # subset matchday data
  matchday_subset = matchday_intersect[matchday_intersect$league %in% league_subset, ]


  ### RF
  if(impute_method == "RF"){
    # merge training and matchday data to one df
    training_matchday = rbind(training_subset, matchday_subset, deparse.level = 1)
    # make everything num
    training_matchday_num = data.frame(sapply(training_matchday, as.numeric))
    # impute
    if(any(apply(training_matchday_num, 2, function(x) is.na(x)))){
      Acc_points = training_matchday_num$Acc_points
      training_matchday_num_impute = randomForest::rfImpute(dplyr::select(training_matchday_num, -Acc_points), y = Acc_points)
    } else {
      training_matchday_num_impute = training_matchday_num
    }
    matchday_imputed = data.frame(tail(training_matchday_num_impute, n = nrow(matchday_subset)), row.names = seq(nrow(matchday_subset)))
  }

  ### Mean
  if(impute_method == "mean"){
    # merge training and matchday data to one df
    training_matchday = rbind(training_subset, matchday_subset, deparse.level = 1)
    # make everything num
    training_matchday_num = sapply(training_matchday, as.numeric)

    ## compute col mean to replace NAs
    # train
    for(i in 1:ncol(training_matchday_num)){
      training_matchday_num[is.na(training_matchday_num[,i]), i] <- mean(training_matchday_num[,i], na.rm = TRUE)
    }
    matchday_imputed = data.frame(tail(training_matchday_num, n = nrow(matchday_subset)), row.names = seq(nrow(matchday_subset)))
  }

  ### Median
  if(impute_method == "median"){
    # merge training and matchday data to one df
    training_matchday = rbind(training_subset, matchday_subset, deparse.level = 1)
    # make everything num
    training_matchday_num = sapply(training_matchday, as.numeric)

    ## compute col median to replace NAs
    # train
    for(i in 1:ncol(training_matchday_num)){
      training_matchday_num[is.na(training_matchday_num[,i]), i] <- median(training_matchday_num[,i], na.rm = TRUE)
    }
    matchday_imputed = data.frame(tail(training_matchday_num, n = nrow(matchday_subset)), row.names = seq(nrow(matchday_subset)))
  }

  ## Predict
  # read models
  naivebayes = readRDS(paste0("./Output", "/Full_model/naivebayes_trainFull.rds"))
  RF = readRDS(paste0("./Output", "/Full_model/RF_trainFull.rds"))
  RF_regression = readRDS(paste0("./Output", "/Full_model/RF_regression_trainFull.rds"))

  # RF
  predictions_RF = predict(RF, matchday_imputed)
  # naivebayes
  predictions_naivebayes = predict(naivebayes, matchday_imputed)
  # RF_regression
  predictions_RF_regression = predict(RF_regression, as.matrix(matchday_imputed))

  ## Write prediction data
  dir.create("./Output/Matchday_data/Prediction")

  # get home/away info
  match_info = worldfootballR::fb_match_results(country = country, gender = sex, tier = tier, season_end_year = season_end_year)
  # subset to URLs of match
  match_info = match_info[match_info$MatchURL %in% matchday_c$MatchURL, ]
  # sort df by date
  match_info_date = match_info[order(as.Date(match_info$Date, format="%Y-%m-%d")),]
  # subset to earlier date
  match_info_date_sub = match_info_date[!duplicated(match_info_date$MatchURL),]
  # get home/away info
  match_homeaway = dplyr::left_join(data.frame(MatchURL = matchday_c$MatchURL), dplyr::select(match_info_date_sub, c("MatchURL", "Home", "Away")))

  # combine prediction
  predictions = data.frame(MatchURL = matchday_c$MatchURL,
                           RF = predictions_RF,
                           naivebayes = predictions_naivebayes,
                           RF_regression = predictions_RF_regression)
  predictions = dplyr::left_join(match_homeaway, predictions)
  write.csv(predictions, paste0("./Output", "/Matchday_data/Prediction/prediction.csv"))
}
