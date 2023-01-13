impute_train_full_model = function(path = "./Output",
                                   impute_method = "median",
                                   country,
                                   sex,
                                   tier){

    # league subset for training and testing
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




    # create new folder
    dir.create(path = paste0(path, '/Full_model'))

    # read data
    training_train = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_train.csv'))
    training_train = dplyr::select(training_train, -X)
    training_test = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_test.csv'))
    training_test = dplyr::select(training_test, -X)

    # possibly subset by league/sex/tier
    if(league_subset <= 15){
      training_train = training_train[training_train$league %in% league_subset, ]
      training_test = training_test[training_test$league %in% league_subset, ]
    }

    # combine
    training_c = dplyr::bind_rows(training_train, training_test)

    # convert Outcome to factor
    training_c$Outcome = as.factor(training_c$Outcome)

    # drop cols that have ONLY NAs
    training_c = training_c[colSums(!is.na(training_c)) > 0]

    ### RF
    if(impute_method == "RF"){
      # train
      if(any(apply(training_c, 2, function(x) is.na(x)))){
        Outcome = training_c$Outcome
        training_c_impute = randomForest::rfImpute(training_c, y = Outcome)
      } else {
        training_c_impute = training_c
      }
    }

    ### Mean
    if(impute_method == "mean"){
      # make num
      training_c_num = sapply(training_c, as.numeric)

      ## compute col mean to replace NAs
      # train
      for(i in 1:ncol(training_c_num)){
        training_c_num[is.na(training_c_num[,i]), i] <- mean(training_c_num[,i], na.rm = TRUE)
      }
      training_c_impute = data.frame(training_c_num[,-which(colnames(training_c_num) %in% "Outcome")], Outcome = training_c$Outcome)
    }

    ### Median
    if(impute_method == "median"){
      # make num
      training_c_num = sapply(training_c, as.numeric)

      ## compute col mean to replace NAs
      # train
      for(i in 1:ncol(training_c_num)){
        training_c_num[is.na(training_c_num[,i]), i] <- median(training_c_num[,i], na.rm = TRUE)
      }
      training_c_impute = data.frame(training_c_num[,-which(colnames(training_c_num) %in% "Outcome")], Outcome = training_c$Outcome)
    }

    # write data
    write.csv(training_c_impute, file = paste0(path, '/Training_data/train_test_imputed/GT_full_imputed.csv'))


    ### Random Forest
    # train
    Outcome = training_c_impute$Outcome
    Outcome_model = randomForest::randomForest(x = dplyr::select(training_c_impute, -c(Outcome, Score)), y = Outcome)

    # save model
    saveRDS(Outcome_model,file = paste0(path, '/Full_model/RF_trainFull.rds'))


    ### Regression
    # train
    Score = training_c_impute$Score
    #data_xgboost = as.matrix(dplyr::select(training_c_impute, -c(Outcome, Score)))
    #Outcome_model = xgboost::xgboost(data = data_xgboost, label = training_c_impute$Score, max.depth = 3, eta = 0.5, nthread = 7, nrounds  = 1, verbose = 1)
    Outcome_model = randomForest::randomForest(x = dplyr::select(training_c_impute, -c(Outcome, Score)), y = training_c_impute$Score)

    # save model
    saveRDS(Outcome_model,file = paste0(path, '/Full_model/RF_regression_trainFull.rds'))


    ### naivebayes
    # train
    Outcome = training_c_impute$Outcome
    Outcome_model = naivebayes::naive_bayes(training_c_impute$Outcome ~ ., data = dplyr::select(training_c_impute, -c(Outcome, Score)), usekernel = T)

    # save model
    saveRDS(Outcome_model,file = paste0(path, '/Full_model/naivebayes_trainFull.rds'))
  }
