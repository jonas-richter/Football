k_fold_testing = function(path = "./Output",
                          impute_method = "median",
                          country,
                          sex,
                          tier,
                          season_end_year,
                          k = 100){
  lapply(seq_len(100), function(s) {

    # read data
    temp = list.files(path = "./Output/Training_data/" ,pattern="*.csv")
    training = lapply(paste0("./Output/Training_data/", temp), read.csv)

    # combine data
    training_c = plyr::rbind.fill(training)

    # drop unnecessary cols
    training_c = dplyr::select(.data = training_c, -c("X"))

    # get match_info
    match_urls = worldfootballR::fb_match_urls(country = country, gender = sex, tier = tier, season_end_year = season_end_year)
    # match results
    match_info = worldfootballR::fb_match_results(country = country, gender = sex, tier = tier, season_end_year = season_end_year)
    # subset to past matches
    match_info_past = match_info[!as.vector(sapply(match_info$HomeGoals,is.na)), ]

    # read odd data
    odds = read.csv("./Application/Bundesliga_2223.csv")
    # convert to time format
    match_info_past$Time = lubridate::hm(match_info_past$Time)
    odds$Time = lubridate::hm(odds$Time) + lubridate::hours(1)
    # Date format
    match_info_past$Date = as.Date(match_info_past$Date, format = "%Y-%m-%d")
    odds$Date = as.Date(odds$Date, format = "%d/%m/%Y")

    # rename cols of odds
    odds_rename = dplyr::rename(.data = odds, HomeGoals = FTHG, AwayGoals = FTAG)

    # join
    match_past_odds = dplyr::left_join(match_info_past, odds_rename)

    # remove both duplicates
    match_odds_unique = match_past_odds[!(duplicated(match_past_odds$MatchURL) | duplicated(match_past_odds$MatchURL, fromLast = TRUE)), ]

    # join training data to match_info_past
    match_past_training = dplyr::left_join(match_odds_unique, training_c, by = "MatchURL")




    ####################### PREDICT GAMES WHERE YOU HAVE THE ODDS (TRAIN ON ALL OTHER DATA) --> MAYBE THEN NO K-FOLD NEEDED?





    # select randomly x% of rows
    training_train = training_c[sample(nrow(training_c), round(ratio*nrow(training_c))), ]
    subset = rownames(training_c) %in% rownames(training_train)
    training_test = training_c[subset == FALSE,]

    # how balanced is the train and test set?
    print(table(training_train$Outcome))
    print(table(training_test$Outcome))

    # create new folder
    dir.create(path = paste0(path, '/train_test_split'))

    # write
    write.csv(training_train, file = paste0(path, '/train_test_split/GT_train.csv'))
    write.csv(training_test, file = paste0(path, '/train_test_split/GT_test.csv'))

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
    dir.create(path = paste0(path, '/K_fold_testing'))
    dir.create(path = paste0(path, '/K_fold_testing/RF'))
    dir.create(path = paste0(path, '/K_fold_testing/RF/results'))
    dir.create(path = paste0(path, '/K_fold_testing/RF/accuracy'))
    dir.create(path = paste0(path, '/K_fold_testing/naivebayes'))
    dir.create(path = paste0(path, '/K_fold_testing/naivebayes/results'))
    dir.create(path = paste0(path, '/K_fold_testing/naivebayes/accuracy'))
    dir.create(path = paste0(path, '/K_fold_testing/RF/accuracy'))
    dir.create(path = paste0(path, '/K_fold_testing/RF_regression'))
    dir.create(path = paste0(path, '/K_fold_testing/RF_regression/results'))
    dir.create(path = paste0(path, '/K_fold_testing/RF_regression/test_rmse'))
    dir.create(paste0(path, '/K_fold_testing/groundtruth'))
    dir.create(path = paste0(path, '/Training_data/train_test_imputed'))

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

    # convert Outcome to factor
    training_train$Outcome = as.factor(training_train$Outcome)
    training_test$Outcome = as.factor(training_test$Outcome)

    # drop cols that have ONLY NAs
    training_train = training_train[colSums(!is.na(training_train)) > 0]
    training_test = training_test[colSums(!is.na(training_test)) > 0]

    # make sure that training and test contain similar cols
    training_test = training_test[,intersect(colnames(training_test), colnames(training_train))]
    training_train = training_train[,intersect(colnames(training_test), colnames(training_train))]

    ### RF
    if(impute_method == "RF"){
      # train
      if(any(apply(training_train, 2, function(x) is.na(x)))){
        Outcome = training_train$Outcome
        training_train_impute = randomForest::rfImpute(training_train, y = Outcome)
      } else {
        training_train_impute = training_train
      }
      # test
      if(any(apply(training_test, 2, function(x) is.na(x)))){
        Outcome = training_test$Outcome
        training_test_impute = randomForest::rfImpute(training_test, y = Outcome)
      } else {
        training_test_impute = training_test
      }
    }

    ### Mean
    if(impute_method == "mean"){
      # make num
      training_train_num = sapply(training_train, as.numeric)
      training_test_num = sapply(training_test, as.numeric)

      ## compute col mean to replace NAs
      # train
      for(i in 1:ncol(training_train_num)){
        training_train_num[is.na(training_train_num[,i]), i] <- mean(training_train_num[,i], na.rm = TRUE)
      }
      # test
      for(i in 1:ncol(training_test_num)){
        training_test_num[is.na(training_test_num[,i]), i] <- mean(training_test_num[,i], na.rm = TRUE)
      }
      training_train_impute = data.frame(training_train_num[,-which(colnames(training_train_num) %in% "Outcome")], Outcome = training_train$Outcome)
      training_test_impute = data.frame(training_test_num[,-which(colnames(training_test_num) %in% "Outcome")], Outcome = training_test$Outcome)
    }

    ### Median
    if(impute_method == "median"){
      # make num
      training_train_num = sapply(training_train, as.numeric)
      training_test_num = sapply(training_test, as.numeric)

      ## compute col mean to replace NAs
      # train
      for(i in 1:ncol(training_train_num)){
        training_train_num[is.na(training_train_num[,i]), i] <- median(training_train_num[,i], na.rm = TRUE)
      }
      # test
      for(i in 1:ncol(training_test_num)){
        training_test_num[is.na(training_test_num[,i]), i] <- median(training_test_num[,i], na.rm = TRUE)
      }
      training_train_impute = data.frame(training_train_num[,-which(colnames(training_train_num) %in% "Outcome")], Outcome = training_train$Outcome)
      training_test_impute = data.frame(training_test_num[,-which(colnames(training_test_num) %in% "Outcome")], Outcome = training_test$Outcome)
    }

    # write data
    write.csv(training_train_impute, file = paste0(path, '/Training_data/train_test_imputed/GT_train_imputed.csv'))
    write.csv(training_test_impute, file = paste0(path, '/Training_data/train_test_imputed/GT_test_imputed.csv'))

    ### Random Forest
    # train
    Outcome = training_train_impute$Outcome
    Outcome_model = randomForest::randomForest(x = dplyr::select(training_train_impute, -c(Outcome, Score)), y = Outcome)

    # predict
    predictions = predict(Outcome_model, dplyr::select(training_test_impute, -c(Outcome, Score)))

    # results
    result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
    # write results
    write.csv(result, file = paste0(path, '/K_fold_testing/RF/results/', s, '_results.csv'))

    # number of times loss and win are being confused
    #veryfalse = sum(apply(result, 1, function(x) (x["Outcome"] == "win" & x["prediction"] == "loss"))) + sum(apply(result, 1, function(x) (x["Outcome"] == "loss" & x["prediction"] == "win")))
    #print(paste0("truth = win or loss and prediction the complete opposite in ", veryfalse, " of ", nrow(result), " cases (Ratio ", veryfalse/nrow(result), ")"))

    # compute accuracy
    accuracy = mean(result$Outcome == result$prediction)
    print(paste0("RF: ", accuracy))
    accuracy_df = data.frame(accuracy)
    # write accuracy
    write.csv(accuracy_df, file = paste0(path, '/K_fold_testing/RF/accuracy/', s,'_accuracy.csv'))

    # confusion matrix
    #confusion_matrix = caret::confusionMatrix(data=result$prediction, reference=result$Outcome)
    # save
    #jpeg(filename = paste0(path, '/train_test_RF/confusion_matrix.jpg'))
    #pheatmap::pheatmap(confusion_matrix[["table"]])
    #dev.off()
    # write
    #write.csv(as.table(confusion_matrix[["table"]]), file = paste0(path, '/train_test_RF/confusion_matrix.csv'))

    # important features for prediction
    #Conditional=True, adjusts for correlations between predictors.
    if(s == k){
      i_scores = caret::varImp(Outcome_model, conditional=TRUE)
      i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]
      # write
      write.csv(i_scores_sorted, file = paste0(path, '/K_fold_testing/RF/i_scores.csv'))
    }


    ### Regression
    # train
    Score = training_train_impute$Score
    #data_xgboost = as.matrix(dplyr::select(training_train_impute, -c(Outcome, Score)))
    #nrounds = 1
    #Outcome_model = xgboost::xgboost(data = data_xgboost, label = training_train_impute$Score, max.depth = 3, eta = 0.5, nthread = 7, nrounds = nrounds, verbose = 1)
    Outcome_model = randomForest::randomForest(x = dplyr::select(training_train_impute, -c(Outcome, Score)), y = training_train_impute$Score)

    # predict
    predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

    # results
    result = data.frame(Outcome = training_test_impute$Score, prediction = predictions)
    write.csv(result, file = paste0(path, '/K_fold_testing/RF_regression/results/', s, '_results.csv'))

    # metric
    metric = Metrics::rmse(result$Outcome, result$prediction)
    print(paste0("test-rmse: ", metric))
    # write test rmse
    write.csv(as.numeric(metric), file = paste0(path, '/K_fold_testing/RF_regression/test_rmse/', s,'_test_rmse.csv'))

    ### naivebayes
    # train
    Outcome = training_train_impute$Outcome
    Outcome_model = naivebayes::naive_bayes(training_train_impute$Outcome ~ ., data = dplyr::select(training_train_impute, -c(Outcome, Score)), usekernel = T)
    # predict
    predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

    # results
    result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
    # write results
    write.csv(result, file = paste0(path, '/K_fold_testing/naivebayes/results/', s, '_results.csv'))

    # number of times loss and win are being confused
    #veryfalse = sum(apply(result, 1, function(x) (x["Outcome"] == "win" & x["prediction"] == "loss"))) + sum(apply(result, 1, function(x) (x["Outcome"] == "loss" & x["prediction"] == "win")))
    #print(paste0("truth = win or loss and prediction the complete opposite in ", veryfalse, " of ", nrow(result), " cases (Ratio ", veryfalse/nrow(result), ")"))

    # compute accuracy
    accuracy = mean(as.vector(result$Outcome) == as.vector(result$prediction))
    print(paste0("naivebayes: ", accuracy))
    accuracy_df = data.frame(accuracy)
    # write accuracy
    write.csv(accuracy_df, file = paste0(path, '/K_fold_testing/naivebayes/accuracy/', s, '_accuracy.csv'))

    # confusion matrix
    #confusion_matrix = caret::confusionMatrix(data=result$prediction, reference=result$Outcome)
    # save
    #jpeg(filename = paste0(path, '/train_test_', algorithm, '/confusion_matrix.jpg'))
    #pheatmap::pheatmap(confusion_matrix[["table"]])
    #dev.off()
    # write
    #write.csv(as.table(confusion_matrix[["table"]]), file = paste0(path, '/train_test_', algorithm, '/confusion_matrix.csv'))

    # Write ground truth
    write.csv(dplyr::select(training_test, c(Outcome, Score)), file = paste0(path, '/K_fold_testing/groundtruth/', s, '_groundtruth.csv'))
  })

  ### combine

  ## accuracy
  # RF
  temp = list.files(path = paste0(path, '/K_fold_testing/RF/accuracy') ,pattern="*.csv")
  accuracy_RF = lapply(paste0(path, '/K_fold_testing/RF/accuracy/', temp), read.csv)
  accuracy_RF_c = plyr::rbind.fill(accuracy_RF)[,2]
  # naivebayes
  temp = list.files(path = paste0(path, '/K_fold_testing/naivebayes/accuracy') ,pattern="*.csv")
  accuracy_naivebayes = lapply(paste0(path, '/K_fold_testing/naivebayes/accuracy/', temp), read.csv)
  accuracy_naivebayes_c = plyr::rbind.fill(accuracy_naivebayes)[,2]
  # RF_regression
  temp = list.files(path = paste0(path, '/K_fold_testing/RF_regression/test_rmse') ,pattern="*.csv")
  test_rmse_RF_regression = lapply(paste0(path, '/K_fold_testing/RF_regression/test_rmse/', temp), read.csv)
  test_rmse_RF_regression_c = plyr::rbind.fill(test_rmse_RF_regression)[,2]
  # combine
  accuracy_df = data.frame(accuracy_RF = accuracy_RF_c,
                           accuracy_naivebayes = accuracy_naivebayes_c,
                           test_rmse_RF_regression = test_rmse_RF_regression_c)
  write.csv(accuracy_df, file = paste0(path, '/K_fold_testing/accuracy_k.csv'))
  # mean accuracy
  accuracy_mean = data.frame(accuracy_RF_mean = mean(accuracy_RF_c),
                             accuracy_naivebayes_mean = mean(accuracy_naivebayes_c),
                             test_rmse_RF_regression_mean = mean(test_rmse_RF_regression_c))
  write.csv(accuracy_mean, file = paste0(path, '/K_fold_testing/accuracy_mean.csv'))


  # results
  # RF
  temp = list.files(path = paste0(path, '/K_fold_testing/RF/results') ,pattern="*.csv")
  results_RF = lapply(paste0(path, '/K_fold_testing/RF/results/', temp), read.csv)
  results_RF_c = plyr::rbind.fill(results_RF)[,3]
  # naivebayes
  temp = list.files(path = paste0(path, '/K_fold_testing/naivebayes/results') ,pattern="*.csv")
  results_naivebayes = lapply(paste0(path, '/K_fold_testing/naivebayes/results/', temp), read.csv)
  results_naivebayes_c = plyr::rbind.fill(results_naivebayes)[,3]
  # RF_regression
  temp = list.files(path = paste0(path, '/K_fold_testing/RF_regression/results') ,pattern="*.csv")
  results_RF_regression = lapply(paste0(path, '/K_fold_testing/RF_regression/results/', temp), read.csv)
  results_RF_regression_c = plyr::rbind.fill(results_RF_regression)[,3]
  # groundtruth
  temp = list.files(path = paste0(path, '/K_fold_testing/groundtruth/') ,pattern="*.csv")
  groundtruth = lapply(paste0(path, '/K_fold_testing/groundtruth/', temp), read.csv)
  groundtruth_c = plyr::rbind.fill(groundtruth)[,c(2,3)]
  # combine
  results_df = data.frame(results_RF = results_RF_c,
                          results_naivebayes = results_naivebayes_c,
                          Score_RF_regression = results_RF_regression_c,
                          groundtruth_c)
  write.csv(results_df, file = paste0(path, '/K_fold_testing/results_k.csv'))

  ## get ensemble information
  # subset results_df to rows where naivebayes and RF are in agreement
  results_df_similar = results_df[as.vector(results_df$results_RF) == as.vector(results_df$results_naivebayes), ]
  # accuracy of ensembl
  accuracy_ensembl = mean(as.vector(results_df_similar$results_RF) == as.vector(results_df_similar$Outcome))
  write.csv(accuracy_ensembl, file = paste0(path, '/K_fold_testing/results_ensembl_agreement.csv'))
  # accuracy of RF
  accuracy_RF = mean(as.vector(results_df$results_RF) == as.vector(results_df$Outcome))
  # accuracy of naivebayes
  accuracy_naivebayes = mean(as.vector(results_df$results_naivebayes) == as.vector(results_df$Outcome))
  # write
  accuracy_combined = data.frame(ensembl = accuracy_ensembl,
                                 RF_alone = accuracy_RF,
                                 naivebayes_alone = accuracy_naivebayes)
  write.csv(accuracy_combined, file = paste0(path, '/K_fold_testing/accuracy_ensembl_agreement.csv'))
  print(data.frame(accuracy_combined$ensembl, accuracy_mean))

  # confusion matrix
  confusion_matrix_RF = caret::confusionMatrix(data=as.factor(results_df$results_RF), reference=as.factor(results_df$Outcome))
  confusion_matrix_naivebayes = caret::confusionMatrix(data=as.factor(results_df$results_naivebayes), reference=as.factor(results_df$Outcome))
  confusion_matrix_ensembl = caret::confusionMatrix(data=as.factor(results_df_similar$results_RF), reference=as.factor(results_df_similar$Outcome))
  # save
  jpeg(filename = paste0(path, '/K_fold_testing/confusion_matrix_RF.jpg'))
  pheatmap::pheatmap(confusion_matrix_RF[["table"]])
  dev.off()
  jpeg(filename = paste0(path, '/K_fold_testing/confusion_matrix_naivebayes.jpg'))
  pheatmap::pheatmap(confusion_matrix_naivebayes[["table"]])
  dev.off()
  jpeg(filename = paste0(path, '/K_fold_testing/confusion_matrix_ensembl.jpg'))
  pheatmap::pheatmap(confusion_matrix_ensembl[["table"]])
  dev.off()

  jpeg(filename = paste0(path, '/K_fold_testing/Score.jpg'))
  plot(results_df$Score_RF_regression, results_df$Score)
  dev.off()

  ## evaluate Score prediction
  # GET EQUATION AND R-SQUARED AS STRING
  # SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

  lm_eqn <- function(df){
    m <- lm(Score ~ Score_RF_regression, df);
    eq <- substitute(italic(Score) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }

  ggplot2::ggplot(data = results_df, ggplot2::aes(Score_RF_regression, Score))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm")+
    ggplot2::geom_text(x = -1, y = 2.5, label = lm_eqn(results_df), parse = TRUE)

  ggplot2::ggsave(paste0(path, '/K_fold_testing/Score.jpg'))

}
