lapply(seq_len(100), function(s) {
  train_test_split()
  path = "./Output"
  impute_method = "median"
  algorithm = "xgboost"
  country = "GER"
  sex = "M"
  tier = "1st"


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
  dir.create(path = paste0(path, '/train_test_', algorithm))
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
  if(algorithm == "RF"){
  # train
  Outcome = training_train_impute$Outcome
  Outcome_model = randomForest(x = dplyr::select(training_train_impute, -c(Outcome, Score)), y = Outcome)

  # predict
  predictions = predict(Outcome_model, dplyr::select(training_test_impute, -c(Outcome, Score)))

  # results
  result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
  write.csv(result, file = paste0(path, '/train_test_', algorithm, '/results.csv'))
  print(table(result$Outcome == result$prediction))
  print(result)

  # number of times loss and win are being confused
  veryfalse = sum(apply(result, 1, function(x) (x["Outcome"] == "win" & x["prediction"] == "loss"))) + sum(apply(result, 1, function(x) (x["Outcome"] == "loss" & x["prediction"] == "win")))
  print(paste0("truth = win or loss and prediction the complete opposite in ", veryfalse, " of ", nrow(result), " cases (Ratio ", veryfalse/nrow(result), ")"))

  # compute accuracy
  accuracy = mean(result$Outcome == result$prediction)
  accuracy_df = data.frame(accuracy)
  print(paste0("accuracy is ", accuracy))
  # write accuracy
  dir.create(path = paste0(path, '/train_test_RF/kfold/'))
  write.csv(accuracy_df, file = paste0(path, '/train_test_RF/kfold/', s,'_accuracy.csv'))

  # confusion matrix
  confusion_matrix = caret::confusionMatrix(data=result$prediction, reference=result$Outcome)
  # save
  jpeg(filename = paste0(path, '/train_test_RF/confusion_matrix.jpg'))
  pheatmap::pheatmap(confusion_matrix[["table"]])
  dev.off()
  # write
  write.csv(as.table(confusion_matrix[["table"]]), file = paste0(path, '/train_test_RF/confusion_matrix.csv'))

  # important features for prediction
  #Conditional=True, adjusts for correlations between predictors.
  i_scores = caret::varImp(Outcome_model, conditional=TRUE)
  i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]
  # write
  write.csv(i_scores_sorted, file = paste0(path, '/train_test_', algorithm, '/i_scores.csv'))
  }

  ### Gradient boosting
  if(algorithm == "xgboost"){

    # train
    Score = training_train_impute$Score
    data_xgboost = as.matrix(dplyr::select(training_train_impute, -c(Outcome, Score)))
    Outcome_model = xgboost::xgboost(data = data_xgboost, label = training_train_impute$Score, max.depth = 3, eta = 1, nthread = 7, nrounds  = 1, verbose = 1)
    # predict
    predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

    # results
    result = data.frame(Outcome = training_test_impute$Score, prediction = predictions)
    write.csv(result, file = paste0(path, '/train_test_', algorithm, '/results.csv'))
    print(result)

    # write accuracy
    #dir.create(path = paste0(path, '/train_test_', algorithm, '/kfold/'))
    #write.csv(accuracy_df, file = paste0(path, '/train_test_', algorithm, '/kfold/', s,'_accuracy.csv'))
  }

 ### naivebayes
  if(algorithm == "naivebayes"){

    # train
    Outcome = training_train_impute$Outcome
    Outcome_model = naivebayes::naive_bayes(training_train_impute$Outcome ~ ., data = dplyr::select(training_train_impute, -c(Outcome, Score)), usekernel = T)
     # predict
    predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

      # results
      result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
      write.csv(result, file = paste0(path, '/train_test_', algorithm, '/results.csv'))
      print(table(as.vector(result$Outcome) == as.vector(result$prediction)))
      print(result)

      # number of times loss and win are being confused
      veryfalse = sum(apply(result, 1, function(x) (x["Outcome"] == "win" & x["prediction"] == "loss"))) + sum(apply(result, 1, function(x) (x["Outcome"] == "loss" & x["prediction"] == "win")))
      print(paste0("truth = win or loss and prediction the complete opposite in ", veryfalse, " of ", nrow(result), " cases (Ratio ", veryfalse/nrow(result), ")"))

      # compute accuracy
      accuracy = mean(as.vector(result$Outcome) == as.vector(result$prediction))
      accuracy_df = data.frame(accuracy)
      print(paste0("accuracy is ", accuracy))
      # write accuracy
      dir.create(path = paste0(path, '/train_test_', algorithm, '/kfold/'))
      write.csv(accuracy_df, file = paste0(path, '/train_test_', algorithm, '/kfold/', s,'_accuracy.csv'))

      # confusion matrix
      confusion_matrix = caret::confusionMatrix(data=result$prediction, reference=result$Outcome)
      # save
      jpeg(filename = paste0(path, '/train_test_', algorithm, '/confusion_matrix.jpg'))
      pheatmap::pheatmap(confusion_matrix[["table"]])
      dev.off()
      # write
      write.csv(as.table(confusion_matrix[["table"]]), file = paste0(path, '/train_test_', algorithm, '/confusion_matrix.csv'))
  }

  ### knn
  if(algorithm == "knn"){

    # train
    Outcome = training_train_impute$Outcome
    #Outcome_model = naive_bayes(training_train_impute$Outcome ~ ., data = dplyr::select(training_train_impute, -c(Outcome, Score)), usekernel = T)
    Outcome_model = naive_bayes(training_train_impute$Outcome ~ ., data = dplyr::select(training_train_impute, -Score), k = 3)
    # predict
    predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

    # results
    result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
    write.csv(result, file = paste0(path, '/train_test_', algorithm, '/results.csv'))
    print(table(as.vector(result$Outcome) == as.vector(result$prediction)))
    print(result)

    # number of times loss and win are being confused
    veryfalse = sum(apply(result, 1, function(x) (x["Outcome"] == "win" & x["prediction"] == "loss"))) + sum(apply(result, 1, function(x) (x["Outcome"] == "loss" & x["prediction"] == "win")))
    print(paste0("truth = win or loss and prediction the complete opposite in ", veryfalse, " of ", nrow(result), " cases (Ratio ", veryfalse/nrow(result), ")"))

    # compute accuracy
    accuracy = mean(as.vector(result$Outcome) == as.vector(result$prediction))
    accuracy_df = data.frame(accuracy)
    print(paste0("accuracy is ", accuracy))
    # write accuracy
    dir.create(path = paste0(path, '/train_test_', algorithm, '/kfold/'))
    write.csv(accuracy_df, file = paste0(path, '/train_test_', algorithm, '/kfold/', s,'_accuracy.csv'))

    # confusion matrix
    confusion_matrix = caret::confusionMatrix(data=result$prediction, reference=result$Outcome)
    # save
    jpeg(filename = paste0(path, '/train_test_', algorithm, '/confusion_matrix.jpg'))
    pheatmap::pheatmap(confusion_matrix[["table"]])
    dev.off()
    # write
    write.csv(as.table(confusion_matrix[["table"]]), file = paste0(path, '/train_test_', algorithm, '/confusion_matrix.csv'))
  }

})

### combine
# read data
path = "./Output"
temp = list.files(path = paste0(path, '/train_test_', algorithm, '/kfold/') ,pattern="*.csv")
kfold_data = lapply(paste0(path, '/train_test_', algorithm, '/kfold/', temp), read.csv)

# combine data
kfold_data_c = plyr::rbind.fill(kfold_data)
mean(kfold_data_c$accuracy)
write.csv(mean(kfold_data_c$accuracy), file = paste0(path, '/train_test_', algorithm, '/accuracy_mean.csv'))
