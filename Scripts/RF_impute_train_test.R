########## Test model
RF_traintest_impute = function(path = "./Output",
                                           method = "RF"){

  # create new folder
  dir.create(path = paste0(path, '/train_test_RF_categorical'))
  dir.create(path = paste0(path, '/Training_data/train_test_imputed'))

  # read data
  training_train = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_train.csv'))
  training_test = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_test.csv'))

  ### RF
  if(method == "RF"){
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
  if(method == "mean"){
    # new DFs
    training_train_impute = training_train
    training_test_impute = training_test

    ## compute col mean to replace NAs
    # train
    for(i in 1:ncol(training_train_impute)){
      data[is.na(training_train_impute[,i]), i] <- mean(training_train_impute[,i], na.rm = TRUE)
    }
    # test
    for(i in 1:ncol(training_test_impute)){
      data[is.na(training_test_impute[,i]), i] <- mean(training_test_impute[,i], na.rm = TRUE)
    }
  }

  ### Median
  if(method == "median"){
    # new DFs
    training_train_impute = training_train
    training_test_impute = training_test

    ## compute col median to replace NAs
    # train
    for(i in 1:ncol(training_train_impute)){
      data[is.na(training_train_impute[,i]), i] <- median(training_train_impute[,i], na.rm = TRUE)
    }
    # test
    for(i in 1:ncol(training_test_impute)){
      data[is.na(training_test_impute[,i]), i] <- median(training_test_impute[,i], na.rm = TRUE)
    }
  }

  # write data
  write.csv(training_train_impute, file = paste0(path, '/train_test_imputed/GT_train_imputed.csv'))
  write.csv(training_test_impute, file = paste0(path, '/train_test_imputed/GT_test_imputed.csv'))

  # train
  Outcome = training_train_impute$Outcome
  Outcome_model = randomForest(x = dplyr::select(training_train_impute, -"Outcome"), y = Outcome)

  # predict
  predictions = predict(Outcome_model, training_test_impute)

  # results
  result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
  table(result$Outcome == result$prediction)

  # compute accuracy
  accuracy = mean(result$Outcome == result$prediction)
  accuracy_df = data.frame(accuracy)
  print(paste0("accuracy is ", accuracy))
  # write accuracy
  write.csv(accuracy_df, file = paste0(path, '/train_test_RF_categorical/accuracy.csv'))

  # confusion matrix
  confusion_matrix = caret::confusionMatrix(data=result$prediction, reference=result$Outcome)
  # write
  write.csv(confusion_matrix, file = paste0(path, '/train_test_RF_categorical/confusion_matrix.csv'))

  # important features for prediction
  #Conditional=True, adjusts for correlations between predictors.
  i_scores <- caret::varImp(Outcome_model, conditional=TRUE)
  i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]
  # write
  write.csv(i_scores_sorted, file = paste0(path, '/train_test_RF_categorical/i_scores.csv'))
}


########## Full model
RF_trainFull_impute = function(path = "./Output",
                                           method = "RF"){

  # create new folder
  dir.create(path = paste0(path, '/train_full_RF_categorical'))
  dir.create(path = paste0(path, '/Training_data/train_test_imputed'))

  # read data
  training_train = read.csv(file = paste0(path, '/train_test_split/GT_train.csv'))
  training_test = read.csv(file = paste0(path, '/train_test_split/GT_test.csv'))

  # combine
  training_c = dplyr::bind_rows(training_train, training_test)

  ### RF
  if(method == "RF"){
    # training_c
    if(any(apply(training_c, 2, function(x) is.na(x)))){
      Outcome = training_c$Outcome
      training_c_impute = randomForest::rfImpute(training_c, y = Outcome)
    } else {
      training_c_impute = training_c
    }
    }

  ### Mean
  if(method == "mean"){
    ## compute col mean to replace NAs
    training_c_impute = training_c
    # train_c
    for(i in 1:ncol(training_c)){
      data[is.na(training_c[,i]), i] <- mean(training_c[,i], na.rm = TRUE)
    }
  }

  ### Median
  if(method == "median"){
    ## compute col mean to replace NAs
    training_c_impute = training_c
    # train_c
    for(i in 1:ncol(training_c)){
      data[is.na(training_c[,i]), i] <- median(training_c[,i], na.rm = TRUE)
    }
  }

  # write data
  write.csv(training_train_impute, file = paste0(path, '/train_test_imputed/GT_train_imputed.csv'))
  write.csv(training_test_impute, file = paste0(path, '/train_test_imputed/GT_test_imputed.csv'))

  # train
  Outcome = training_c$Outcome
  Outcome_model = randomForest(x = dplyr::select(training_c_impute, -"Outcome"), y = Outcome)

  # save model
  save(Outcome_model,file = paste0(path, '/train_full_RF_categorical/RF_trainFull.RData'))

  # important features for prediction
  #Conditional=True, adjusts for correlations between predictors.
  i_scores <- caret::varImp(Outcome_model, conditional=TRUE)
  i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]

  # write
  write.csv(i_scores_sorted, file = paste0(path, '/train_full_RF_categorical/i_scores.csv'))
}
