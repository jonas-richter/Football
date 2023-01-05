########## Test model
RF_traintest_impute = function(path = "./Output",
                               method = "median"){

  # create new folder
  dir.create(path = paste0(path, '/train_test_RF'))
  dir.create(path = paste0(path, '/Training_data/train_test_imputed'))

  # read data
  training_train = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_train.csv'))
  training_train = dplyr::select(training_train, -X)
  training_test = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_test.csv'))
  training_test = dplyr::select(training_test, -X)

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

  ### Mean
  if(method == "median"){
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

  # train
  Outcome = training_train_impute$Outcome
  Outcome_model = randomForest(x = dplyr::select(training_train_impute, -Outcome), y = Outcome)

  # predict
  predictions = predict(Outcome_model, dplyr::select(training_test_impute, -Outcome))

  # results
  result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
  write.csv(result, file = paste0(path, '/train_test_RF/results.csv'))
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
  write.csv(accuracy_df, file = paste0(path, '/train_test_RF/accuracy.csv'))

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
  i_scores <- caret::varImp(Outcome_model, conditional=TRUE)
  i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]
  # write
  write.csv(i_scores_sorted, file = paste0(path, '/train_test_RF/i_scores.csv'))
}


########## Full model
RF_trainFull_impute = function(path = "./Output",
                                           method = "median"){

  # create new folder
  dir.create(path = paste0(path, '/train_full_RF'))
  dir.create(path = paste0(path, '/Training_data/train_test_imputed'))

  # read data
  training_train = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_train.csv'))
  training_test = read.csv(file = paste0(path, '/Training_data/train_test_split/GT_test.csv'))

  # combine
  training_c = dplyr::bind_rows(training_train, training_test)

  # convert Outcome to factor
  training_c$Outcome = as.factor(training_c$Outcome)

  # drop cols that have ONLY NAs
  training_c = training_c[colSums(!is.na(training_c)) > 0]

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
  write.csv(training_c_impute, file = paste0(path, '/Training_data/train_test_imputed/GT_full_imputed.csv'))

  # train
  Outcome = training_c$Outcome
  Outcome_model = randomForest(x = dplyr::select(training_c_impute, -Outcome), y = Outcome)

  # save model
  save(Outcome_model,file = paste0(path, '/train_full_RF/RF_trainFull.RData'))

  # important features for prediction
  #Conditional=True, adjusts for correlations between predictors.
  i_scores <- caret::varImp(Outcome_model, conditional=TRUE)
  i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]

  # write
  write.csv(i_scores_sorted, file = paste0(path, '/train_full_RF/i_scores.csv'))
}
