impute_missing = function(path = "./Output/Training_data/",
                          method = "median"){

  # create new folder
  dir.create(path = paste0(path, '/train_test_imputed'))

  # read data
  training_train = read.csv(file = paste0(path, 'train_test_split/GT_train.csv'))
  training_train = dplyr::select(training_train, -X)
  training_test = read.csv(file = paste0(path, 'train_test_split/GT_test.csv'))
  training_test = dplyr::select(training_test, -X)

  # convert Outcome to factor
  training_train$Outcome = as.factor(training_train$Outcome)
  training_test$Outcome = as.factor(training_test$Outcome)

  # drop cols that have ONLY NAs
  training_train = training_train[colSums(!is.na(training_train)) > 0]
  training_test = training_test[colSums(!is.na(training_test)) > 0]

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

    # write
    write.csv(training_train_impute, file = paste0(path, '/train_test_imputed/GT_train_imputed.csv'))
    write.csv(training_test_impute, file = paste0(path, '/train_test_imputed/GT_test_imputed.csv'))

}

