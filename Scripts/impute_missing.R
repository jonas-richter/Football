impute_missing = function(path = "./Output/Training_data/",
                          method = "RF"){

  # create new folder
  dir.create(path = paste0(path, '/train_test_imputed'))

  # read data
  training_train = read.csv(file = paste0(path, '/train_test_split/GT_train.csv'))
  training_test = read.csv(file = paste0(path, '/train_test_split/GT_test.csv'))

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

    # write
    write.csv(training_train_impute, file = paste0(path, '/train_test_imputed/GT_train_imputed.csv'))
    write.csv(training_test_impute, file = paste0(path, '/train_test_imputed/GT_test_imputed.csv'))

}

