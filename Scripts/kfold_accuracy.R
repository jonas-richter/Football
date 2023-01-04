
k_fold_testing = function(path,
                          impute_method,
                          country,
                          sex,
                          tier,
                          k = 100){
  lapply(seq_len(100), function(s) {
    # run train/test split
    train_test_split()

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
    dir.create(path = paste0(path, '/K_fold_testing/xgboost'))
    dir.create(path = paste0(path, '/K_fold_testing/xgboost/results'))
    dir.create(path = paste0(path, '/K_fold_testing/xgboost/train_rmse'))
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
    Outcome_model = randomForest(x = dplyr::select(training_train_impute, -c(Outcome, Score)), y = Outcome)

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


    ### Gradient boosting
    # train
    Score = training_train_impute$Score
    data_xgboost = as.matrix(dplyr::select(training_train_impute, -c(Outcome, Score)))
    Outcome_model = xgboost::xgboost(data = data_xgboost, label = training_train_impute$Score, max.depth = 3, eta = 1, nthread = 7, nrounds  = 1, verbose = 1)

    # write train rmse
    write.csv(as.numeric(Outcome_model$evaluation_log[,2]), file = paste0(path, '/K_fold_testing/xgboost/train_rmse/', s,'_train_rmse.csv'))

    # predict
    predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

    # results
    result = data.frame(Outcome = training_test_impute$Score, prediction = predictions)
    write.csv(result, file = paste0(path, '/K_fold_testing/xgboost/results/', s, '_results.csv'))

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
  # xgboost
  temp = list.files(path = paste0(path, '/K_fold_testing/xgboost/train_rmse') ,pattern="*.csv")
  train_rmse_xgboost = lapply(paste0(path, '/K_fold_testing/xgboost/train_rmse/', temp), read.csv)
  train_rmse_xgboost_c = plyr::rbind.fill(train_rmse_xgboost)[,2]
  # combine
  accuracy_df = data.frame(accuracy_RF = accuracy_RF_c,
                           accuracy_naivebayes = accuracy_naivebayes_c,
                           train_rmse_xgboost = train_rmse_xgboost_c)
  write.csv(accuracy_df, file = paste0(path, '/K_fold_testing/accuracy_k.csv'))
  # mean accuracy
  accuracy_mean = data.frame(accuracy_RF_mean = mean(accuracy_RF_c),
                             accuracy_naivebayes_mean = mean(accuracy_naivebayes_c),
                             train_rmse_xgboost_mean = mean(train_rmse_xgboost_c))
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
  # xgboost
  temp = list.files(path = paste0(path, '/K_fold_testing/xgboost/results') ,pattern="*.csv")
  results_xgboost = lapply(paste0(path, '/K_fold_testing/xgboost/results/', temp), read.csv)
  results_xgboost_c = plyr::rbind.fill(results_xgboost)[,3]
  # groundtruth
  temp = list.files(path = paste0(path, '/K_fold_testing/groundtruth/') ,pattern="*.csv")
  groundtruth = lapply(paste0(path, '/K_fold_testing/groundtruth/', temp), read.csv)
  groundtruth_c = plyr::rbind.fill(groundtruth)[,c(2,3)]
  # combine
  results_df = data.frame(results_RF = results_RF_c,
                          results_naivebayes = results_naivebayes_c,
                          Score_xgboost = results_xgboost_c,
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
  confusion_matrix_RF = caret::confusionMatrix(data=as.factor(results_df$results_RF), reference=as.factor(results_df$results_naivebayes))
  confusion_matrix_naivebayes = caret::confusionMatrix(data=as.factor(results_df$results_naivebayes), reference=as.factor(results_df$results_naivebayes))
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
  plot(results_df$Score_xgboost, results_df$Score)
  dev.off()

  ## evaluate Score prediction
  # GET EQUATION AND R-SQUARED AS STRING
  # SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

  lm_eqn <- function(df){
    m <- lm(Score ~ Score_xgboost, df);
    eq <- substitute(italic(Score) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }

  ggplot2::ggplot(data = results_df, aes(Score_xgboost, Score))+
    geom_point()+
    geom_smooth(method = "lm")+
    geom_text(x = -1, y = 2.5, label = lm_eqn(results_df), parse = TRUE)

  ggsave(paste0(path, '/K_fold_testing/Score.jpg'))

}

############################# remove this later
path = "./Output"
impute_method = "median"
country = "GER"
sex = "M"
tier = "1st"
#############################
k_fold_testing(path = "./Output", impute_method = "median", country = "GER", sex = "M", tier = "1st")
