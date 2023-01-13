predict_matchday = function(path = "./Output",
                            impute_method = "median"){

# read models
naivebayes = readRDS(paste0(path, "/Full_model/naivebayes_trainFull.rds"))
RF = readRDS(paste0(path, "/Full_model/RF_trainFull.rds"))
RF_regression = readRDS(paste0(path, "/Full_model/RF_regression_trainFull.rds"))

# read matchday data
temp = list.files(path = paste0(path, "/Matchday_data/") ,pattern="*.csv")
matchday = lapply(paste0(path, "/Matchday_data/", temp), read.csv)
# combine data
matchday_c = plyr::rbind.fill(matchday)
# drop unnecessary cols
matchday_drop = dplyr::select(.data = matchday_c, -c("X", "MatchURL"))

## impute NAs, based on data the models have been trained with
# read imputed trainings data
training_imputed = read.csv(paste0(path, '/Training_data/train_test_imputed/GT_full_imputed.csv'))
# drop unnecessary cols
training_subset = dplyr::select(.data = training_imputed, -c("X", "Score"))

## subset matchday data to the league/sex/tier which was used to train the models
league_subset = as.vector(unique(training_subset$league))
# subset matchday data
matchday_subset = matchday_drop[matchday_drop$league %in% league_subset, ]

### RF
if(impute_method == "RF"){
  print("please use mean or median to impute NAs in the matchday data")
}

### Mean
if(impute_method == "mean"){
  # merge training and matchday data to one df
  training_matchday = rbind(dplyr::select(training_subset, -"Outcome"), matchday_subset, deparse.level = 1)
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
  training_matchday = rbind(dplyr::select(training_subset, -"Outcome"), matchday_subset, deparse.level = 1)
  # make everything num
  training_matchday_num = sapply(training_matchday, as.numeric)

  ## compute col mean to replace NAs
  # train
  for(i in 1:ncol(training_matchday_num)){
    training_matchday_num[is.na(training_matchday_num[,i]), i] <- median(training_matchday_num[,i], na.rm = TRUE)
  }
  matchday_imputed = data.frame(tail(training_matchday_num, n = nrow(matchday_subset)), row.names = seq(nrow(matchday_subset)))
}

## Predict
# RF
predictions_RF = predict(RF, matchday_imputed)
# naivebayes
predictions_naivebayes = predict(naivebayes, matchday_imputed)
# RF_regression
predictions_RF_regression = predict(RF_regression, as.matrix(matchday_imputed))

## Write prediction data
dir.create(paste0(path, "/Matchday_data/Prediction"))
# combine prediction
predictions = data.frame(match = stringr::str_remove(matchday_c$MatchURL, pattern = ".*/"),
                         RF = predictions_RF,
                         naivebayes = predictions_naivebayes,
                         RF_regression = predictions_RF_regression)
write.csv(predictions, paste0(path, "/Matchday_data/Prediction/prediction.csv"))
}
