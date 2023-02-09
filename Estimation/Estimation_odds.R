impute_method = "RF"
country = "GER"
sex = "M"
tier = "1st"
season_end_year = "2023"

# read data
temp = list.files(path = "./Output/Training_data/" ,pattern="*.csv")
training = lapply(paste0("./Output/Training_data/", temp), read.csv)

# combine data
training_c = plyr::rbind.fill(training)

# drop unnecessary cols
training_c = dplyr::select(.data = training_c, -c("X"))

# get match_info
match_urls = worldfootballR::fb_match_urls(country = country, gender = sex, tier = tier, season_end_year = season_end_year)
match_info = worldfootballR::fb_match_results(country = country, gender = sex, tier = tier, season_end_year = season_end_year)
# subset to past matches
match_info_past = match_info[!as.vector(sapply(match_info$HomeGoals,is.na)), ]

# read odd data
odds = if(country == "ENG" & season_end_year == "2023") {
  read.csv("./Estimation/odds/Premier_League_2223.csv")
} else if(country == "ENG" & season_end_year == "2022") {
  read.csv("./Estimation/odds/Premier_League_2122.csv")
} else if(country == "ENG" & season_end_year == "2021") {
  read.csv("./Estimation/odds/Premier_League_2021.csv")
}else if(country == "GER" & season_end_year == "2023") {
  read.csv("./Estimation/odds/Bundesliga_2223.csv")
} else if(country == "GER" & season_end_year == "2022") {
  read.csv("./Estimation/odds/Bundesliga_2122.csv")
}else if(country == "GER" & season_end_year == "2021") {
  read.csv("./Estimation/odds/Bundesliga_2021.csv")
}

# convert to time format
match_info_past$Time = lubridate::hm(match_info_past$Time)
if(country == "GER"){
  odds$Time = lubridate::hm(odds$Time) + lubridate::hours(1)
}else{
  odds$Time = lubridate::hm(odds$Time)
}
# Date format
match_info_past$Date = as.Date(match_info_past$Date, format = "%Y-%m-%d")
odds$Date = as.Date(odds$Date, format = "%d/%m/%Y")

# rename cols of odds
odds_rename = dplyr::rename(.data = odds, HomeGoals = FTHG, AwayGoals = FTAG)
# join
match_past_odds = dplyr::left_join(match_info_past, odds_rename, c("Date", "Time", "HomeGoals", "AwayGoals"))
# remove both duplicates
match_odds_unique = match_past_odds[!(duplicated(match_past_odds$MatchURL) | duplicated(match_past_odds$MatchURL, fromLast = TRUE)), ]
# join training data to match_info_past
match_past_training = dplyr::left_join(match_odds_unique, training_c, by = "MatchURL")

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




# filter for league_subset
training_league = training_c[training_c$league %in% league_subset, ]
# training data (every row except the ones which have odds information)
training_train = training_league[!(training_league$MatchURL %in% match_past_training$MatchURL), ]
# test data (data with odds info)
training_test = match_past_training[!is.na(match_past_training$Outcome),]

# create new folder
dir.create(path = paste0("./Estimation/", country, "_estimate_", season_end_year))

# write
write.csv(training_train, file = paste0("./Estimation/", country, "_estimate_", season_end_year, "/GT_train.csv"))
write.csv(training_test, file = paste0("./Estimation/", country, "_estimate_", season_end_year, "/GT_test.csv"))

# convert Outcome to factor
training_train$Outcome = as.factor(training_train$Outcome)
training_test$Outcome = as.factor(training_test$Outcome)

# drop cols that have ONLY NAs
training_train = training_train[colSums(!is.na(training_train)) > 0]
training_test = training_test[colSums(!is.na(training_test)) > 0]

# convert -Inf and Inf to NA
training_train[training_train == -Inf] <- NA
training_test[training_test == -Inf] <- NA
training_train[training_train == Inf] <- NA
training_test[training_test == Inf] <- NA

# make sure that training and test contain similar cols
training_test = training_test[,intersect(colnames(training_test), colnames(training_train))]
training_train = training_train[,intersect(colnames(training_test), colnames(training_train))]

# drop matchurls
train_url = dplyr::select(training_train, "MatchURL")
test_url = dplyr::select(training_test, "MatchURL")
training_train = dplyr::select(training_train, -"MatchURL")
training_test = dplyr::select(training_test, -"MatchURL")

# combine training and test for imputation
train_test_c = rbind(training_train, training_test, deparse.level = 1)

### RF
if(impute_method == "RF"){
  # train
  if(any(apply(train_test_c, 2, function(x) is.na(x)))){
    Acc_points = train_test_c$Acc_points
    train_test_c_impute = randomForest::rfImpute(dplyr::select(train_test_c, -Acc_points), y = Acc_points)
  } else {
    train_test_c_impute = train_test_c
  }
}

### Mean
if(impute_method == "mean"){
  # make num
  train_test_c_num = data.frame(sapply(dplyr::select(train_test_c, -Outcome), as.numeric))

  ## compute col mean to replace NAs
  # train
  for(i in 1:ncol(train_test_c_num)){
    train_test_c_num[is.na(train_test_c_num[,i]), i] <- mean(train_test_c_num[,i], na.rm = TRUE)
  }
  train_test_c_impute = data.frame(train_test_c_num[,-which(colnames(train_test_c_num) %in% "Acc_points")], Acc_points = train_test_c_num$Acc_points)
  train_test_c_impute$Outcome = train_test_c$Outcome
}

### Median
if(impute_method == "median"){
  # make num
  train_test_c_num = data.frame(sapply(dplyr::select(train_test_c, -Outcome), as.numeric))

  ## compute col median to replace NAs
  # train
  for(i in 1:ncol(train_test_c_num)){
    train_test_c_num[is.na(train_test_c_num[,i]), i] <- median(train_test_c_num[,i], na.rm = TRUE)
  }
  train_test_c_impute = data.frame(train_test_c_num[,-which(colnames(train_test_c_num) %in% "Acc_points")], Acc_points = train_test_c_num$Acc_points)
  train_test_c_impute$Outcome = train_test_c$Outcome
}

# split in train and test
training_train_impute = data.frame(head(train_test_c_impute, n = nrow(training_train)), row.names = seq(nrow(training_train)))
training_test_impute = data.frame(tail(train_test_c_impute, n = nrow(training_test)), row.names = seq(nrow(training_test)))

### Random Forest
# train
Outcome = training_train_impute$Outcome
Outcome_model = randomForest::randomForest(x = dplyr::select(training_train_impute, -c(Outcome, Score)), y = Outcome)

# predict
predictions = predict(Outcome_model, dplyr::select(training_test_impute, -c(Outcome, Score)))

# results
result_RF = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)

# compute accuracy
accuracy = mean(result_RF$Outcome == result_RF$prediction)
print(paste0("RF: ", accuracy))
accuracy_df = data.frame(accuracy)

### Regression
# train
Score = training_train_impute$Score
#Outcome_model = xgboost::xgboost(data = data_xgboost, label = training_train_impute$Score, max.depth = 3, eta = 0.5, nthread = 7, nrounds = nrounds, verbose = 1)
Outcome_model = randomForest::randomForest(x = dplyr::select(training_train_impute, -c(Outcome, Score)), y = training_train_impute$Score)

# predict
predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

# results
result_RF_regression = data.frame(Outcome = training_test_impute$Score, prediction = predictions)

# metric
metric = Metrics::rmse(result_RF_regression$Outcome, result_RF_regression$prediction)
print(paste0("test-rmse: ", metric))
# write test rmse
#write.csv(as.numeric(metric), file = paste0(path, '/K_fold_testing/RF_regression/test_rmse/', s,'_test_rmse.csv'))

### naivebayes
# train
Outcome = training_train_impute$Outcome
Outcome_model = naivebayes::naive_bayes(training_train_impute$Outcome ~ ., data = dplyr::select(training_train_impute, -c(Outcome, Score)), usekernel = T)
# predict
predictions = predict(Outcome_model, as.matrix(dplyr::select(training_test_impute, -c(Outcome, Score))))

# results
result_naivebayes = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)

# compute accuracy
accuracy = mean(as.vector(result_naivebayes$Outcome) == as.vector(result_naivebayes$prediction))
print(paste0("naivebayes: ", accuracy))
accuracy_df = data.frame(accuracy)

# results
results_df = data.frame(results_RF = result_RF$prediction,
                        results_naivebayes = result_naivebayes$prediction,
                        Score_RF_regression = result_RF_regression$prediction,
                        Outcome = match_past_training[!is.na(match_past_training$Outcome),]$Outcome,
                        match_past_training[!is.na(match_past_training$Outcome),colnames(match_past_training) %in% c("IWH", "IWD", "IWA")])
results_df = na.omit(results_df)
write.csv(results_df, file = paste0("./Estimation/", country, "_estimate_", season_end_year, "/results.csv"))

# RF
## Bet on everything

a = ifelse(results_df$results_RF == "win" & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$results_RF == "draw" & results_df$Outcome == "draw", results_df$IWD -1,
                  ifelse(results_df$results_RF == "loss" & results_df$Outcome == "loss", results_df$IWA -1, -1)))
sum(a)
table(a)

## Bet on wins and losses only

b = ifelse(results_df$results_RF == "win" & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$results_RF == "win" & results_df$Outcome != "win", -1,
                  ifelse(results_df$results_RF == "loss" & results_df$Outcome != "loss", -1,
                         ifelse(results_df$results_RF == "loss" & results_df$Outcome == "loss", results_df$IWA -1, 0))))
sum(b)
table(b)

## Bet on wins only

c = ifelse(results_df$results_RF == "win" & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$results_RF == "win" & results_df$Outcome != "win", -1, 0))
sum(c)
table(c)

## Bet on loss only

d = ifelse(results_df$results_RF == "loss" & results_df$Outcome == "loss", results_df$IWA -1,
           ifelse(results_df$results_RF == "loss" & results_df$Outcome != "loss", -1, 0))
sum(d)
table(d)

# naivebayes
## Bet on everything

a2 = ifelse(results_df$results_naivebayes == "win" & results_df$Outcome == "win", results_df$IWH -1,
            ifelse(results_df$results_naivebayes == "draw" & results_df$Outcome == "draw", results_df$IWD -1,
                   ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", results_df$IWA -1, -1)))
sum(a2)
table(a2)

## Bet on wins and losses only

b2 = ifelse(results_df$results_naivebayes == "win" & results_df$Outcome == "win", results_df$IWH -1,
            ifelse(results_df$results_naivebayes == "win" & results_df$Outcome != "win", -1,
                   ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome != "loss", -1,
                          ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", results_df$IWA -1, 0))))
sum(b2)
table(b2)

## Bet on wins only

c2 = ifelse(results_df$results_naivebayes == "win" & results_df$Outcome == "win", results_df$IWH -1,
            ifelse(results_df$results_naivebayes == "win" & results_df$Outcome != "win", -1, 0))
sum(c2)
table(c2)

## Bet on loss only

d2 = ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", results_df$IWA -1,
            ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome != "loss", -1, 0))
sum(d2)
table(d2)



# ENSEMBLE
## Bet on everything

e = ifelse(results_df$results_RF == "win" & results_df$results_naivebayes == "win" & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$results_RF == "draw" & results_df$results_naivebayes == "draw" & results_df$Outcome == "draw", results_df$IWD -1,
                  ifelse(results_df$results_RF == "loss" & results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", results_df$IWA -1, -1)))

sum(e)
table(e)

## Bet on wins and losses only

f = ifelse(results_df$results_RF == "win" & results_df$Outcome == "win" & results_df$results_naivebayes == "win", results_df$IWH -1,
           ifelse(results_df$results_RF == "win" & results_df$results_naivebayes == "win" & results_df$Outcome != "win", -1,
                  ifelse(results_df$results_RF == "loss" & results_df$results_naivebayes == "loss" & results_df$Outcome != "loss", -1,
                         ifelse(results_df$results_RF == "loss" & results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", results_df$IWA -1, 0))))

sum(f)
table(f)

## Bet on wins only

g = ifelse(results_df$results_RF == "win" & results_df$results_naivebayes == "win" & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$results_RF == "win" & results_df$results_naivebayes == "win" & results_df$Outcome != "win", -1, 0))

sum(g)
table(g)

## Bet on loss only

h = ifelse(results_df$results_RF == "loss" & results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", results_df$IWA -1,
           ifelse(results_df$results_RF == "loss" & results_df$results_naivebayes == "loss" & results_df$Outcome != "loss", -1, 0))

sum(h)
table(h)

# Strengths of individual classifiers

# accuracy wins naivebayes
s = ifelse(results_df$results_naivebayes == "win" & results_df$Outcome == "win", TRUE,
           ifelse(results_df$results_naivebayes == "win" & results_df$Outcome != "win", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy wins RF
s = ifelse(results_df$results_RF == "win" & results_df$Outcome == "win", TRUE,
           ifelse(results_df$results_RF == "win" & results_df$Outcome != "win", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy losss naivebayes
s = ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome == "loss", TRUE,
           ifelse(results_df$results_naivebayes == "loss" & results_df$Outcome != "loss", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy losss RF
s = ifelse(results_df$results_RF == "loss" & results_df$Outcome == "loss", TRUE,
           ifelse(results_df$results_RF == "loss" & results_df$Outcome != "loss", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy draw naivebayes
s = ifelse(results_df$results_naivebayes == "draw" & results_df$Outcome == "draw", TRUE,
           ifelse(results_df$results_naivebayes == "draw" & results_df$Outcome != "draw", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy draw RF
s = ifelse(results_df$results_RF == "draw" & results_df$Outcome == "draw", TRUE,
           ifelse(results_df$results_RF == "draw" & results_df$Outcome != "draw", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy wins ensembl
s = ifelse(results_df$results_naivebayes == "win" & results_df$results_RF == "win" & results_df$Outcome == "win", TRUE,
           ifelse(results_df$results_naivebayes == "win" & results_df$results_RF == "win" & results_df$Outcome != "win", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# accuracy loss ensembl
s = ifelse(results_df$results_naivebayes == "loss" & results_df$results_RF == "loss" & results_df$Outcome == "loss", TRUE,
           ifelse(results_df$results_naivebayes == "loss" & results_df$results_RF == "loss" & results_df$Outcome != "loss", FALSE, 8))
s_clean = s[!s %in% 8]
mean(s_clean)

# ENSEMBLE single choice
i = ifelse(results_df$results_naivebayes == "win" & results_df$results_RF != "loss" & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$results_naivebayes != "win" & results_df$results_RF == "loss" & results_df$Outcome == "loss", results_df$IWA -1,
                  ifelse(results_df$results_naivebayes == "win" & results_df$results_RF != "loss" & results_df$Outcome == "loss", -1,
                         ifelse(results_df$results_naivebayes == "win" & results_df$results_RF != "loss" & results_df$Outcome == "draw", -1,
                                ifelse(results_df$results_naivebayes != "win" & results_df$results_RF == "loss" & results_df$Outcome == "win", -1,
                                       ifelse(results_df$results_naivebayes != "win" & results_df$results_RF == "loss" & results_df$Outcome == "draw", -1,
                                              0))))))
sum(i)
table(i)

# according to bookmakers
j = ifelse(results_df$IWH < results_df$IWD & results_df$IWH < results_df$IWA & results_df$Outcome == "win", results_df$IWH -1,
           ifelse(results_df$IWD < results_df$IWH & results_df$IWD < results_df$IWA& results_df$Outcome == "draw", results_df$IWD -1,
                  ifelse(results_df$IWA < results_df$IWH & results_df$IWA < results_df$IWD & results_df$Outcome == "loss", results_df$IWA -1,
                         -1)))
sum(j)
table(j)

# combine to data frame
df = data.frame(everything_RF = sum(a),
                wins_losses_RF = sum(b),
                wins_RF = sum(c),
                losses_RF = sum(d),
                everything_naivebayes = sum(a2),
                wins_losses_naivebayes = sum(b2),
                wins_naivebayes = sum(c2),
                losses_naivebayes = sum(d2),
                everything_ensembl = sum(e),
                wins_losses_ensembl = sum(f),
                wins_ensembl = sum(g),
                losses_ensembl = sum(h),
                wins_losses_selected = sum(i),
                bookmaker = sum(j))
df_c = tidyr::pivot_longer(data = df, cols = 1:ncol(df))


# Plot
library(ggplot2)
ggplot(df_c, aes(x = reorder(name, -value), y = value))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = paste0("./Estimation/", country, "_estimate_", season_end_year, "/", Sys.time(), ".jpg"))

# write win
number_bets = data.frame(number_bets = nrow(training_test),
                         everything_RF = sum(a),
                         wins_losses_RF = sum(b),
                         wins_RF = sum(c),
                         losses_RF = sum(d),
                         everything_naivebayes = sum(a2),
                         wins_losses_naivebayes = sum(b2),
                         wins_naivebayes = sum(c2),
                         losses_naivebayes = sum(d2),
                         everything_ensembl = sum(e),
                         wins_losses_ensembl = sum(f),
                         wins_ensembl = sum(g),
                         losses_ensembl = sum(h),
                         wins_losses_selected = sum(i),
                         bookmaker = sum(j))
write.csv(number_bets, file = paste0("./Estimation/", country, "_estimate_", season_end_year, "/win_sum.csv"))

# read win information from all dirs in the Estimation directory
temp = list.dirs(path = "./Estimation")
temp_str = na.omit(stringr::str_extract(string = temp, pattern = ".*_estimate_.*"))
win_sum_c = lapply(temp_str, function(x) read.csv(paste0(x, "/win_sum.csv")))
# get win in %
win_sum_percent = lapply(win_sum_c, function(x) apply(x[, !colnames(x) %in% "number_bets"], 2, function(y) y/x$number_bets)*100)
s = c()
win_sum_percent_c = data.frame(t(sapply(win_sum_percent, function(x) s = x)))[,-1]
# add league info
win_sum_percent_c$league = stringr::str_remove(string = temp_str, pattern = ".*/.*/")
# pivot
win_sum_pivot = tidyr::pivot_longer(win_sum_percent_c, cols = c(1:14), names_to = "approach")

# plot
ggplot(win_sum_pivot, aes(x = reorder(approach, -value), y = value))+
  geom_boxplot()+
  geom_point(aes(color = league))+
  theme(axis.text.x = element_text(angle = 90))
ggsave("./Estimation/win_leagues.jpg", width = 15, height = 15, units = "cm")
