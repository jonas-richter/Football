impute_method = "RF"
country = "GER"
sex = "M"
tier = "1st"
season_end_year = "2021"
pca = F
impute_with_training = T

# load package
library(ggplot2)

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

if(impute_with_training){

  # combine train and test for imputation
  training_test_c = rbind(training_test, training_train)

  ### RF
  if(impute_method == "RF"){
    # train
    if(any(apply(training_test_c, 2, function(x) is.na(x)))){
      Acc_points = training_test_c$Acc_points
      training_test_c_impute = randomForest::rfImpute(dplyr::select(training_test_c, -Acc_points), y = Acc_points)
    } else {
      training_test_c_impute = training_test_c
    }
  }

  ### Mean
  if(impute_method == "mean"){
    # make num
    training_test_c_num = sapply(training_test_c, as.numeric)
    training_test_num = sapply(training_test, as.numeric)

    ## compute col mean to replace NAs
    # train
    for(i in 1:ncol(training_test_c_num)){
      training_test_c_num[is.na(training_test_c_num[,i]), i] <- mean(training_test_c_num[,i], na.rm = TRUE)
    }
    training_test_c_impute = data.frame(training_test_c_num[,-which(colnames(training_test_c_num) %in% "Outcome")], Outcome = training_test_c$Outcome)
  }

  ### Median
  if(impute_method == "median"){
    # make num
    training_test_c_num = sapply(training_test_c, as.numeric)
    training_test_num = sapply(training_test, as.numeric)

    ## compute col mean to replace NAs
    # train
    for(i in 1:ncol(training_test_c_num)){
      training_test_c_num[is.na(training_test_c_num[,i]), i] <- median(training_test_c_num[,i], na.rm = TRUE)
    }
    training_test_c_impute = data.frame(training_test_c_num[,-which(colnames(training_test_c_num) %in% "Outcome")], Outcome = training_test_c$Outcome)
  }

  # split again in training and test
  training_test_impute = head(training_test_c_impute, n = nrow(training_test))
  training_train_impute = tail(training_test_c_impute, n = nrow(training_train))
}

if(!impute_with_training){
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
}

# write data
write.csv(training_train_impute, file = paste0("./Output", '/Training_data/train_test_imputed/GT_train_imputed.csv'))
write.csv(training_test_impute, file = paste0("./Output", '/Training_data/train_test_imputed/GT_test_imputed.csv'))

# save train and test outcome
Outcome_train = training_train_impute$Outcome
Outcome_test = training_test_impute$Outcome
Score_train = training_train_impute$Score
Score_test = training_test_impute$Score

if(pca){
  # combine train and test for imputation
  train_test_c_impute = rbind(training_train_impute, training_test_impute)
  # Perform PCA without scaling
  pca_fit <- prcomp(train_test_c_impute[, !colnames(train_test_c_impute) %in% c("Per90_mean_Center.Backs_Straight.Corner.Kicks", "X2", "league", "Outcome", "Score")], center = FALSE, scale. = FALSE)
  pca_data <- as.data.frame(pca_fit$x)
  train_test_c_impute = pca_data

  # split in train and test
  training_train_impute = data.frame(head(train_test_c_impute, n = nrow(training_train_impute)), row.names = seq(nrow(training_train_impute)))
  training_test_impute = data.frame(tail(train_test_c_impute, n = nrow(training_test_impute)), row.names = seq(nrow(training_test_impute)))
}

# remove cols with var = 0 and target cols
training_train_impute = training_train_impute[, !colnames(training_train_impute) %in% c("Per90_mean_Center.Backs_Straight.Corner.Kicks", "X2", "league", "Outcome", "Score")]
training_test_impute = training_test_impute[, !colnames(training_test_impute) %in% c("Per90_mean_Center.Backs_Straight.Corner.Kicks", "X2", "league", "Outcome", "Score")]

### Random Forest
# train
Outcome_model = randomForest::randomForest(x = training_train_impute, y = Outcome_train)

# predict
predictions = predict(Outcome_model, training_test_impute)

# results
result_RF = data.frame(Outcome = Outcome_test, prediction = predictions)

# compute accuracy
accuracy = mean(Outcome_test == result_RF$prediction)
print(paste0("RF: ", accuracy))


# important features for prediction
#Conditional=True, adjusts for correlations between predictors.
  i_scores = caret::varImp(Outcome_model, conditional=TRUE)
  i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]
  # write
  write.csv(i_scores_sorted, file = paste0("./Estimation/", country, "_estimate_", season_end_year, "/i_scores_RF.csv"))


### Regression
# train
Outcome_model = randomForest::randomForest(x = training_train_impute, y = Score_train)

# predict
predictions = predict(Outcome_model, as.matrix(training_test_impute))

# results
result_RF_regression = data.frame(Outcome = Score_test, prediction = predictions)

# metric
metric = Metrics::rmse(Score_test, result_RF_regression$prediction)
print(paste0("test-rmse: ", metric))

lm_eqn <- function(df){
  m <- lm(Score ~ Score_RF_regression, df);
  eq <- substitute(italic(Score) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

results_df = data.frame(Score_RF_regression = result_RF_regression$prediction,
                        Score = Score_test)

ggplot2::ggplot(data = results_df, ggplot2::aes(Score_RF_regression, Score))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = "lm")+
  ggplot2::geom_text(x = -1, y = 2.5, label = lm_eqn(results_df), parse = TRUE)

ggplot2::ggsave(paste0("./Estimation/", country, "_estimate_", season_end_year, "/Score.jpg"))

### naivebayes
# train
Outcome_model = naivebayes::naive_bayes(Outcome_train ~ ., data = training_train_impute, usekernel = T)
# predict
predictions = predict(Outcome_model, as.matrix(training_test_impute))

# results
result_naivebayes = data.frame(Outcome = Outcome_test, prediction = predictions)

# compute accuracy
accuracy = mean(as.vector(Outcome_test) == as.vector(result_naivebayes$prediction))
print(paste0("naivebayes: ", accuracy))

# Write ground truth
ground_truth = data.frame(Outcome = Outcome_test, Score = Score_test)

### combine
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
  theme(axis.text.x = element_text(angle = 90))+
  ylab("percent profit")+
  theme(axis.title.x = element_blank())
ggsave("./Estimation/win_leagues.jpg", width = 15, height = 15, units = "cm")
