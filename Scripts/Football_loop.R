library(worldfootballR)
library(tidyverse)
library(dplyr)
library(parallel)

## Leagure URLs

## get match URLs
# fix if error
#match_urls = as.vector(httr::GET(worldfootballR::fb_match_urls(country = "GER", gender = "M", tier = "1st", season_end_year = "2023")[1])$url)
match_urls = worldfootballR::fb_match_urls(country = "GER", gender = "M", tier = "1st", season_end_year = "2023")


################################################### START LOOP
lapply(match_urls,
                   function(z) {


# get lineup for matches
lineups = worldfootballR::fb_match_lineups(match_url = z, time_pause = 2)

# match results
match_info = worldfootballR::fb_match_results(country = "GER", gender = "M", tier = "1st", season_end_year = "2023")

# Rename Borussia Mönchengladbach (if present)
match_info = data.frame(lapply(match_info, function(x) {
                     gsub("M'Gladbach", "Mönchengladbach", x)
                 }))

# make in match_info the Wk col numeric
match_info[,c("Wk", "HomeGoals", "Home_xG", "AwayGoals", "Away_xG")] <- sapply(match_info[,c("Wk", "HomeGoals", "Home_xG", "AwayGoals", "Away_xG")], as.numeric)

# make outcome column
match_info$Outcome = ifelse(match_info$HomeGoals > match_info$AwayGoals, "win",
                            ifelse(match_info$HomeGoals < match_info$AwayGoals, "loss", "draw"))
# make numeric outcome column
match_info = mutate(.data = match_info, Score = HomeGoals - AwayGoals)

# subset to past matches
match_info_past = match_info[!as.vector(sapply(match_info$HomeGoals,is.na)), ]


## Player URLs

# get player urls from ALL players
mapped_players = player_dictionary_mapping()

# get player URLs ONLY for players of a certain match
mapped_players_match = mapped_players[mapped_players$PlayerFBref %in% lineups[lineups$Min > 30,]$Player_Name, ] # players must have played at least 30 minutes


## Player statistics
### Player statistics FBRef

# define function to scrape data from FBRef and skip if error occurs
scouting_FB_fun = function(x) {
  return(tryCatch(worldfootballR::fb_player_scouting_report(player_url = x, pos_versus = "primary", time_pause = 2), error=function(e) NULL))
}

# get statistics of players from certain match
## TAKES A LOT OF TIME
scouting_FB = lapply(mapped_players_match$UrlFBref, scouting_FB_fun)

# remove empty data frames from list (from players without FBRef scouting report)
scouting_FB_clean = scouting_FB[sapply(scouting_FB, function(x) !is.null(dim(x)[1]))]

# combine data frames
scouting_FB_c = plyr::rbind.fill(scouting_FB_clean)

# keep only information on last 365 days
scouting_FB_365 = scouting_FB_c %>% filter(scouting_period == "Last 365 Days")

# filter out some cols
scouting_FB_365 = select(scouting_FB_365, -c("StatGroup", "scouting_period"))


### Player statistics TM

# statistics of Players from TM
scouting_TM = sapply(mapped_players_match$UrlTmarkt, function(x) data.frame(worldfootballR::tm_player_bio(player_urls = x)))

# combine data frames
scouting_TM_c = plyr::rbind.fill(scouting_TM)

# rename player col
names(scouting_TM_c)[names(scouting_TM_c) == 'player_name'] <- 'Player'

# drop uninformative cols
scouting_TM_c = dplyr::select(scouting_TM_c, c("Player", "age", "height", "current_club", "player_valuation", "max_player_valuation"))


### Player statistics: join different data sources (FBRef and TM)

## Solve problem that some players are named slightly different across data bases
# pattern matching to find similar names between data sources
similar_names = sapply(scouting_FB_365$Player, function(x)
  sapply(scouting_TM_c$Player, function(y) agrepl(x, y , max.distance=0.1)))

# find first true instance
similar_names_match = apply(similar_names,1, function(x) min(which(x == TRUE)))

# get colnames of first true instance
FBRef_Player_names = as.vector(colnames(similar_names[,as.vector(similar_names_match)]))

# replace TM player names with FBRef player names
scouting_TM_c$Player = FBRef_Player_names

# drop row in scouting_TM_c when NA in Player column
scouting_TM_c = scouting_TM_c[!as.vector(sapply(scouting_TM_c$Player,is.na)), ]

# join TM and FBRef info
scouting_FB_TM = left_join(scouting_FB_365, scouting_TM_c)

# Remove players which had no proper match between TM and FBRef
scouting_FB_TM = na.omit(scouting_FB_TM)

## Remove players whose current club is a minority team
# retrieve team names from current match
club_filter = names(sort(table(scouting_FB_TM$current_club), decreasing = T))[1:2]

# drop players which are not longer players of clubs that participated in a match
scouting_FB_TM_clubs = scouting_FB_TM[scouting_FB_TM$current_club %in% club_filter, ]


## Pivot

# make age numeric
scouting_FB_TM_clubs$age = sapply(scouting_FB_TM_clubs$age, as.numeric)

# group by club, by age and by Statistic
group = scouting_FB_TM_clubs %>%
  dplyr::group_by(current_club, Versus, Statistic) %>%
  dplyr::summarise(Per90_mean = mean(Per90),
                   Percentile_mean = mean(Percentile),
                   BasedOnMinutes_mean = mean(BasedOnMinutes),
                   age_mean = mean(age),
                   height_mean = mean(height),
                   player_valuation_mean = mean(player_valuation),
                   max_player_valuation_mean = mean(max_player_valuation))
group = ungroup(group)

# this is how to make the df wide
group_wide = pivot_wider(data = group, names_from = c(Versus, Statistic), values_from = c(4:ncol(group)))



## Team statistics

# get statistics
team_stat = worldfootballR::fb_season_team_stats(country = "GER", gender = "M", season_end_year = "2023", tier = "1st", time_pause = 2, stat_type = "standard")

# filter out opponent data
team_stat = team_stat[team_stat$Team_or_Opponent == "team", ]

# Rename Borussia Mönchengladbach (if present)
team_stat$Squad[team_stat$Squad == "M'Gladbach"] <- "Mönchengladbach"

## Solve problem that some teams are named slightly different across data bases
# pattern matching to find similar names between data sources
similar_team_names = sapply(team_stat$Squad, function(x)
  sapply(group_wide$current_club, function(y) agrepl(x, y , max.distance=0.2)))

# find first true instance
similar_teams_match = apply(similar_team_names,1, function(x) min(which(x == TRUE)))

# get colnames of first true instance
FBRef_team_names = as.vector(colnames(similar_team_names[,as.vector(similar_teams_match)]))

# replace TM player names with FBRef player names
group_wide$Squad = FBRef_team_names

# join team stats to group_wide
group_wide_team = dplyr::left_join(group_wide, team_stat)

# add H/A
group_wide_team$Home = ifelse(match_info_past[which(match_info_past$MatchURL %in% z),]$Home == group_wide_team$Squad, 1, 2)


### TEAM 1 INFO
match_info_past_team1 = apply(match_info_past[,c("Home", "Away")], 1, function(x) x == group_wide_team$Squad[1])

# find rows in which team1 played
matchID_team1 = which(apply(match_info_past_team1,2,function(x) sum(x) > 0))

# subset match info by team1 games
match_info_subset1 = match_info_past[matchID_team1,]

# split in home and away
match_info_subset1_home = match_info_subset1[match_info_subset1$Home %in% group_wide_team$Squad[1],]
match_info_subset1_away = match_info_subset1[match_info_subset1$Away %in% group_wide_team$Squad[1],]

# home performance
match_info_subset1_home$Points = ifelse(match_info_subset1_home$HomeGoals > match_info_subset1_home$AwayGoals, 3,
                                        ifelse(match_info_subset1_home$HomeGoals < match_info_subset1_home$AwayGoals, 0, 1))
match_info_subset1_home = match_info_subset1_home[order(as.Date(match_info_subset1_home$Date, format="%Y-%m-%d")),]

# away performance
match_info_subset1_away$Points = ifelse(match_info_subset1_away$HomeGoals < match_info_subset1_away$AwayGoals, 3,
                                        ifelse(match_info_subset1_away$HomeGoals > match_info_subset1_away$AwayGoals, 0, 1))
match_info_subset1_away = match_info_subset1_away[order(as.Date(match_info_subset1_away$Date, format="%Y-%m-%d")),]

# bind rows
match_info_subset1_c = bind_rows(match_info_subset1_home,match_info_subset1_away)
match_info_subset1_c = match_info_subset1_c[order(as.Date(match_info_subset1_c$Date, format="%Y-%m-%d")),]

# add points
team1_info = data.frame(Squad = group_wide_team$Squad[1],
                        Acc_points = sum(match_info_subset1_c$Points),
                        Acc_points_away = sum(match_info_subset1_away$Points),
                        Acc_points_home = sum(match_info_subset1_home$Points),
                        Last_match = if(which(match_info_subset1_c$MatchURL == z) == 1) {
                          NA
                        } else {
                          match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 1, "Points"]
                        },
                        Last_2_match = if(which(match_info_subset1_c$MatchURL == z) <= 2) {
                          NA
                        } else {
                          sum(match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 1, "Points"],
                              match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 2, "Points"])
                        },
                        Last_3_match = if(which(match_info_subset1_c$MatchURL == z) <= 3) {
                          NA
                        } else {
                          sum(match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 1, "Points"],
                              match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 2, "Points"],
                              match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 3, "Points"])

                        },
                        Last_4_match = if(which(match_info_subset1_c$MatchURL == z) <= 4) {
                          NA
                        } else {
                          sum(match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 1, "Points"],
                              match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 2, "Points"],
                              match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 3, "Points"],
                              match_info_subset1_c[which(match_info_subset1_c$MatchURL == z) - 4, "Points"])

                        }
)

### TEAM 2 INFO
match_info_past_team2 = apply(match_info_past[,c("Home", "Away")], 1, function(x) x == group_wide_team$Squad[2])

# find rows in which team2 played
matchID_team2 = which(apply(match_info_past_team2,2,function(x) sum(x) > 0))

# subset match info by team2 games
match_info_subset2 = match_info_past[matchID_team2,]

# split in home and away
match_info_subset2_home = match_info_subset2[match_info_subset2$Home %in% group_wide_team$Squad[2],]
match_info_subset2_away = match_info_subset2[match_info_subset2$Away %in% group_wide_team$Squad[2],]

# home performance
match_info_subset2_home$Points = ifelse(match_info_subset2_home$HomeGoals > match_info_subset2_home$AwayGoals, 3,
                                        ifelse(match_info_subset2_home$HomeGoals < match_info_subset2_home$AwayGoals, 0, 1))
match_info_subset2_home = match_info_subset2_home[order(as.Date(match_info_subset2_home$Date, format="%Y-%m-%d")),]

# away performance
match_info_subset2_away$Points = ifelse(match_info_subset2_away$HomeGoals < match_info_subset2_away$AwayGoals, 3,
                                        ifelse(match_info_subset2_away$HomeGoals > match_info_subset2_away$AwayGoals, 0, 1))
match_info_subset2_away = match_info_subset2_away[order(as.Date(match_info_subset2_away$Date, format="%Y-%m-%d")),]

# bind rows
match_info_subset2_c = bind_rows(match_info_subset2_home,match_info_subset2_away)
match_info_subset2_c = match_info_subset2_c[order(as.Date(match_info_subset2_c$Date, format="%Y-%m-%d")),]

# add points
team2_info = data.frame(Squad = group_wide_team$Squad[2],
                        Acc_points = sum(match_info_subset2_c$Points),
                        Acc_points_away = sum(match_info_subset2_away$Points),
                        Acc_points_home = sum(match_info_subset2_home$Points),
                        Last_match = if(which(match_info_subset2_c$MatchURL == z) == 1) {
                          NA
                        } else {
                          match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 1, "Points"]
                        },
                        Last_2_match = if(which(match_info_subset2_c$MatchURL == z) <= 2) {
                          NA
                        } else {
                          sum(match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 1, "Points"],
                              match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 2, "Points"])
                        },
                        Last_3_match = if(which(match_info_subset2_c$MatchURL == z) <= 3) {
                          NA
                        } else {
                          sum(match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 1, "Points"],
                              match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 2, "Points"],
                              match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 3, "Points"])

                        },
                        Last_4_match = if(which(match_info_subset2_c$MatchURL == z) <= 4) {
                          NA
                        } else {
                          sum(match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 1, "Points"],
                              match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 2, "Points"],
                              match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 3, "Points"],
                              match_info_subset2_c[which(match_info_subset2_c$MatchURL == z) - 4, "Points"])

                        }
)
# combine
team_info = rbind(team1_info, team2_info)

# join with group_wide_team
group_wide_team_performance = dplyr::left_join(team_info, group_wide_team)

# drop unnessecary cols
group_wide_team_num = dplyr::select(group_wide_team_performance, -c("current_club", "Squad", "Competition_Name", "Gender", "Country", "Season_End_Year", "Team_or_Opponent"))

# make all cols num except the last
group_wide_team_num <- sapply(group_wide_team_num, as.numeric)

# add small constant to all values to make sure that no divisions of 0 cause errors
group_wide_team_num_constant = group_wide_team_num
group_wide_team_num_constant = apply(group_wide_team_num, 2, function(x) x + 0.01)

# divide row with Home == 1 by row with Home == 2
division = group_wide_team_num_constant[which(group_wide_team_num_constant$Home == 1),]/group_wide_team_num_constant[which(group_wide_team_num_constant$Home == 2),]

# add GT
division$Outcome = match_info_past$Outcome[which(match_info_past$MatchURL %in% z)]
division$Score = match_info_past$Score[which(match_info_past$MatchURL %in% z)]

# add url as identifier
division$MatchURL = z

# write df
write.csv(x = division, file = paste0("./Output/Training_data/", stringr::str_remove(z, pattern = ".*/"), ".csv"))

# print some output
print(stringr::str_remove(z, pattern = ".*/"))

################################################### END LOOP
                   })

# read data
temp = list.files(path = "./Output/Training_data/" ,pattern="*.csv")
training = lapply(paste0("./Output/Training_data/", temp), read.csv)

# combine data
training_c = plyr::rbind.fill(training)

# drop unnecessary cols
training_c = dplyr::select(.data = training_c, -c("X", "MatchURL", "Score"))

# convert Outcome to factor
training_c$Outcome = as.factor(training_c$Outcome)

# select randomly 70% of rows
training_train = training_c[sample(nrow(training_c), round(0.7*nrow(training_c))), ]
subset = rownames(training_c) %in% rownames(training_train)
training_test = training_c[subset == FALSE,]

## impute missing values
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

# how balanced is the train and test set?
table(training_train_impute$Outcome)
table(training_test_impute$Outcome)

# train
Outcome = training_train_impute$Outcome
Outcome_model = randomForest(x = dplyr::select(training_train_impute, -"Outcome"), y = Outcome)
# predict
predictions <- predict(Outcome_model, training_test_impute)

# results
result = data.frame(Outcome = training_test_impute$Outcome, prediction = predictions)
table(result$Outcome == result$prediction)
accuracy = mean(result$Outcome == result$prediction)
accuracy

# important features for prediction
#Conditional=True, adjusts for correlations between predictors.
i_scores <- caret::varImp(Outcome_model, conditional=TRUE)
i_scores_sorted = i_scores[order(-i_scores$Overall), , drop = FALSE]
