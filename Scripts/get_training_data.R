get_training_data = function(country,
                             sex,
                             tier,
                             season_end_year,
                             match_urls = NULL,
                             scouting_period,
                             path = "./Output"){

  # get scout_report function
  source("./Scripts/scout_report.R")

  # create dir
  dir.create(paste0(path, "/Training_data"))

  ## get match URLs
  if(is.null(match_urls)){
    match_urls = worldfootballR::fb_match_urls(country = country, gender = sex, tier = tier, season_end_year = season_end_year)
  }

  # remove match urls which are already in training data
  if(length(dir(paste0(path, "/Training_data/"))) > 0){
  # read data
  temp = list.files(path = paste0(path, "/Training_data/") ,pattern="*.csv")
  training = lapply(paste0(path, "/Training_data/", temp), read.csv)
  # combine data
  training_c = plyr::rbind.fill(training)
  # drop unnecessary cols
  training_c = dplyr::select(.data = training_c, -X)
  # remove URLs from match_urls which are already present in training data
  match_urls = match_urls[!(match_urls %in% training_c$MatchURL)]
  }

  ################################################### START LOOP
  lapply(match_urls,
         function(z) {

           # get lineup for matches
           lineups = worldfootballR::fb_match_lineups(match_url = z, time_pause = 2)

           # match results
           match_info = worldfootballR::fb_match_results(country = country, gender = sex, tier = tier, season_end_year = season_end_year)

           # Rename some teams (if present)
           match_info[,-which(colnames(match_info) %in% "MatchURL")] = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("M'Gladbach", "Mönchengladbach", x)
           }))
           match_info[,-which(colnames(match_info) %in% "MatchURL")]  = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("Nott'ham Forest", "Nottingham Forest", x)
           }))
           match_info[,-which(colnames(match_info) %in% "MatchURL")]  = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("Newcastle Utd", "Newcastle United", x)
           }))
           match_info[,-which(colnames(match_info) %in% "MatchURL")]  = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("Manchester Utd", "Manchester United", x)
           }))
           match_info[,-which(colnames(match_info) %in% "MatchURL")]  = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("Wolves", "Wolverhamptom Wanderers", x)
           }))
           match_info[,-which(colnames(match_info) %in% "MatchURL")]  = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("West Ham", "West Ham United", x)
           }))
           match_info[,-which(colnames(match_info) %in% "MatchURL")]  = data.frame(lapply(dplyr::select(match_info, -c("MatchURL")), function(x) {
             gsub("Tottenham", "Tottenham Hotspur", x)
           }))

           # make some cols in match_info numeric
           match_info[,c("Wk", "HomeGoals", "Home_xG", "AwayGoals", "Away_xG")] <- sapply(match_info[,c("Wk", "HomeGoals", "Home_xG", "AwayGoals", "Away_xG")], as.numeric)

           # make outcome column
           match_info$Outcome = ifelse(match_info$HomeGoals > match_info$AwayGoals, "win",
                                       ifelse(match_info$HomeGoals < match_info$AwayGoals, "loss", "draw"))
           # make numeric outcome column
           match_info = dplyr::mutate(.data = match_info, Score = HomeGoals - AwayGoals)

           # subset to past matches
           match_info_past = match_info[!as.vector(sapply(match_info$HomeGoals,is.na)), ]


           ## Player URLs

           # get player urls from ALL players
           mapped_players = worldfootballR::player_dictionary_mapping()

           # get player URLs ONLY for players of a certain match
           mapped_players_match = mapped_players[mapped_players$PlayerFBref %in% lineups[lineups$Min > 30,]$Player_Name, ] # players must have played at least 30 minutes


           ## Player statistics
           ### Player statistics FBRef

           # define function to scrape data from FBRef and skip if error occurs
           scouting_FB_fun = function(x) {
             return(
               tryCatch(
                 scout_report(player_url = x, pos_versus = "primary", time_pause = 2),
                 error=function(e) NULL))
           }

           # get statistics of players from certain match
           ## TAKES A LOT OF TIME
           scouting_FB = lapply(mapped_players_match$UrlFBref, scouting_FB_fun)

           # remove empty data frames from list (from players without FBRef scouting report)
           scouting_FB_clean = scouting_FB[sapply(scouting_FB, function(x) !is.null(dim(x)[1]))]

           # combine data frames
           scouting_FB_c = plyr::rbind.fill(scouting_FB_clean)

           # keep only information on given scouting periode
           scouting_FB_365 = scouting_FB_c %>% filter(scouting_period == scouting_period)

           # join club information from lineups
           colnames(scouting_FB_365)[which(names(scouting_FB_365) == "Player")] <- "Player_Name"
           scouting_FB_365 = dplyr::left_join(scouting_FB_365, dplyr::select(lineups, c(Team, Player_Name)))

           # drop some cols
           scouting_FB_365 = dplyr::select(scouting_FB_365, -c("StatGroup", "scouting_period"))


           ### Player statistics TM

           # statistics of Players from TM
           scouting_TM = sapply(mapped_players_match$UrlTmarkt, function(x) data.frame(worldfootballR::tm_player_bio(player_urls = x)))

           # combine data frames
           scouting_TM_c = plyr::rbind.fill(scouting_TM)

           # rename player col
           names(scouting_TM_c)[names(scouting_TM_c) == 'player_name'] <- 'Player_Name'

           # drop uninformative cols
           scouting_TM_c = dplyr::select(scouting_TM_c, c("Player_Name", "age", "height", "player_valuation", "max_player_valuation"))


           ### Player statistics: join different data sources (FBRef and TM)

           ## Solve problem that some players are named slightly different across data bases
           # pattern matching to find similar names between data sources
           similar_names = sapply(scouting_FB_365$Player,
                                  function(x) sapply(scouting_TM_c$Player,
                                                     function(y) agrepl(x, y , max.distance=0.1)))

           # find first true instance
           similar_names_match = apply(similar_names,1, function(x) min(which(x == TRUE)))

           # get colnames of first true instance
           FBRef_Player_names = as.vector(colnames(similar_names[,as.vector(similar_names_match)]))

           # replace TM player names with FBRef player names
           scouting_TM_c$Player_Name = FBRef_Player_names

           # drop row in scouting_TM_c when NA in Player column
           scouting_TM_c = scouting_TM_c[!as.vector(sapply(scouting_TM_c$Player,is.na)), ]

           # join TM and FBRef info
           scouting_FB_TM = dplyr::left_join(scouting_FB_365, scouting_TM_c)

           # Remove players which had no proper match between TM and FBRef
           scouting_FB_TM_clubs = na.omit(scouting_FB_TM)

           ## Pivot

           # make age numeric
           scouting_FB_TM_clubs$age = sapply(scouting_FB_TM_clubs$age, as.numeric)

           # group by club, by age and by Statistic
           group = scouting_FB_TM_clubs %>%
             dplyr::group_by(Team, Versus, Statistic) %>%
             dplyr::summarise(Per90_mean = mean(Per90),
                              Percentile_mean = mean(Percentile),
                              BasedOnMinutes_mean = mean(BasedOnMinutes),
                              age_mean = mean(age),
                              height_mean = mean(height),
                              player_valuation_mean = mean(player_valuation),
                              max_player_valuation_mean = mean(max_player_valuation))
           group = dplyr::ungroup(group)

           # this is how to make the df wide
           group_wide = tidyr::pivot_wider(data = group, names_from = c(Versus, Statistic),
                                    values_from = c(4:ncol(group)))


           ## Team statistics

           # get statistics
           team_stat = worldfootballR::fb_season_team_stats(country = country,
                                                            gender = sex,
                                                            season_end_year = season_end_year,
                                                            tier = tier,
                                                            time_pause = 2,
                                                            stat_type = "standard")

           # filter out opponent data
           team_stat = team_stat[team_stat$Team_or_Opponent == "team", ]

           # Rename few teams (if present)
           team_stat$Squad[team_stat$Squad == "M'Gladbach"] <- "Mönchengladbach"
           team_stat$Squad[team_stat$Squad == "Nott'ham Forest"] <- "Nottingham Forest"
           team_stat$Squad[team_stat$Squad == "Tottenham"] <- "Tottenham Hotspur"
           team_stat$Squad[team_stat$Squad == "Newcastle Utd"] <- "Newcastle United"
           team_stat$Squad[team_stat$Squad == "Manchester Utd"] <- "Manchester United"
           team_stat$Squad[team_stat$Squad == "Wolves"] <- "Wolverhamptom Wanderers"
           team_stat$Squad[team_stat$Squad == "West Ham"] <- "West Ham United"

           ## Solve problem that some teams are named slightly different across data bases
           # pattern matching to find similar names between data sources
           if(country == "ENG"){
             similar_team_names = sapply(team_stat$Squad, function(x)
               sapply(group_wide$Team,
                      function(y) agrepl(x, y , max.distance=0.1)))
           }else{
             similar_team_names = sapply(team_stat$Squad, function(x)
               sapply(group_wide$Team,
                      function(y) agrepl(x, y , max.distance=0.2)))
           }


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
           match_info_subset1_c = dplyr::bind_rows(match_info_subset1_home,match_info_subset1_away)
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

           # join home info with team_info
           team_info_home = dplyr::left_join(team_info, dplyr::select(group_wide_team, c("Squad", "Home")))
           # subtract home - away
           team_info_subtract = team_info_home[which(team_info_home$Home == 1), -c(colnames(team_info_home) == "Squad")]-team_info_home[which(team_info_home$Home == 2),-c(colnames(team_info_home) == "Squad")]
           team_info_subtract = dplyr::select(team_info_subtract, -Home)

           # drop unnessecary cols
           group_wide_team_num = dplyr::select(group_wide_team, -c("Team", "Squad", "Competition_Name", "Gender", "Country", "Season_End_Year", "Team_or_Opponent"))

           # make all cols num
           group_wide_team_num <- sapply(group_wide_team_num, as.numeric)

           # add small constant to all values to make sure that no divisions of 0 cause errors
           group_wide_team_num_constant = group_wide_team_num
           group_wide_team_num_constant = apply(group_wide_team_num, 2, function(x) x + 0.01)
           group_wide_team_num_constant = as.data.frame(group_wide_team_num_constant)

           # divide row with Home == 1 by row with Home == 2
           division = group_wide_team_num_constant[which(group_wide_team_num_constant$Home == 1.01),]/group_wide_team_num_constant[which(group_wide_team_num_constant$Home == 2.01),]

           # add team_info_subtract
           division = cbind(team_info_subtract, division, 2)

           # add GT
           division$Outcome = match_info_past$Outcome[which(match_info_past$MatchURL %in% z)]
           division$Score = match_info_past$Score[which(match_info_past$MatchURL %in% z)]

           # add url as identifier
           division$MatchURL = z

           # add division and country identifier to df
           division$league = ifelse(country == "GER" & sex =="M" & tier =="1st", 1,
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
                                                                                                                               ifelse(country == "FRA" & sex =="M" & tier =="2nd", 15, 0)))))))))))))))

           # write df
           dir.create(path = paste0(path, "/Training_data"))
           write.csv(x = division, file = paste0("./Output/Training_data/", stringr::str_remove(z, pattern = ".*/"), ".csv"))

           # print some output
           print(stringr::str_remove(z, pattern = ".*/"))

           ################################################### END LOOP
         })
}
