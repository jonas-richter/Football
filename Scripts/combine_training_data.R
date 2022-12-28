combine_training_data = function(categorical = TRUE,
                              path = "./Output/Training_data/"){

  # read data
  temp = list.files(path = path ,pattern="*.csv")
  training = lapply(paste0(path, temp), read.csv)

  # combine data
  training_c = plyr::rbind.fill(training)

  if(isTRUE(categorical)){
    # drop unnecessary cols
    training_c = dplyr::select(.data = training_c, -c("X", "MatchURL", "Score"))

    # convert Outcome to factor
    training_c$Outcome = as.factor(training_c$Outcome)
  } else {
    # drop unnecessary cols
    training_c = dplyr::select(.data = training_c, -c("X", "MatchURL", "Outcome"))
  }
  # create new folder
  dir.create(path = paste0(path, '/ground_truth_combined'))

  # write combined training data
  write.csv(training_c, file = paste0(path, '/ground_truth_combined/training_combined.csv'))
}
