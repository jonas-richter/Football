train_test_split = function(path = "./Output/Training_data/",
                            ratio = 0.8){

  # read ground truth data
  training_c = read.csv(file = paste0(path, '/ground_truth_combined/training_combined.csv'))


  # select randomly x% of rows
  training_train = training_c[sample(nrow(training_c), round(ratio*nrow(training_c))), ]
  subset = rownames(training_c) %in% rownames(training_train)
  training_test = training_c[subset == FALSE,]

  # create new folder
  dir.create(path = paste0(path, '/train_test_split'))

  # write
  write.csv(training_train, file = paste0(path, '/train_test_split/GT_train.csv'))
  write.csv(training_test, file = paste0(path, '/train_test_split/GT_test.csv'))
}
