# Function definitions -------------------
#Takes a dataset and split it into two datasets of a specified proportion.
#Returns the datasets as a list.

split_df <- function(tidyDF,split_prop){
  set.seed(1982)  # for reproducibility
  split <- rsample::initial_split(tidyDF, prop = split_prop, strata = "VPD_F") #quite specific, may be better to pass onto function?
  train <- rsample::training(split)
  test <- rsample::testing(split)
  return(list("train" = train, "test" = test))
}