split_df <- function(tidyDF,split_prop){
  set.seed(1982)  # for reproducibility
  split <- rsample::initial_split(tidyDF, prop = split_prop, strata = "VPD_F")
  train <- rsample::training(split)
  test <- rsample::testing(split)
  return(list("train" = train, "test" = test))
}