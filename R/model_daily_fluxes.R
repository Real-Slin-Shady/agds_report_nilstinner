
model_daily_fluxes <- function(tidyDF,method_loc,kofKNN,split_prop){
# Data splitting
set.seed(1982)  # for reproducibility
split <- rsample::initial_split(daily_fluxes, prop = split_prop, strata = "VPD_F")
daily_fluxes_train <- rsample::training(split)
daily_fluxes_test <- rsample::testing(split)

# Model and pre-processing formulation, use all variables but LW_IN_F
pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                      data = daily_fluxes_train |> drop_na()) |> 
  recipes::step_BoxCox(c(all_predictors(),-TA_F)) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())



if(method_loc =="knn"){
# Fit KNN model
mod <- caret::train(
  pp, 
  data = daily_fluxes_train |> drop_na(), 
  method = method_loc,
  trControl = caret::trainControl(method = "none"),
  tuneGrid = data.frame(k = kofKNN),
  metric = "RMSE")
} else {
  mod <- caret::train(
    pp, 
    data = daily_fluxes_train |> drop_na(), 
    method = method_loc,
    trControl = caret::trainControl(method = "none"),
    metric = "RMSE"
  )
}


return(list("mod" = mod,"daily_fluxes_train" = daily_fluxes_train,"daily_fluxes_test" = daily_fluxes_test))
}