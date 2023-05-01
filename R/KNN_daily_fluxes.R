
KNN_daily_fluxes <- function(tidyDF,kofKNN,split_prop){
# Data splitting
set.seed(123)  # for reproducibility
split <- rsample::initial_split(daily_fluxes, prop = split_prop, strata = "VPD_F")
daily_fluxes_train <- rsample::training(split)
daily_fluxes_test <- rsample::testing(split)

# Model and pre-processing formulation, use all variables but LW_IN_F
pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                      data = daily_fluxes_train |> drop_na()) |> 
  recipes::step_BoxCox(all_predictors()) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())


# Fit KNN model
mod_knn <- caret::train(
  pp, 
  data = daily_fluxes_train |> drop_na(), 
  method = "knn",
  trControl = caret::trainControl(method = "none"),
  tuneGrid = data.frame(k = kofKNN),
  metric = "RMSE"
)


return(list("mod_knn" = mod_knn,"daily_fluxes_train" = daily_fluxes_train,"daily_fluxes_test" = daily_fluxes_test))
}