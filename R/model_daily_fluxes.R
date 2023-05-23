# Function definitions -------------------

#Function Modelling daily fluxes of dataset with fluxes and biofluxes.
#Can differentiate between methods lm and knn with also parameters to specify the splitratio and the k of KNN if needed.
#returns the model

model_daily_fluxes <- function(trainDF,testDF,method_loc,kofKNN = 0){
# Data splitting


source("../R/Recipe_GPP.R")

pp <- recipe_GPP(trainDF)


if(method_loc =="knn"){
# Fit KNN model
mod <- caret::train(
  pp, 
  data = trainDF |> drop_na(), 
  method = method_loc,
  trControl = caret::trainControl(method = "none"),
  tuneGrid = data.frame(k = kofKNN),
  metric = "RMSE")
} else if(method_loc == "lm"){ #if model is lm
  mod <- caret::train(
    pp, 
    data = trainDF |> drop_na(), 
    method = method_loc,
    trControl = caret::trainControl(method = "none"),
    metric = "RMSE"
  )
}


return(mod)
}