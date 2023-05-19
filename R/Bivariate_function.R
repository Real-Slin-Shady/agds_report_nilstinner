#Models all bivariate models of a linear regression for all variables in a df except the target and manually removed rows.

bivariate_regressions<-function(tidydf,toPredict,removeColumns = NULL){
  all_predictors <-
    tidydf|>
    select(-toPredict,
           -all_of(removeColumns))|>
    colnames() #manage predictors
  
  models <-  list() #setup for loop
  plots <- list()
  
  for (predictor in all_predictors) {
    temp <- as.formula(paste(toPredict,"~", paste(predictor, collapse="+"))) #variable form for later implementation
    models[[predictor]]<- lm(temp,data = hh_fluxes) #managing models in models list..
    
  }#each predictor is used and then checked for correlation to the variable to predict...
  
  r_squared <- purrr::map(models,~summary(.)$r.squared) #get all r_squared values of the models...
  print(r_squared) #quick output...
  
  
  
  
  return(models) 
}