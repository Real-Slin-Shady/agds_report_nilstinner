bivariate_regressions<-function(tidydf,toPredict,removeColumns){
  all_predictors <-
    tidydf|>
    select(-toPredict,
           -all_of(removeColumns))|>
    colnames()
  
  models <-  list() #more elegant way?
  plots <- list()
  
  for (predictor in all_predictors) {
    temp <- as.formula(paste(toPredict,"~", paste(predictor, collapse="+")))#Weird structure for later
    models[[predictor]]<- lm(temp,data = hh_fluxes) #same here
    
  }
  
  r_squared <- purrr::map(models,~summary(.)$r.squared)
  print(r_squared)
  
  
  
  
  return(models)
}