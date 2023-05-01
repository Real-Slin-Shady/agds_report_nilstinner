bivariate_regressions<-function(tidydf){
  all_predictors <-
    tidydf|>
    select(-siteid,
           -TIMESTAMP,
           -GPP_NT_VUT_REF)|>
    colnames()
  
  models <-  list() #more elegant way?
  plots <- list()
  
  for (predictor in all_predictors) {
    temp <- as.formula(paste("GPP_NT_VUT_REF~", paste(predictor, collapse="+")))#Weird structure for later
    models[[predictor]]<- lm(temp,data = hh_fluxes) #same here
    
  }
  
  r_squared <- purrr::map(models,~summary(.)$r.squared)
  print(r_squared)
  
  
  
  
  return(models)
}