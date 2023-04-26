#Raw implementation of stepwise forward Regression
# Package names
packages <- c("lubridate","tidyverse","visdat","yardstick","purrr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}else{
  print("All packages sucessfully installed")
}

invisible(lapply(packages, library, character.only = TRUE))

hh_fluxes<- read_csv("./data/halfhourly_data.csv")

vis_miss(
  hh_fluxes,
  cluster = FALSE, 
  warn_large_data = FALSE
)


unused_predictors <-
  hh_fluxes|>
  select(-siteid,
         -TIMESTAMP,
         -GPP_NT_VUT_REF)|>
  colnames()#Save unused predictors to a  vector, initallly all...

ACI_condition <- TRUE #Condition for loop
old_modelACI <- Inf #Initial state of ACI value...
Used_Predictors = NULL # Vector to later save the used predictor values
models = list()

while (ACI_condition) {
  models_temp <-  list() #overriding to save models for each run
  
  for (predictor in unused_predictors) {
    local_predictors <- unlist(c(Used_Predictors,predictor))#Somehow makes a list, needed as a vector so unlist
    local_predictors <- paste(local_predictors,collapse = " + ") #Now collapse vector
    formula_temp <- as.formula(paste("GPP_NT_VUT_REF~", local_predictors)) #temporary formula
    models_temp[[local_predictors]]<- lm(formula_temp,data = hh_fluxes) #Save models to appripriate place
    #The name is used for later referral
  }
  
  r_squared <- purrr::map(models_temp,~summary(.)$r.squared) #Here summary for each model+name is created
  
  predictor_rsquared <- r_squared[which.max(r_squared)] #Best one is chosen
  
  #remove used predictors from pool:
  Used_Predictors = strsplit(names(predictor_rsquared),split=' + ', fixed=TRUE) #separate the used predictors
  unused_predictors <- unused_predictors[!(unused_predictors %in% Used_Predictors)] 
  
  modelACI <- extractAIC(models_temp[[which.max(r_squared)]])[2]#Extraction of ACI
  
  
  
  if (modelACI > old_modelACI) { #Checking for improved ACI value...
    ACI_condition <- FALSE
    print("Terminating since lower ACI with new predictors")
    
  }else{
    old_modelACI <- modelACI #Replacing new one with old because check passed
    models[[names(predictor_rsquared)]] <- models_temp[[which.max(r_squared)]]}#Saving Model for later visualization
  
  
  
  
  #Output
  print("name:")
  print(names(predictor_rsquared))
  print("r-squared:")
  print(predictor_rsquared[[1]])
  print("ACI of these predictors: ")
  print(modelACI)
  cat("\n\n\n")
}
