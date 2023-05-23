# Function definitions -------------------

#Manual stepwise forward regression.
#Takes a tidy dataframe and a variable to predict and can take some columns of the dataframe that should not be used as predictors.
#Strictly follows the specified algorithm of the GECO-Group of the university of Bern
#Returns all models as a list.
Stepwiseforward_tidy <- function(TidyDF,toPredict,removeColumns = NULL){
  unused_predictors <-
    hh_fluxes|>
    select(-all_of(toPredict),
           -all_of(removeColumns))|>
    colnames()#Save unused predictors to a  vector, initallly all...
  
  ACI_condition <- TRUE #Condition for loop
  old_modelACI <- Inf #Initial state of ACI value...
  Used_Predictors = NULL # Vector to later save the used predictor values
  models = list() #list to manage the best models...
  
  while (ACI_condition) {
    models_temp <-  list() #overriding to save models for each run
    
    for (predictor in unused_predictors) {
      local_predictors <- unlist(c(Used_Predictors,predictor))#Somehow makes a list, needed as a vector so unlist
      local_predictors <- paste(local_predictors,collapse = " + ") #Now collapse vector
      formula_temp <- as.formula(paste(toPredict,"~", local_predictors)) #temporary formula
      models_temp[[local_predictors]]<- lm(formula_temp,data = hh_fluxes) #Save models to appripriate place
      #The name is used for later referral
    }
    
    r_squared <- purrr::map(models_temp,~summary(.)$r.squared) #Here summary for each model+name is created
    predictor_rsquared <- r_squared[which.max(r_squared)] #Best one is chosen
    #remove used predictors from pool:
    Used_Predictors = strsplit(names(predictor_rsquared[1]),split=' + ', fixed=TRUE) #separate the used predictors
    unused_predictors <- unused_predictors[!unused_predictors %in% Used_Predictors[[1]]] 
    modelACI <- extractAIC(models_temp[[which.max(r_squared)]])[2]#Extraction of ACI
    if (modelACI >= old_modelACI) { #Checking for improved ACI value...
      ACI_condition <- FALSE
      print("Terminate since AIC criterion")
    }else{
      old_modelACI <- modelACI #Replacing new one with old because check passed
      models[[names(predictor_rsquared)]] <- models_temp[[which.max(r_squared)]]}#Saving Model for later visualization
    
  if (identical(unused_predictors, character(0))) {#Additional backup solution to terminate if all predictors were used...
    ACI_condition <- FALSE
    print("Terminate since all predictors used")
  }
    
    #UI-output
    print("name:")
    print(names(predictor_rsquared))
    print("r-squared:")
    print(predictor_rsquared[[1]])
    print("ACI of these predictors: ")
    print(modelACI)
    cat("\n\n\n")
  }
  return(models) #returns only models that met the criterion.
}