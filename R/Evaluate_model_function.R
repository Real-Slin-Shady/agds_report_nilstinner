# Function definitions -------------------
 # Evaluates Model
# Function that evaluates models based on the tow given datasets (usually train and test datasets). Further parameters are the output names for these two datasets.
# Aditionally a plot containing also the time dimension of the data can be plotted if this is chosen. This is done with the out parameter, which can also return the plots
# as a list or the R2 values from the evaluation.
#Function may be overloaded but works for the excerises...

eval_model <- function(mod, df_train, df_test,out = "plot",plot_name1 = "Training set",plot_name2 = "Test set"){
  
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  

  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  mae_train <- metrics_train |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  mae_test <- metrics_test |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = plot_name1) +
    theme_classic()
  
  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = plot_name2) +
    theme_classic()
  
  #visualize the time dimension of the residuals
  plot_3 <- ggplot(data = df_train, aes(TIMESTAMP, fitted-GPP_NT_VUT_REF)) +
    geom_point(alpha = 0.3) +
    stat_smooth(method="loess",formula = 'y ~ x', na.rm=TRUE)+
    labs(x = "Date",y = "Residuals", subtitle = paste("Bias:", round(mean(df_train$fitted- df_train$GPP_NT_VUT_REF),4)))+
    geom_hline(yintercept=0, linetype="dashed", 
               color = "red", linewidth=0.5)+
    theme_classic()
  
  plot_4 <- ggplot(data = df_test, aes(TIMESTAMP, fitted-GPP_NT_VUT_REF)) +
    geom_point(alpha = 0.3) +
    stat_smooth(method="loess",formula = 'y ~ x', na.rm=TRUE)+
    labs(x = "Date",y = "Residuals", subtitle = paste("Bias: " ,round(mean(df_test$fitted-df_test$GPP_NT_VUT_REF),4)))+ 
    geom_hline(yintercept=0, linetype="dashed", 
               color = "red", linewidth=0.5)+
    theme_classic()
  
  
  if (out == "plot_all"){return(cowplot::plot_grid(plot_1, plot_2, plot_3,plot_4))}else if(out =="plot"){
    return(cowplot::plot_grid(plot_1, plot_2))
  }else if(out == "r.squared"){
    return(list("rsq_train" = rsq_train,"rsq_test" = rsq_test))
  }else if(out == "return_plots"){
    return(list("plot_1" = plot_1,"plot_2" = plot_2))
  }else if(out == "rmse"){
    return(list("rmse_train" = rmse_train,"rmse_test" = rmse_test))
  }else if(out == "mae"){
    return(list("mae_train" = mae_train,"mae_test" = mae_test))
    }
}
