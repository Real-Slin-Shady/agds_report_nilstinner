eval_model <- function(mod, df_train, df_test,out){
  
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
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()
  
  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()
  
  
  plot_3 <- ggplot(data = df_train, aes(TIMESTAMP, fitted-GPP_NT_VUT_REF)) +
    geom_point(alpha = 0.3) +
    stat_smooth(method="loess",formula = 'y ~ x', na.rm=TRUE)+
    theme_classic()
  
  plot_4 <- ggplot(data = df_test, aes(TIMESTAMP, fitted-GPP_NT_VUT_REF)) +
    geom_point(alpha = 0.3) +
    stat_smooth(method="loess",formula = 'y ~ x', na.rm=TRUE)+
    theme_classic()
  
  
  if (out == "plot_all"){return(cowplot::plot_grid(plot_1, plot_2, plot_3,plot_4))}else if(out =="plot"){
    return(cowplot::plot_grid(plot_1, plot_2))
  }else if(out == "r.squared"){
    return(list("rsq_train" = rsq_train,"rsq_test" = rsq_test))
  }
  return()
}