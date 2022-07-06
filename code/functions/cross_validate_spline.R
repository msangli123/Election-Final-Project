##########################################################
# Run cross-validation to select the degrees of freedom 
# for a natural spline fit
#
# Inputs
# x:         vector of x coordinates in training data  
# y:         vector of y coordinates in training data
# nfolds:    number of folds for cross-validation
# df_values: vector of values of degrees of freedom to try
##########################################################
cross_validate_spline = function(x, y, nfolds, df_values){
  # a few checks of the inputs
  stopifnot(is.vector(x))
  stopifnot(is.vector(y))
  stopifnot(length(x) == length(y))
  
  # divide training data into folds
  n = length(x)
  train_data = tibble(x,y)
  folds = sample(rep(1:nfolds, length.out = n))
  train_data = train_data %>% mutate(fold = folds)
  
  # create a matrix for out-of-fold predictions
  num_df_values = length(df_values)
  out_of_fold_predictions = 
    matrix(0, n, num_df_values) %>% 
    as_tibble() %>%
    setNames(paste0('y_hat_', df_values))
  
  # iterate over folds
  for(current_fold in 1:nfolds){
    # out-of-fold data will be used for training
    out_of_fold_data = train_data %>% filter(fold != current_fold)
    # in-fold data will be used for validation
    in_fold_data = train_data %>% filter(fold == current_fold)
    
    # iterate over df
    for(i in 1:num_df_values){
      df = df_values[i]
      
      # train on out-of-fold data
      formula = sprintf("y ~ splines::ns(x, df = %d)", df)
      spline_fit = lm(formula = formula, data = out_of_fold_data)
      
      # predict on in-fold data
      out_of_fold_predictions[folds == current_fold, i] = 
        predict(spline_fit, newdata = in_fold_data) 
    }
  }
  
  # add the out-of-fold predictions to the data frame
  results = train_data %>% bind_cols(out_of_fold_predictions)
  results
  
  # compute the CV estimate and standard error
  cv_table = results %>%
    pivot_longer(-c(x,y,fold), 
                 names_to = "df",
                 names_prefix = "y_hat_",
                 names_transform = list(df = as.integer),
                 values_to = "yhat") %>%
    group_by(df, fold) %>%
    summarise(cv_fold = mean((yhat-y)^2)) %>%  # CV estimates per fold
    summarise(cv_mean = mean(cv_fold),
              cv_se = sd(cv_fold)/sqrt(nfolds))
  
  df.1se = cv_table %>% 
    filter(cv_mean-cv_se <= min(cv_mean)) %>% 
    summarise(min(df)) %>% 
    pull()
  
  df.min = cv_table %>% 
    filter(cv_mean == min(cv_mean)) %>% 
    summarise(min(df)) %>% 
    pull()
  
  # plot the results, along with the previously computed validation error
  cv_plot = cv_table %>%
    ggplot(aes(x = df, y = cv_mean, ymin = cv_mean-cv_se, ymax = cv_mean+cv_se)) +
    geom_point() + geom_line() + geom_errorbar() +
    geom_hline(aes(yintercept = min(cv_mean)), linetype = "dashed") +
    xlab("Degrees of freedom") + ylab("CV error") + 
    theme_bw()
  
  # return CV table and plot
  return(list(cv_table = cv_table, 
              cv_plot = cv_plot, 
              df.1se = df.1se, 
              df.min = df.min))
}
