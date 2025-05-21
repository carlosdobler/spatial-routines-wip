# Based on this: https://bookdown.org/max/FES/recursive-feature-elimination.html
# Important points: Removing features makes sense. Doing it recursively solves issues with multicollinearity. Subsetting features should be done with the analysis subset, not the whole training data (see fig. 11.6)
# Nested structure: https://bookdown.org/max/FES/selection-overfitting.html
# (Subsetting features *inside* the tuning/cross-validation run)


# df is a data frame with 107 predictors and 33 observations

# resamples
set.seed(1)
folds <- vfold_cv(df, v = 6) # should be training data

# specification
spec_nn <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")

# tuning parameters
grid <- 
  spec_nn |> 
  extract_parameter_set_dials() |> 
  update(hidden_units = hidden_units(c(1,8))) |> 
  grid_regular(levels = 4) |> 
  mutate(param_set = row_number())


result <-
pmap_dfr(folds, \(splits, id) {
  
  message(str_glue("{id}"))

  # each iteration, use a smaller number of predictors
  map_dfr(107:5, \(num_preds){
    
    message(str_glue("  {num_preds}"))

    # each iteration, try a combination of parameters from "grid"
    future_pmap_dfr(grid, \(hidden_units, penalty, epochs, ...){  # use param_set column to id

      # specification
      spec <- 
        mlp(hidden_units = hidden_units, 
            penalty = penalty, 
            epochs = epochs) %>%
        set_engine("nnet") %>%
        set_mode("classification")

      # initialize variables
      vars <- names(df |> select(-Value))
      
      # loop through the number of predictors to remove
      for(i in seq(108-num_preds)){
        
        f_vi <- 
          workflow() |> 
          add_model(spec) |> 
          add_variables(outcomes = Value, predictors = all_of(vars)) |> 
          # fit(splits |> analysis()) # should be done with the analysis set
          fit(df)

        # get variables' importance and remove the last one
        vars <- 
          vi(f_vi, type = "olden") |> 
          head(-1) |> 
          pull(Variable)
        
      }
      
      # fit the model with the final subset of variables
      f <- 
        workflow() |> 
        add_model(spec) |> 
        add_variables(outcomes = Value, predictors = all_of(vars)) |> 
        fit(splits |> analysis())

      # assess results
      tb_assess <- 
        tibble(truth = splits |> assessment() |> pull(Value), 
               estimate_class = predict(f, splits |> assessment(), type = "class") |> pull(),
               estimate_prob = predict(f, splits |> assessment(), type = "prob") |> pull()) 

      # ensemble final table
      bind_rows(accuracy(tb_assess, truth, estimate_class),
                roc_auc(tb_assess, truth, estimate_prob)) |> 
        mutate(hidden_units = hidden_units,
               penalty = penalty,
               epochs = epochs,
               num_preds = num_preds,
               fold = id)
      
    })
  }) |> 
    write_rds(str_glue("tb_fold{id}"))
})
