# Logistic Regression
lm_model <- linear_reg() %>% 
  set_engine('lm')

lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(valRecipe)

lm_fit <- fit_resamples(lm_wflow, valFold)



# Lasso Regression
lasso_spec <- 
  linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("glmnet") 

lasso_workflow <- workflow() %>% 
  add_recipe(valRecipe) %>% 
  add_model(lasso_spec)

penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)

tune_res <- tune_grid(
  lasso_workflow,
  resamples = valFold, 
  grid = penalty_grid)

autoplot(tune_res)

best_penalty <- select_best(tune_res, metric = "rsq")

lasso_final <- finalize_workflow(lasso_workflow, best_penalty)

lasso_final_fit <- fit(lasso_final, data = valTrain)



# Decision Tree
reg_tree_spec <- tree_spec %>%
  set_mode("regression")

reg_tree_fit <- fit(reg_tree_spec, win_percent ~ .)



# Random Forest
rf_spec <- rand_forest(mtry = 6) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("regression")

rf_fit <- fit(rf_spec, win_percent ~ ., data = valTrain)



save(lm_fit, file = 'valFits.rda')

load(file = 'valFits.rda')




#how to reduce file appropriately
# valorant <- sample_n(valorant, size = any size I want)

#what interactions to make
# up to me

#difference between fit() and fit_resamples()
# fits onto 1 thing

#do I only have to change the specification to regression
# yes?

# Am I allowed to go through the project with a small sample and then go to a larger size when I'm done?
# Yes

# What is the minimum number of observations to use
# Don't go below 150

# Running lm_fit gives message 'Fold1: preprocessor 1/1: Column(s) have zero variance so scaling cannot be used: `agent_1_Phoenix`, `agent_2_...' is that a problem?
# step_other() - any predictors that are catergorical variables that have a lot of levels
# step_nzv() - apply to all predictors
# step_novel() - apply to predictors like agent_1_name (something about not seeing Phoenix before?)

# step_pca
# list the variables highly positively correlated with each other (2 or 3 number of compositions)