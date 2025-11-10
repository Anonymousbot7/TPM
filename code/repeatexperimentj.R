source('readdata.R')

source('SingleGLM.R')


source('Twopartj.R')


j=4

thewholename=read.table('1.txt')[,1]
# list_significant_single_Gaussian=create_initial_cumulative_dict(thewholename)
list_significant_single_Gamma=create_initial_cumulative_dict(thewholename)
# list_significant_two_Gaussian_logistic=create_initial_cumulative_dict(thewholename)
# list_significant_two_Gaussian_GLM=create_initial_cumulative_dict(thewholename)
list_significant_two_Gamma_logistic=create_initial_cumulative_dict(thewholename)
list_significant_two_Gamma_GLM_zero=create_initial_cumulative_dict(thewholename)
list_significant_two_Gamma_GLM_nonzero=create_initial_cumulative_dict(thewholename)


list_significant_two_combined_GLM_zero_logistic=create_initial_cumulative_dict(thewholename)
list_significant_two_combined_GLM_nonzero_logistic=create_initial_cumulative_dict(thewholename)
list_significant_two_combined_GLM_zero_nonzero=create_initial_cumulative_dict(thewholename)


# MAE_single_Gaussian=c()
MAE_single_Gamma=c()
# RMSE_single_Gaussian=c()
RMSE_single_Gamma=c()
# Rsquare_single_Gaussian=c()
Rsquare_single_Gamma=c()





# MAE1_two_Gaussian=c()
MAE1_two_Gamma=c()
# RMSE1_two_Gaussian=c()
RMSE1_two_Gamma=c()
# Rsquare1_two_Gaussian=c()
Rsquare1_two_Gamma=c()

# Initialize counters: valid_iterations for successful runs and attempt_count overall.
valid_iterations <- 0
attempt_count  <- 0

# Loop until 400 valid iterations are obtained.
while (valid_iterations < 400) {
  attempt_count <- attempt_count + 1
  cat("Overall attempt:", attempt_count, "\n")
  
  # Wrap your per-iteration code in tryCatch.
  valid <- tryCatch({
    datalist <- split_train_test_stratified(mat, 3000)
    # datalist <- split_train_test(mat, 3000)
    train_set <- datalist[[1]]
    test_set  <- datalist[[2]]
      # table(train_set$Comb_Anc)
      # table(test_set$Comb_Anc)

    single_gamma   <- gamma_regression_with_categories(train_set, test_set, 0.05)
    two_gamma      <- two_part_with_categories_j(train_set, test_set, 0.05,thres=j)

    list_significant_single_Gamma <<- combine_significant_predictors(list_significant_single_Gamma, single_gamma$significant_X)



    list_significant_two_Gamma_logistic <<- combine_significant_predictors(list_significant_two_Gamma_logistic, two_gamma$significant_Xlogit)
    list_significant_two_Gamma_GLM_zero <<- combine_significant_predictors(list_significant_two_Gamma_GLM_zero, two_gamma$significant_XGLM_zero)
    list_significant_two_Gamma_GLM_nonzero <<- combine_significant_predictors(list_significant_two_Gamma_GLM_nonzero, two_gamma$significant_XGLM_nonzero)

    
  

    list_significant_two_combined_GLM_zero_logistic<<- combine_significant_predictors(list_significant_two_combined_GLM_zero_logistic,two_gamma$significant_XGLM_zero[two_gamma$significant_XGLM_zero%in% two_gamma$significant_Xlogit])
    list_significant_two_combined_GLM_nonzero_logistic<<- combine_significant_predictors(list_significant_two_combined_GLM_nonzero_logistic,two_gamma$significant_XGLM_nonzero[two_gamma$significant_XGLM_nonzero%in% two_gamma$significant_Xlogit])
    list_significant_two_combined_GLM_zero_nonzero<<- combine_significant_predictors(list_significant_two_combined_GLM_zero_nonzero,two_gamma$significant_XGLM_nonzero[two_gamma$significant_XGLM_nonzero%in% two_gamma$significant_XGLM_zero])



    # MAE_single_Gaussian <<- c(MAE_single_Gaussian, single_gaussian$MAE)
    MAE_single_Gamma <<- c(MAE_single_Gamma, single_gamma$MAE)
    # RMSE_single_Gaussian <<- c(RMSE_single_Gaussian, single_gaussian$RMSE)
    RMSE_single_Gamma <<- c(RMSE_single_Gamma, single_gamma$RMSE)
    # Rsquare_single_Gaussian <<- c(Rsquare_single_Gaussian, single_gaussian$Rsquare)
    Rsquare_single_Gamma <<- c(Rsquare_single_Gamma, single_gamma$Rsquare)
    


    # MAE1_two_Gaussian <<- c(MAE1_two_Gaussian, two_gaussian$MAE1)
    MAE1_two_Gamma <<- c(MAE1_two_Gamma, two_gamma$MAE1)
    # RMSE1_two_Gaussian <<- c(RMSE1_two_Gaussian, two_gaussian$RMSE1)
    RMSE1_two_Gamma <<- c(RMSE1_two_Gamma, two_gamma$RMSE1)
    # Rsquare1_two_Gaussian <<- c(Rsquare1_two_Gaussian, two_gaussian$Rsquare1)
    Rsquare1_two_Gamma <<- c(Rsquare1_two_Gamma, two_gamma$Rsquare1)
    
    # If everything runs without error, return TRUE
    TRUE
  }, error = function(e) {
    message("Error in attempt ", attempt_count, ": ", e$message)
    # If there is an error, return FALSE and do not increment the valid counter.
    FALSE
  })
  
  # Only count this attempt if no error occurred
  if (valid) {
    valid_iterations <- valid_iterations + 1
    cat("Valid iteration:", valid_iterations, "of 400.\n")
  }
}





res_dir <- paste0("results", j)

write.csv(list_significant_single_Gamma,
          file = file.path(res_dir, "list_significant_single_Gamma.csv"))

write.csv(list_significant_two_Gamma_logistic,
          file = file.path(res_dir, "list_significant_two_Gamma_logistic.csv"))

write.csv(list_significant_two_Gamma_GLM_zero,
          file = file.path(res_dir, "list_significant_two_Gamma_GLM_zero.csv"))

write.csv(list_significant_two_Gamma_GLM_nonzero,
          file = file.path(res_dir, "list_significant_two_Gamma_GLM_nonzero.csv"))


write.csv(list_significant_two_combined_GLM_zero_logistic,
          file = file.path(res_dir, "list_significant_two_combined_GLM_zero_logistic.csv"))

write.csv(list_significant_two_combined_GLM_nonzero_logistic,
          file = file.path(res_dir, "list_significant_two_combined_GLM_nonzero_logistic.csv"))

write.csv(list_significant_two_combined_GLM_zero_nonzero,
          file = file.path(res_dir, "list_significant_two_combined_GLM_zero_nonzero.csv"))


write(MAE_single_Gamma,
      file = file.path(res_dir, "MAE_single_Gamma.txt"))

write(RMSE_single_Gamma,
      file = file.path(res_dir, "RMSE_single_Gamma.txt"))

write(Rsquare_single_Gamma,
      file = file.path(res_dir, "Rsquare_single_Gamma.txt"))


write(MAE1_two_Gamma,
      file = file.path(res_dir, "MAEstandard_two_Gamma.txt"))

write(RMSE1_two_Gamma,
      file = file.path(res_dir, "RMSEstandard_two_Gamma.txt"))

write(Rsquare1_two_Gamma,
      file = file.path(res_dir, "Rsquarestandard_two_Gamma.txt"))






