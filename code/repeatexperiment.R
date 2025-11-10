source('SingleGLM.R')
source('Twopart.R')

thewholename=read.table('1.txt')[,1]
# list_significant_single_Gaussian=create_initial_cumulative_dict(thewholename)
list_significant_single_Gamma=create_initial_cumulative_dict(thewholename)
# list_significant_two_Gaussian_logistic=create_initial_cumulative_dict(thewholename)
# list_significant_two_Gaussian_GLM=create_initial_cumulative_dict(thewholename)
list_significant_two_Gamma_logistic=create_initial_cumulative_dict(thewholename)
list_significant_two_Gamma_GLM=create_initial_cumulative_dict(thewholename)

list_significant_two_combined_GLM=create_initial_cumulative_dict(thewholename)


# MAE_single_Gaussian=c()
MAE_single_Gamma=c()
# RMSE_single_Gaussian=c()
RMSE_single_Gamma=c()
# Rsquare_single_Gaussian=c()
Rsquare_single_Gamma=c()



# MAE_two_Gaussian=c()
MAE_two_Gamma=c()
# RMSE_two_Gaussian=c()
RMSE_two_Gamma=c()
# Rsquare_two_Gaussian=c()
Rsquare_two_Gamma=c()


# TP_two_Gaussian=c()
TP_two_Gamma=c()
# FP_two_Gaussian=c()
FP_two_Gamma=c()
# TN_two_Gaussian=c()
TN_two_Gamma=c()
# FN_two_Gaussian=c()
FN_two_Gamma=c()
# TPR_two_Gaussian=c()
TPR_two_Gamma=c()
# FPR_two_Gaussian=c()
FPR_two_Gamma=c()


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
    train_set <- datalist[[1]]
    test_set  <- datalist[[2]]
    
    single_gamma   <- gamma_regression_with_categories(train_set, test_set, 0.05)
    two_gamma      <- two_part_with_categories(train_set, test_set, 0.05)
    # single_gaussian <- gaussian_regression_with_categories(train_set, test_set, 0.05)
    # two_gaussian   <- two_part_with_categories(train_set, test_set, 0.05, module = 'Gaussian')
    
    list_significant_single_Gamma <<- combine_significant_predictors(list_significant_single_Gamma, single_gamma$significant_X)
    # list_significant_single_Gaussian <<- combine_significant_predictors(list_significant_single_Gaussian, single_gaussian$significant_X)
    
    # list_significant_two_Gaussian_logistic <<- combine_significant_predictors(list_significant_two_Gaussian_logistic, two_gaussian$significant_Xlogit)
    # list_significant_two_Gaussian_GLM <<- combine_significant_predictors(list_significant_two_Gaussian_GLM, two_gaussian$significant_XGLM)
    
    list_significant_two_Gamma_logistic <<- combine_significant_predictors(list_significant_two_Gamma_logistic, two_gamma$significant_Xlogit)
    list_significant_two_Gamma_GLM <<- combine_significant_predictors(list_significant_two_Gamma_GLM, two_gamma$significant_XGLM)
    list_significant_two_combined_GLM<<- combine_significant_predictors(list_significant_two_combined_GLM,two_gamma$significant_XGLM[two_gamma$significant_XGLM%in% two_gamma$significant_Xlogit])
    # MAE_single_Gaussian <<- c(MAE_single_Gaussian, single_gaussian$MAE)
    MAE_single_Gamma <<- c(MAE_single_Gamma, single_gamma$MAE)
    # RMSE_single_Gaussian <<- c(RMSE_single_Gaussian, single_gaussian$RMSE)
    RMSE_single_Gamma <<- c(RMSE_single_Gamma, single_gamma$RMSE)
    # Rsquare_single_Gaussian <<- c(Rsquare_single_Gaussian, single_gaussian$Rsquare)
    Rsquare_single_Gamma <<- c(Rsquare_single_Gamma, single_gamma$Rsquare)
    
    # MAE_two_Gaussian <<- rbind(MAE_two_Gaussian, two_gaussian$MAE)
    MAE_two_Gamma <<- rbind(MAE_two_Gamma, two_gamma$MAE)
    # RMSE_two_Gaussian <<- rbind(RMSE_two_Gaussian, two_gaussian$RMSE)
    RMSE_two_Gamma <<- rbind(RMSE_two_Gamma, two_gamma$RMSE)
    # Rsquare_two_Gaussian <<- rbind(Rsquare_two_Gaussian, two_gaussian$Rsquare)
    Rsquare_two_Gamma <<- rbind(Rsquare_two_Gamma, two_gamma$Rsquare)
    
    # TP_two_Gaussian <<- rbind(TP_two_Gaussian, two_gaussian$TP)
    TP_two_Gamma <<- rbind(TP_two_Gamma, two_gamma$TP)
    # FP_two_Gaussian <<- rbind(FP_two_Gaussian, two_gaussian$FP)
    FP_two_Gamma <<- rbind(FP_two_Gamma, two_gamma$FP)
    # TN_two_Gaussian <<- rbind(TN_two_Gaussian, two_gaussian$TN)
    TN_two_Gamma <<- rbind(TN_two_Gamma, two_gamma$TN)
    # FN_two_Gaussian <<- rbind(FN_two_Gaussian, two_gaussian$FN)
    FN_two_Gamma <<- rbind(FN_two_Gamma, two_gamma$FN)
    
    # TPR_two_Gaussian <<- rbind(TPR_two_Gaussian, two_gaussian$TPR)
    TPR_two_Gamma <<- rbind(TPR_two_Gamma, two_gamma$TPR)
    # FPR_two_Gaussian <<- rbind(FPR_two_Gaussian, two_gaussian$FPR)
    FPR_two_Gamma <<- rbind(FPR_two_Gamma, two_gamma$FPR)
    
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



write.csv(list_significant_single_Gamma, 'results/list_significant_single_Gamma.csv')
write.csv(list_significant_two_Gamma_logistic, 'results/list_significant_two_Gamma_logistic.csv')
write.csv(list_significant_two_Gamma_GLM, 'results/list_significant_two_Gamma_GLM.csv')
write.csv(list_significant_two_combined_GLM, 'results/list_significant_two_combined_GLM.csv')

write(MAE_single_Gamma, 'results/MAE_single_Gamma.txt')
write(RMSE_single_Gamma, 'results/RMSE_single_Gamma.txt')
write(Rsquare_single_Gamma, 'results/Rsquare_single_Gamma.txt')
# write.csv(MAE_two_Gamma, 'results/MAE_two_Gamma.csv')
# write.csv(RMSE_two_Gamma, 'results/RMSE_two_Gamma.csv')
# write.csv(Rsquare_two_Gamma, 'results/Rsquare_two_Gamma.csv')
# write.csv(TP_two_Gamma, 'results/TP_two_Gamma.csv')
# write.csv(FP_two_Gamma, 'results/FP_two_Gamma.csv')
# write.csv(TN_two_Gamma, 'results/TN_two_Gamma.csv')
# write.csv(FN_two_Gamma, 'results/FN_two_Gamma.csv')
# write.csv(TPR_two_Gamma, 'results/TPR_two_Gamma.csv')
# write.csv(FPR_two_Gamma, 'results/FPR_two_Gamma.csv')
write(MAE1_two_Gamma, 'results/MAEstandard_two_Gamma.txt')
write(RMSE1_two_Gamma, 'results/RMSEstandard_two_Gamma.txt')
write(Rsquare1_two_Gamma, 'results/Rsquarestandard_two_Gamma.txt')




# write.csv(list_significant_single_Gaussian, 'results/list_significant_single_Gaussian.csv')
# write.csv(list_significant_two_Gaussian_logistic, 'results/list_significant_two_Gaussian_logistic.csv')
# write.csv(list_significant_two_Gaussian_GLM, 'results/list_significant_two_Gaussian_GLM.csv')
# write(MAE_single_Gaussian, 'results/MAE_single_Gaussian.txt')
# write(RMSE_single_Gaussian, 'results/RMSE_single_Gaussian.txt')
# write(Rsquare_single_Gaussian, 'results/Rsquare_single_Gaussian.txt')
# write.csv(MAE_two_Gaussian, 'results/MAE_two_Gaussian.csv')
# write.csv(RMSE_two_Gaussian, 'results/RMSE_two_Gaussian.csv')
# write.csv(Rsquare_two_Gaussian, 'results/Rsquare_two_Gaussian.csv')
# write.csv(TP_two_Gaussian, 'results/TP_two_Gaussian.csv')
# write.csv(FP_two_Gaussian, 'results/FP_two_Gaussian.csv')
# write.csv(TN_two_Gaussian, 'results/TN_two_Gaussian.csv')
# write.csv(FN_two_Gaussian, 'results/FN_two_Gaussian.csv')
# write.csv(TPR_two_Gaussian, 'results/TPR_two_Gaussian.csv')
# write.csv(FPR_two_Gaussian, 'results/FPR_two_Gaussian.csv')
# write(MAE1_two_Gaussian, 'results/MAEstandard_two_Gaussian.txt')
# write(RMSE1_two_Gaussian, 'results/RMSEstandard_two_Gaussian.txt')
# write(Rsquare1_two_Gaussian, 'results/Rsquarestandard_two_Gaussian.txt')






sum(Rsquare1_two_Gamma>Rsquare_single_Gamma)
sum(MAE1_two_Gaussian>MAE_single_Gaussian)
sum(RMSE1_two_Gaussian>RMSE_single_Gaussian)
sum(RMSE1_two_Gamma>RMSE_single_Gamma)

sd(RMSE1_two_Gaussian)
sd(RMSE_single_Gaussian)
sd(RMSE1_two_Gamma)
sd(RMSE_single_Gamma)
sd(MAE1_two_Gamma)
t.test(RMSE1_two_Gamma,RMSE_single_Gaussian)
