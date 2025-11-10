# Function to calculate TP, TN, FP, FN for a single threshold
calculate_confusion_matrix <- function(pred_prob, y, threshold) {
  y_pred <- ifelse(pred_prob >= threshold, 1, 0)
  TP <- sum(y >= 1 & y_pred == 1)
  TN <- sum(y == 0 & y_pred == 0)
  FP <- sum(y == 0 & y_pred == 1)
  FN <- sum(y >= 1 & y_pred == 0)
  c(TP = TP, TN = TN, FP = FP, FN = FN)
}






two_part_with_categories_j <- function(train_set, test_set, alpha = 0.05,module='Gamma', thres=3) {
  # Step 1: Extract Y (response) and X (predictors) from the training set
#   Y_train <- train_set[, 25]  # Column 25 is the response variable
#   X_train <- train_set[, c(-1,-25)] # All other columns are predictors
  
  # Step 2: Convert categorical variables in X_train to factors
  train_set <- as.data.frame(lapply(train_set, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(as.factor(col)) # Convert to factor
    } else {
      return(col) # Keep numeric columns as-is
    }
  }))


  train_set0=train_set
  train_set0$FullSurgBPI5_m3[train_set0$FullSurgBPI5_m3<=thres]=0
  train_set0$FullSurgBPI5_m3[train_set0$FullSurgBPI5_m3>thres]=1
  # Step 3: Create a design matrix with dummy variables for categorical predictors
  model_with_intercept_logit=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set0, family = 'binomial')




  summary_model_logit <- summary(model_with_intercept_logit)
  p_values_logit <- summary_model_logit$coefficients[, "Pr(>|z|)"] # Exclude intercept
  significant_predictors_logit <- names(p_values_logit[p_values_logit < alpha]) # Significant X
  
  # levels_full <- levels(train_set$SurgGroup)
  train_set_nonzero=train_set[train_set[,25]>thres,]
  train_set_zero=train_set[train_set[,25]<=thres,]
  train_set_zero[,25][train_set_zero[,25]==0]=0.1
  
  # train_set_nonzero$SurgGroup <- factor(train_set_nonzero$SurgGroup, levels = levels_full)
  if (module=='Gamma'){
  model_with_intercept_glm_nonzero=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set_nonzero, family = Gamma(link='log'))
  model_with_intercept_glm_zero=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set_zero, family = Gamma(link='log'))
                            
                            
                            }
  else{
    model_with_intercept_glm=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set_nonzero, family = 'gaussian')
                            }


  summary_model_glm_nonzero <- summary(model_with_intercept_glm_nonzero)
  summary_model_glm_zero <- summary(model_with_intercept_glm_zero)

  p_values_glm_nonzero <- summary_model_glm_nonzero$coefficients[, "Pr(>|t|)"]
  p_values_glm_zero <- summary_model_glm_zero$coefficients[, "Pr(>|t|)"]

  significant_predictors_glm_nonzero <- names(p_values_glm_nonzero[p_values_glm_nonzero < alpha]) # Significant X
  significant_predictors_glm_zero <- names(p_values_glm_zero[p_values_glm_zero < alpha]) # Significant X





  # Convert categorical variables in X_test to factors
  test_set <- as.data.frame(lapply(test_set, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(as.factor(col)) # Convert to factor
    } else {
      return(col) # Keep numeric columns as-is
    }
  }))
  
  test_set[,25][test_set[,25]==0]=0.1




predictions_prob <- predict(model_with_intercept_logit, newdata = as.data.frame(test_set),type='response')
predictions_glm_nonzero  <- predict(model_with_intercept_glm_nonzero, newdata = as.data.frame(test_set),type='response')
predictions_glm_zero  <- predict(model_with_intercept_glm_zero, newdata = as.data.frame(test_set),type='response')




p_test <- predict(model_with_intercept_logit, newdata = test_set, type = "response")

# Step 2: Predict the mean outcome for positive cases using the Gamma model.
# We'll create a vector to hold predictions for all test observations.
mu_pred <- rep(NA, nrow(test_set))

nonzero_idx <- test_set$FullSurgBPI5_m3 > thres
mu_pred[nonzero_idx] <- predict(model_with_intercept_glm_nonzero, newdata = test_set[nonzero_idx, ], type = "response")






if (module!='Gamma'){
predictions_glm[predictions_glm<0]=0
}





predictions1 <- (predictions_prob ) * predictions_glm_nonzero+(1-predictions_prob)*predictions_glm_zero
mae_values1<-mean(abs(predictions1 - test_set[, 25])) 
RMSE_values1<-sqrt(mean((predictions1 - test_set[, 25])^2)) 
R_square1<-1-sum((predictions1 - (test_set[, 25]))^2)/sum((mean(test_set[, 25]) - test_set[, 25])^2)











  # Step 9: Return results as a list
  return(list(
    significant_Xlogit = significant_predictors_logit,
    significant_XGLM_zero =significant_predictors_glm_zero,
    significant_XGLM_nonzero =significant_predictors_glm_nonzero,
    MAE1=mae_values1,
    Rsquare1=R_square1,
    RMSE1=RMSE_values1
  ))
}



# datalist=split_train_test_stratified(mat,3000)
# train_set=datalist[[1]]
# test_set=datalist[[2]]


# singlegamma=gamma_regression_with_categories(train_set,test_set,0.05)
resultgamma_j=two_part_with_categories_j(train_set,test_set,0.05,thres=4)

# resultgamma_j$Rsquare1
# singlegamma$Rsquare


# resultgamma_j$RMSE1
# singlegamma$RMSE










create_initial_cumulative_dict <- function(row_names) {
  # Initialize a named vector with all counts set to 0
  cumulative_dict <- setNames(rep(0, length(row_names)), row_names)
  
  # Return the initialized dictionary
  return(cumulative_dict)
}


combine_significant_predictors <- function(cumulative_dict, new_predictors) {
  # Convert cumulative_dict to a named vector if it's not already
  if (!is.null(cumulative_dict)) {
    cumulative_counts <- unlist(cumulative_dict)
  } else {
    cumulative_counts <- c()
  }
  
  # Initialize a result dictionary to store updated counts and names
  result_dict <- cumulative_counts
  
  # Process each predictor in the new_predictors list
  for (pred in new_predictors) {
    if (pred %in% names(result_dict)) {
      # If the predictor already exists, increment its count
      result_dict[pred] <- result_dict[pred] + 1
    } else {
      # If the predictor is new, initialize its count to 1
      result_dict[pred] <- 1
    }
  }
  
  # Create dynamically named predictors based on their counts
  return(result_dict)
}











