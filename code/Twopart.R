# Function to calculate TP, TN, FP, FN for a single threshold
calculate_confusion_matrix <- function(pred_prob, y, threshold) {
  y_pred <- ifelse(pred_prob >= threshold, 1, 0)
  TP <- sum(y >= 1 & y_pred == 1)
  TN <- sum(y == 0 & y_pred == 0)
  FP <- sum(y == 0 & y_pred == 1)
  FN <- sum(y >= 1 & y_pred == 0)
  c(TP = TP, TN = TN, FP = FP, FN = FN)
}


two_part_with_categories <- function(train_set, test_set, alpha = 0.05,module='Gamma') {
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
  train_set0$FullSurgBPI5_m3[train_set0$FullSurgBPI5_m3<=0]=0
  train_set0$FullSurgBPI5_m3[train_set0$FullSurgBPI5_m3>0]=1
  # Step 3: Create a design matrix with dummy variables for categorical predictors
  model_with_intercept_logit=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set0, family = 'binomial')




  summary_model_logit <- summary(model_with_intercept_logit)
  p_values_logit <- summary_model_logit$coefficients[, "Pr(>|z|)"] # Exclude intercept
  significant_predictors_logit <- names(p_values_logit[p_values_logit < alpha]) # Significant X
  
  # levels_full <- levels(train_set$SurgGroup)
  train_set_nonzero=train_set[train_set[,25]>0,]
  # train_set_nonzero$SurgGroup <- factor(train_set_nonzero$SurgGroup, levels = levels_full)
  if (module=='Gamma'){
  model_with_intercept_glm=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set_nonzero, family = Gamma(link='log'))
                            }
  else{
    model_with_intercept_glm=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set_nonzero, family = 'gaussian')
                            }


  summary_model_glm <- summary(model_with_intercept_glm)
  p_values_glm <- summary_model_glm$coefficients[, "Pr(>|t|)"]
  significant_predictors_glm <- names(p_values_glm[p_values_glm < alpha]) # Significant X





  # Convert categorical variables in X_test to factors
  test_set <- as.data.frame(lapply(test_set, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(as.factor(col)) # Convert to factor
    } else {
      return(col) # Keep numeric columns as-is
    }
  }))
  





predictions_prob <- predict(model_with_intercept_logit, newdata = as.data.frame(test_set),type='response')
predictions_glm  <- predict(model_with_intercept_glm, newdata = as.data.frame(test_set),type='response')


p_test <- predict(model_with_intercept_logit, newdata = test_set, type = "response")

# Step 2: Predict the mean outcome for positive cases using the Gamma model.
# We'll create a vector to hold predictions for all test observations.
mu_pred <- rep(NA, nrow(test_set))
nonzero_idx <- test_set$FullSurgBPI5_m3 > 0
mu_pred[nonzero_idx] <- predict(model_with_intercept_glm, newdata = test_set[nonzero_idx, ], type = "response")

# Step 3: Obtain the dispersion parameter from the Gamma model.
phi <- summary(model_with_intercept_glm)$dispersion

# Step 4: Compute the log-likelihood contributions for each test observation.
# For y = 0: use the logistic part only.
# For y > 0: use the logistic part and the Gamma density.
loglik <- rep(NA, nrow(test_set))
loglik[test_set$FullSurgBPI5_m3 == 0] <- log(1 - p_test[test_set$FullSurgBPI5_m3 == 0])
loglik[nonzero_idx] <- log(p_test[nonzero_idx]) +
  dgamma(test_set$FullSurgBPI5_m3[nonzero_idx],
         shape = 1/phi,
         scale = phi * mu_pred[nonzero_idx],
         log = TRUE)

# Step 5: Sum the log-likelihood contributions to get the overall log-likelihood.
LL_total <- sum(loglik)

# Step 6: Extract the number of estimated parameters (degrees of freedom) from both models.
df_logit <- attr(logLik(model_with_intercept_logit), "df")
df_gamma <- attr(logLik(model_with_intercept_glm), "df")
df_total <- df_logit + df_gamma

# Step 7: Compute the overall AIC for the test set.
AIC_test <- -2 * LL_total + 2 * df_total

thresholds <- seq(0.0,  0.99, by = 0.01)

result_matrix <- sapply(thresholds, function(th) {
  calculate_confusion_matrix(predictions_prob, test_set[,25], th)
})

listTP=result_matrix[1,]
listTN=result_matrix[2,]
listFP=result_matrix[3,]
listFN=result_matrix[4,]


listFPR=listFP/(listFP+listTN)
listTPR=listTP/(listTP+listFN)







if (module!='Gamma'){
predictions_glm[predictions_glm<0]=0
}

# Compute MAE for each threshold using vectorized operation
mae_values <- sapply(thresholds, function(threshold) {
#   predictions <- (predictions_prob > threshold) * predictions_glm
predictions <- (predictions_prob>threshold ) * predictions_glm
  predictions[predictions < 0] <- 0  # Ensure non-negative predictions
  mean(abs(predictions - test_set[, 25]))  # Compute MAE
})


RMSE_values <- sapply(thresholds, function(threshold) {
#   predictions <- (predictions_prob > threshold) * predictions_glm
predictions <- (predictions_prob>threshold ) * predictions_glm
  predictions[predictions < 0] <- 0  # Ensure non-negative predictions
  sqrt(mean((predictions - test_set[, 25])^2))  # Compute MAE
})


predictions1 <- (predictions_prob ) * predictions_glm
mae_values1<-mean(abs(predictions1 - test_set[, 25])) 
RMSE_values1<-sqrt(mean((predictions1 - test_set[, 25])^2)) 
R_square1<-1-sum((predictions1 - (test_set[, 25]))^2)/sum((mean(test_set[, 25]) - test_set[, 25])^2)






R_square <- sapply(thresholds, function(threshold) {
  predictions <- (predictions_prob>threshold) * predictions_glm
  predictions[predictions < 0] <- 0  # Ensure non-negative predictions
  1-(sum((predictions - (test_set[, 25]))^2))/(sum((mean(test_set[, 25]) - test_set[, 25])^2))  # Compute Rsquare
})


Number_nonzero <- sapply(thresholds, function(threshold) {
  predictions <- sum(predictions_prob > threshold) 
})


  # Step 9: Return results as a list
  return(list(
    significant_Xlogit = significant_predictors_logit,
    significant_XGLM =significant_predictors_glm,
    MAE = mae_values,
    Number=Number_nonzero,
    Rsquare=R_square,
    MAE1=mae_values1,
    Rsquare1=R_square1,
    RMSE=RMSE_values,
    RMSE1=RMSE_values1,
    TP=listTP,
    TN=listTN,
    FP=listFP,
    FN=listFN,
    FPR=listFPR,
    TPR=listTPR,
    AIC=AIC_test
  ))
}




datalist=split_train_test_stratified(mat,3000)
train_set=datalist[[1]]
test_set=datalist[[2]]


singlegamma=gamma_regression_with_categories(train_set,test_set,0.05)
resultgamma=two_part_with_categories(train_set,test_set,0.05)

resultgamma$Rsquare1
singlegamma$Rsquare

resultgamma$RMSE1
singlegamma$RMSE



resultgamma$AIC
singlegamma$AIC

resultgamma$MAE1
singlegamma$MAE




singlegaussian$Rsquare




singlegamma$Rsquare







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



write(row.names(summary_model_glm$coefficients),'1.txt')
# Initial cumulative dictionary (can be NULL for the first call)
cumulative_dict=create_initial_cumulative_dict(row.names(summary_model_glm$coefficients))
new_predictors <- gamma_regression_with_categories(train_set,test_set,0.05)$significant_X
cumulative_dict=combine_significant_predictors(cumulative_dict,new_predictors)









