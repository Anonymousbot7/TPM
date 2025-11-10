
gamma_regression_with_categories <- function(train_set, test_set, alpha = 0.05) {
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

  train_set[,25]= train_set[,25]+0.1
  test_set[,25]= test_set[,25]+0.1
#  comb_age, Comb_Sex, Comb_Anc, educ, marital, asa, bmi, SurgGroup, BodyBPI3_d1,
 
#   BodyBPI5_d1, FullSurgBPI3_d1, FullSurgBPI5_d1, ACRFDC2a_d1, ACRFDC2b_d1, ACRFDC2c_d1, ACRFDC3a_d1,
  
#    ACRFDC3b_d1, ACRFDC3c_d1, MBM_Wolfe1_d1, FullOpioidUse2_d1, Anx_Tscore_d1, Dep_Tscore_d1
  # Step 3: Create a design matrix with dummy variables for categorical predictors
  model_with_intercept=glm(FullSurgBPI5_m3 ~ comb_age + Comb_Sex + Comb_Anc+educ +marital+asa
                            +bmi+SurgGroup+BodyBPI5_d1+FullSurgBPI5_d1+SSI_Wolfe2_d1+MBM_Wolfe1_d1+FullOpioidUse2_d1+Anx_Tscore_d1+Dep_Tscore_d1, data = train_set, family = Gamma(link='log'))
  
  # Step 5: Perform significance testing for predictors
  summary_model <- summary(model_with_intercept)
  p_values <- summary_model$coefficients[, "Pr(>|t|)"]
  significant_predictors <- names(p_values[p_values < alpha]) # Significant X
  


  # Convert categorical variables in X_test to factors
  test_set <- as.data.frame(lapply(test_set, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(as.factor(col)) # Convert to factor
    } else {
      return(col) # Keep numeric columns as-is
    }
  }))
  test_set[,25][test_set[,25]==0]=0.1

  # 1. Predict the mean response on the test set:
p_test <- predict(model_with_intercept, newdata = test_set, type = "response")

# 2. Extract the dispersion parameter from the training model:
phi <- summary(model_with_intercept)$dispersion

# 3. Compute the test log-likelihood:
#    Note: dgamma requires 'shape' and 'scale'. For a Gamma GLM with a log link,
#    a common parameterization is: shape = 1/phi, scale = phi * mu,
#    so that E(y) = mu and Var(y) = phi * mu^2.
test_loglik <- sum(dgamma(test_set$FullSurgBPI5_m3,
                          shape = 1/phi,
                          scale = phi * p_test,
                          log = TRUE))

# 4. Get the number of parameters (degrees of freedom) from the training model:
df_model <- attr(logLik(model_with_intercept), "df")

# 5. Compute the out-of-sample AIC-like measure:
AIC_test <- -2 * test_loglik + 2 * df_model


predictions <- predict(model_with_intercept, newdata = as.data.frame(test_set),type='response')
predictions[predictions<0]=0
R_square=1-sum((predictions - (test_set[, 25]))^2)/sum((mean(test_set[, 25]) - test_set[, 25])^2)
  # Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predictions - test_set[,25]))
RMSE_values<-sqrt(mean((predictions - test_set[, 25])^2)) 
  # Step 9: Return results as a list
  return(list(
    significant_X = significant_predictors,
    MAE = mae,
    Rsquare=R_square,
    RMSE=RMSE_values,
    AIC=AIC_test
  ))
}

datalist=split_train_test(mat,3000)
train_set[1,]
train_set=datalist[[1]]
test_set=datalist[[2]]

gamma_regression_with_categories(train_set,test_set,0.05)



