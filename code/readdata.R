library(dplyr)
library(caret)
library(broom)
library(MASS)
library(car)
library(mvrsquared)
# Load the dataset
datawhole<-read.csv('data/Baseline.csv')

datad1zero<-datawhole[datawhole$FullSurgBPI3_d1==0,]
datad1nonzero<-datawhole[datawhole$FullSurgBPI3_d1!=0,]

matzero=datad1zero[,1:25]
matzero=matzero[complete.cases(matzero),]
matnonzero=datad1nonzero[,1:25]
matnonzero=matnonzero[complete.cases(matnonzero),]




datamatrix <- rbind(matzero, matnonzero)
datamatrix=datawhole
# Shuffle rows randomly
set.seed(123)  # Set seed for reproducibility (optional)
datamatrix_shuffle <- datamatrix[sample(nrow(datamatrix)), ]

mat<-datamatrix_shuffle
mat=mat[mat$educ!=1,]
mat=mat[mat$SurgGroup!="Optho",]
mat=mat[mat$asa!=4,]
mat=mat[mat$Comb_Anc!=3,]


nrow(mat)
mat$Comb_Sex=as.factor(mat$Comb_Sex)
mat$Comb_Anc=as.factor(mat$Comb_Anc)
mat$Comb_Anc=relevel(mat$Comb_Anc, ref = "4")
mat$educ=as.factor(mat$educ)
mat$educ=relevel(mat$educ, ref = "3")

mat$marital=as.factor(mat$marital)
mat$asa=as.factor(mat$asa)
mat$SurgGroup=as.factor(mat$SurgGroup)
mat$FullOpioidUse2_d1=as.factor(mat$FullOpioidUse2_d1)






split_train_test_stratified <- function(data_matrix, num_train, max_attempts = 100) {
  # Check if num_train is valid
  if (num_train < 1 || num_train > nrow(data_matrix)) {
    stop("Number of training samples must be between 1 and the number of rows in the matrix.")
  }
  
  # Detect categorical columns
  categorical_cols <- sapply(data_matrix, function(col) {
    is.factor(col) || (is.character(col) && length(unique(col)) < nrow(data_matrix) / 2)
  })
  
  # Ensure there is at least one categorical column
  if (!any(categorical_cols)) {
    stop("No categorical columns detected for stratification.")
  }
  
  # Use the first detected categorical column for stratification
  category_col <- names(data_matrix)[which(categorical_cols)[1]]
  
  # Force the stratification column to be a factor with its full levels
  if (!is.factor(data_matrix[[category_col]])) {
    data_matrix[[category_col]] <- factor(data_matrix[[category_col]], 
                                          levels = sort(unique(data_matrix[[category_col]])))
  }
  
  # Get the original levels (full set of categories)
  original_levels <- levels(data_matrix[[category_col]])
  
  # Initialize counters for re-separation attempts
  attempt <- 0
  
  # Loop until all categories are represented in both sets or max_attempts is reached
  while (attempt < max_attempts) {
    attempt <- attempt + 1
    
    # Shuffle the rows of the matrix
    shuffled_matrix <- data_matrix[sample(nrow(data_matrix)), ]
    
    # Split into train and test sets
    train_set <- shuffled_matrix[1:num_train, ]
    test_set <- shuffled_matrix[(num_train + 1):nrow(shuffled_matrix), ]
    
    # Ensure that both sets have the same factor levels for the stratification column
    train_set[[category_col]] <- factor(train_set[[category_col]], levels = original_levels)
    test_set[[category_col]]  <- factor(test_set[[category_col]],  levels = original_levels)
    
    # Check if all categories are present (observed) in both sets
    train_categories <- levels(train_set[[category_col]])
    test_categories  <- levels(test_set[[category_col]])
    
    # With explicit levels set above, both train_categories and test_categories will
    # always equal original_levels. However, you may want to check that there is at least
    # one observation for each category in both sets:
    observed_train <- unique(train_set[[category_col]])
    observed_test  <- unique(test_set[[category_col]])
    
    if (all(original_levels %in% observed_train) && all(original_levels %in% observed_test)) {
      # cat("Successful separation after", attempt, "attempts.\n")
      return(list(train = train_set, test = test_set))
    }
  }
  
  # If max_attempts is reached without success, stop with an error
  stop("Failed to achieve a satisfactory separation after ", max_attempts, " attempts.")
}






