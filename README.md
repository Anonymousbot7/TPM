# Two part model code

This is R script for the manuscirpt  **`A novel two-part statistical model for identifying baseline predictors of chronic postsurgical pain`**. 


## File Descriptions

- **`1.txt`** gives the name for the covariates.
- **`readdata.R`** gives the pre-address of the data.
- **`SingleGLM.R`** gives the code for single Gamma regression.
- **`Twopart.R`** **`Twopartj.R`** gives the code for the two part model regression.
- **`repeatexperiment.R`** **`repeatexperimentj.R`** repeatedly separate samples and conduct experiments.


## Working Flow

1. Run **`readdata.R`** to read csv files and prepare required functions such as ''split_train_test_stratified''.
2. Run **`SingleGLM.R`** and **`Twopart.R`** to prepare single regression model and two part regression model.
3. Run **`repeatexperiment.R`** to repeatedly separate samples and conduct experiments.
4. Do not need to run **`Twopartj.R`** and **`repeatexperimentj.R`**, these two codes are prepared for different threshold. In our two part models, the threshold is naturally 0.

## Note

These codes are allowed to run on a standard desktop computer. In accordance with data use agreements, the dataset cannot be uploaded.


