
read.csv(list_significant_single_Gamma,'multisplit/list_significant_single_Gamma.csv')
read.csv(list_significant_single_Gaussian,'multisplit/list_significant_single_Gaussian.csv')
read.csv(list_significant_two_Gaussian_logistic,'multisplit/list_significant_two_Gaussian_logistic.csv')
read.csv(list_significant_two_Gaussian_GLM,'multisplit/list_significant_two_Gaussian_GLM.csv')
read.csv(list_significant_two_Gamma_logistic,'multisplit/list_significant_two_Gamma_logistic.csv')
read.csv(list_significant_two_Gamma_GLM,'multisplit/list_significant_two_Gamma_GLM.csv')









read.csv('multisplit/MAE_two_Gaussian.csv')
read.csv('multisplit/MAE_two_Gamma.csv')
read.csv('multisplit/RMSE_two_Gaussian.csv')
read.csv('multisplit/RMSE_two_Gamma.csv')
read.csv('multisplit/Rsquare_two_Gaussian.csv')
read.csv('multisplit/Rsquare_two_Gamma.csv')




read.csv('multisplit/TP_two_Gaussian.csv')
read.csv('multisplit/TP_two_Gamma.csv')
read.csv('multisplit/FP_two_Gaussian.csv')
read.csv('multisplit/FP_two_Gamma.csv')
read.csv('multisplit/TN_two_Gaussian.csv')
read.csv('multisplit/TN_two_Gamma.csv')
read.csv('multisplit/FN_two_Gaussian.csv')
read.csv('multisplit/FN_two_Gamma.csv')
read.csv('multisplit/TPR_two_Gaussian.csv')
read.csv('multisplit/TPR_two_Gamma.csv')
read.csv('multisplit/FPR_two_Gaussian.csv')
read.csv('multisplit/FPR_two_Gamma.csv')



MAE_single_Gaussian=scan('multisplit/MAE_single_Gaussian.txt')
MAE_single_Gamma=scan('multisplit/MAE_single_Gamma.txt')
RMSE_single_Gaussian=scan('multisplit/RMSE_single_Gaussian.txt')
RMSE_single_Gamma=scan('multisplit/RMSE_single_Gamma.txt')
Rsquare_single_Gaussian=scan('multisplit/Rsquare_single_Gaussian.txt')
Rsquare_single_Gamma=scan('multisplit/Rsquare_single_Gamma.txt')


MAE1_two_Gaussian=scan('multisplit/MAEstandard_two_Gaussian.txt')
MAE1_two_Gamma=scan('multisplit/MAEstandard_two_Gamma.txt')
RMSE1_two_Gaussian=scan('multisplit/RMSEstandard_two_Gaussian.txt')
RMSE1_two_Gamma=scan('multisplit/RMSEstandard_two_Gamma.txt')
Rsquare1_two_Gaussian=scan('multisplit/Rsquarestandard_two_Gaussian.txt')
Rsquare1_two_Gamma=scan('multisplit/Rsquarestandard_two_Gamma.txt')

t.test(Rsquare_single_Gaussian,Rsquare_single_Gamma)
mean(RMSE1_two_Gamma)
