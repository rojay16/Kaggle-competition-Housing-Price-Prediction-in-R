# Kaggle-competition-Housing-Price-Prediction-in-R

Keywords: Kaggle, House price predction, stacked model, random forest, gradient boosting, elastic-net regression, imputation, Box-Cox transformation,
near zero variance.

The R script in the repository was used to generate an entry into the House Prices Advanced Regression Techniques competition in Kaggle. 
The entry was in the top 28% of submissions. The orginal data set required several preprocessing techniques. First NA values in the data set 
were inputed as "NA" strings for factor variables and 0 for numeric variables. Furthemore variables found in the test set but not the training set
were imputed using multipe imputation chain equations (mice). Thus the R package "mice" was used to to impute factor variable values
in the test set that are also in the training set. I also use Box-Cox transformation on continous variables (via the function BoxCoxTrans()
and remove variables that have near zero variance (via the function nvz()). A stacked model is then used to predict the hosuing prices, using 
random forests, gradient boosting and eleastic-net regression. The stacked model provides a better prediction than each individual model. 
This script utilized the packages caret, mice, dyplyr and random forest.

The data set used in this script can be found in the House Prices Advanced Regression Techniques competition in Kaggle
