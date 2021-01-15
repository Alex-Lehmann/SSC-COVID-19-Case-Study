# Author: Andrea Payne

library(tidyverse)
library(car)
library(glmnet)

#running LASSO regression
set.seed(29)

#creating variables
deathProportion = nonStandardizedRegressionMatrix$deathProportion
#reducing covariates to a manageable level
StandardizedRegressionMatrixWODeathProp = scale(nonStandardizedRegressionMatrix %>% 
                                                  select(-c(No_Certificate_Diploma_Degree_Proportion, 
                                                            Secondary_School_Or_Equivalent_Proportion,
                                                            Trades_Certificate_Proportion, 
                                                            Certificate_Of_Apprenticeship_or_Qualification_Proportion,
                                                            College_Or_CEGEP_Diploma_Proportion, 
                                                            University_Diploma_Below_Bachelor_Proportion,
                                                            University_Bachelors_Degree_Proportion, 
                                                            University_Masters_Degree_Proportion,
                                                            University_Earned_Doctorate_Proportion, 
                                                            Apartment_Greater_Or_Equal_To_Five_Storeys_Proportion,
                                                            Apartment_Less_Than_Five_Storeys_Proportion, 
                                                            Multi_Census_Households_Proportion,
                                                            LICOAT, deathProportion)))


#running LASSO fit
cvLassoFit = cv.glmnet(StandardizedRegressionMatrixWODeathProp, deathProportion, 
                       family = 'gaussian', alpha = 1)
#plotting
plot(cvLassoFit)
plot(cvLassoFit$glmnet.fit)

#predicting death proportion
deathPredicted <- predict(cvLassoFit, s = "lambda.min", 
                          StandardizedRegressionMatrixWODeathProp)

# Sum of Squares Total and Error
sst <- sum((deathProportion- mean(deathProportion))^2)
sse <- sum((deathPredicted - deathProportion)^2)

# R squared
rsq <- 1 - (sse / sst)
rsq

#printing fitted coefficients
(lassoCoef = predict(cvLassoFit, type = "coefficients", s = "lambda.min")[(1:33),])
