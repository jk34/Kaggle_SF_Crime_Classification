# Kaggle_SF_Crime_Classification
My work on the San Francisco Crime Classification competition on Kaggle: https://www.kaggle.com/c/sf-crime

I have ran Linear Discriminant Analysis and Random Forest onto the training data in order to create probabilities that 
test data will predict a certain crime category. 

So far, I got a better value for the log-loss when using LDA than random forest. For LDA, I used the first 100000 rows of the validation set and the remaining rows as the training set for Cross Validation. The log-loss was 2.547. I could not do this with Random Forest because I kept getting errors with memory size because Random Forest uses up alot of the computer's RAM. Therefore, I had to use smaller data for the training and validation set. The log-loss was -3.18 when using just the rows 850001:878049 of the original training set file as the training set and the 1st 100 rows of that as the validation set and using ntree=100. 

I tried to get a better log-loss, so I got 6 samples that contained each outcome for the dependent variable (crime Category) using dplyr as the training set. I then used the first 50000 of the training set file as the validation set for Cross Validation. I then ran Random Forest with 5000 trees and computed the log-loss as 3.856. It worsened to 4.856 when using 200 samples that contained each possible outcome for the crime category.

So the log-loss for LDA was better than any of the log-loss values computed from Random Forest

I then used k-fold cross validation on LDA before creating a submission file containing the predicted probabilities on the test data provided by Kaggle. With 10 folds, the average log-loss was 2.668. 

In the future, I plan to modify this by using k-fold cross-validation and trying to further tune the parameters for the Random Forest to get the best possible log-loss
