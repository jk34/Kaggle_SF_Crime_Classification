# Kaggle_SF_Crime_Classification

## Summary

I obtained data from the San Francisco Crime Classification competition on Kaggle: https://www.kaggle.com/c/sf-crime, and used R programming to utilize machine learning algorithms to generate predictions for the classification of crimes

I used random forest, linear discriminant analysis, and gradient boosting on the training set to generate predictions. For gradient boosting, I had to convert the matrix containing the data into a sparse matrix, according to this: https://brittlab.uwaterloo.ca/2015/11/01/KaggleSFcrime


## Background

The goal was to use the features to predict the probabilities of each category the crime was likely to be. The crimes could be categorized as "Argon", "Assault", "Theft", etc. 

## Data Exploration

The CSV files provided by Kaggle are assumed to be already cleaned. I could perform further data exploration, but due to time constraints, I chose not to. I could have generated histograms and boxplots to look for noticeable typos and outliers

## Rest of Code

I first converted the Dates variable in DayOfWeek, Month, Year, Hour, Minute, and Second. I also used the Hour variable to create a Night variable, and used the Address variable to create an Intersection variable, as explained here: https://brittlab.uwaterloo.ca/2015/11/01/KaggleSFcrime/

The features I used were PdDistrict, DayOfWeek, Intersection, X, Y, and Night. PdDistrict is the name of the district in San Francisco that a crime occurred in, Intersection specified whether the crime occurred at an intersection or not, X and Y were the x,y coordinates that a crime occurred in, and Night specified whether the crime occurred after 10pm or before 6am.

## Results

I got a better value for the log-loss when using LDA than random forest. For LDA, I used the first 100000 rows of the validation set and the remaining rows as the training set for Cross Validation. The log-loss was 2.547. I could not do this with Random Forest because I kept getting errors with memory size because Random Forest uses up alot of the computer's RAM. Therefore, I had to use smaller data for the training and validation set. The log-loss was -3.18 when using just the rows 850001:878049 of the original training set file as the training set and the 1st 100 rows of that as the validation set and using ntree=100. 

I tried to get a better log-loss, so I got 6 samples that contained each outcome for the dependent variable (crime Category) using dplyr as the training set. I then used the first 50000 of the training set file as the validation set for Cross Validation. I then ran Random Forest with 5000 trees and computed the log-loss as 3.856. It worsened to 4.856 when using 200 samples that contained each possible outcome for the crime category.

So the log-loss for LDA was better than any of the log-loss values computed from Random Forest

I then used k-fold cross validation on LDA before creating a submission file containing the predicted probabilities on the test data provided by Kaggle. With 10 folds, the average log-loss was 2.668. 

I then used gradient boosting, which gave a better log-loss score than LDA or random forest. I submitted my predictions from gradient boosting to Kaggle

## Conclusion

These predictions could be improved as I didn't spend much time with feature engineering due to time constraints. 


