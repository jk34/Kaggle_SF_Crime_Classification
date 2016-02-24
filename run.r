date <- as.POSIXct("2009-03-08 01:59:59") # DST boundary
date2 <- as.POSIXct("2000-02-29 12:00:00")
span <- date2 - date  #creates interval 
library('lubridate')
as.duration(span)
[1] "-284651999s (~-9.02 years)"
as.numeric(span)/(3600*24)
[1] -0.03813175
span
Time difference of -3294.583 days



trainDF<-read.csv("train.csv")
testDF<-read.csv("test.csv")
#nearly a million rows!

trainDF_subset<-trainDF[,names(trainDF)[-c(3,5,6,7)]]
trainDF_subset$Category <- factor(trainDF_subset$Category)
#don't include "PdDistrict" because importance(rf,type) with both type=1 and 2 
#showed it was by far least important predictor

#can't try PCA since we have categorial variables

#remove these columns because the 3rd column Descript and 
#6th column Resolution are not in the test set
#the 7th column Address cannot be quantified. Maybe convert it to Zipcode?
#Can also use X and Y instead of the 7th column

library('lubridate')
trainDF_subset$Dates<- as.POSIXct(trainDF_subset$Dates, tz = "UTC")
datesDiff <- trainDF_subset[, 1] - trainDF_subset[878049, 1]
head(datesDiff)
trainDF_subset["timeDiff"]<-datesDiff
trainDF_subset<-trainDF_subset[,names(trainDF_subset)[-c(1)]] #get rid of Dates since we now have timeDiff

#as explained in Intro to Statistical Learning, because the outcome variable has 
#more than 2 outcomes, it's better to use LDA than logistic regression
#because the parameter estimates are unstable forlogistic regression, but that's 
#not true for LDA

#however, because the predictors are a mix of quantitative and categorical
#variables, we can't use LDA


testDF_subset<-testDF[,names(testDF)[-c(1,4,5)]]

testDF_subset$Dates<- as.POSIXct(testDF_subset$Dates, tz = "UTC")
datesDiffTest <- testDF_subset[, 1] - testDF_subset[878049, 1]
testDF_subset["timeDiff"]<-datesDiffTest
testDF_subset<-testDF_subset[,names(testDF_subset)[-c(1)]] #get rid of Dates since we now have timeDiff



#LDA
library('MASS')
#trainDF_dplyr<- group_by(trainDF_subset, Category) %>% sample_n(200, replace=TRUE)
#rCV_CV<-setdiff(trainDF_subset, trainDF_dplyr)
#CV_CV<-rCV_CV[1:100000,] #use remaining testdata as validation set for CV

testDF_subset<-testDF_subset[,names(testDF_subset)[-c(1)]] 
#get rid of DayOfWeek for TEST set since LDA only handles non-categorical predictors


logloss_sum=0
for (k in 1:10)
{
	#fac<-(878049/10)
	fac<-87800
	write(k, "")
	inds=(1+(k-1)*fac):(fac*k) #don't use "sample" because that takes random sample
	CV_CV<-list()
	CV_CV<-trainDF_subset[inds,]
	trainDF_subsetCV<-list()
	trainDF_subsetCV<-trainDF_subset[-inds,]
	trainDF_subsetCV$Category <- factor(trainDF_subsetCV$Category)
	table(trainDF_subsetCV$Category)

	lda.fit =lda(Category ~ timeDiff + X+Y , data=trainDF_subsetCV)


	CV_woCategory<-CV_CV[,names(CV_CV)[-c(1)]]

#use validation set without "Category"
#because you will use Training set to make predictions on 
#validation set without "Category" and then compare those predictions
#with the actual "Category" values in CV_CV 

	predictions_LDA <- predict(lda.fit, CV_woCategory, type = "prob")
	predictions <- data.frame(predictions_LDA)
	predictions<-predictions[,names(predictions)[-c(1,41,42,43)]]
#don't need "class" and linear discriminant coefficients

#subfile <- data.frame(id = CV_woCategory, pred=predictions)
#write.csv(subfile, file = "LDA_model_CV.csv", row.names = FALSE)

#to see how well model matched CV data
	sum<-0
#for(i in c(1:nrow(final_LDA))) {
	for(i in c(1:nrow(CV_CV))) {
    #row <- final_LDA[i, ]
		rowCV <- CV_CV[i, ]
		predrow<-predictions[i,]
		if(rowCV$Category=="ARSON" && !is.na('predrow$posterior.ARSON'))
    #sum <- row$pred*row$Win + sum
			sum <- log(predrow$posterior.ARSON) + sum
		else if(rowCV$Category=="ASSAULT" && !is.na('predrow$posterior.ASSAULT'))
			sum <- log(predrow$posterior.ASSAULT) + sum
		else if(rowCV$Category=="BAD CHECKS" && !is.na('predrow$posterior.BAD.CHECKS'))
			sum <- log(predrow$posterior.BAD.CHECKS) + sum
		else if(rowCV$Category=="BRIBERY" && !is.na('predrow$posterior.BRIBERY'))
			sum <- log(predrow$posterior.BRIBERY) + sum
		else if(rowCV$Category=="BURGLARY" && !is.na('predrow$posterior.BURGLARY'))
			sum <- log(predrow$posterior.BURGLARY) + sum
		else if(rowCV$Category=="DISORDERLY CONDUCT" && !is.na('predrow$posterior.DISORDERLY.CONDUCT'))
			sum <- log(predrow$posterior.DISORDERLY.CONDUCT) + sum
	    else if(rowCV$Category=="DRIVING UNDER THE INFLUENCE" && !is.na('predrow$posterior.DRIVING.UNDER.THE.INFLUENCE'))
            sum <- log(predrow$posterior.DRIVING.UNDER.THE.INFLUENCE) + sum
	    else if(rowCV$Category=="DRUG/NARCOTIC" && !is.na('predrow$posterior.DRUG.NARCOTIC'))
            sum <- log(predrow$posterior.DRUG.NARCOTIC) + sum
	    else if(rowCV$Category=="DRUNKENNESS" && !is.na('predrow$posterior.DRUNKENNESS'))
            sum <- log(predrow$posterior.DRUNKENNESS) + sum
	    else if(rowCV$Category=="EMBEZZLEMENT" && !is.na('predrow$posterior.EMBEZZLEMENT'))
            sum <- log(predrow$posterior.EMBEZZLEMENT) + sum
	    else if(rowCV$Category=="EXTORTION" && !is.na('predrow$posterior.EXTORTION'))
            sum <- log(predrow$posterior.EXTORTION) + sum
	    else if(rowCV$Category=="FAMILY OFFENSES" && !is.na('predrow$posterior.FAMILY.OFFENSES'))
            sum <- log(predrow$posterior.FAMILY.OFFENSES) + sum
	    else if(rowCV$Category=="FORGERY/COUNTERFEITING" && !is.na('predrow$posterior.FORGERY.COUNTERFEITING'))
            sum <- log(predrow$posterior.FORGERY.COUNTERFEITING) + sum
	    else if(rowCV$Category=="FRAUD" && !is.na('predrow$posterior.FRAUD'))
            sum <- log(predrow$posterior.FRAUD) + sum
	    else if(rowCV$Category=="GAMBLING" && !is.na('predrow$posterior.GAMBLING'))
            sum <- log(predrow$posterior.GAMBLING) + sum
	    else if(rowCV$Category=="KIDNAPPING" && !is.na('predrow$posterior.KIDNAPPING'))
            sum <- log(predrow$posterior.KIDNAPPING) + sum
	    else if(rowCV$Category=="LARCENY/THEFT" && !is.na('predrow$posterior.LARCENY.THEFT'))
            sum <- log(predrow$posterior.LARCENY.THEFT) + sum
	    else if(rowCV$Category=="LIQUOR LAWS" && !is.na('predrow$posterior.LIQUOR.LAWS'))
            sum <- log(predrow$posterior.LIQUOR.LAWS) + sum
	    else if(rowCV$Category=="LOITERING" && !is.na('predrow$posterior.LOITERING'))
            sum <- log(predrow$posterior.LOITERING) + sum
	    else if(rowCV$Category=="MISSING PERSON" && !is.na('predrow$posterior.MISSING.PERSON'))
            sum <- log(predrow$posterior.MISSING.PERSON) + sum
	    else if(rowCV$Category=="NON-CRIMINAL" && !is.na('predrow$posterior.NON.CRIMINAL'))
            sum <- log(predrow$posterior.NON.CRIMINAL) + sum
	    else if(rowCV$Category=="OTHER OFFENSES" && !is.na('predrow$posterior.OTHER.OFFENSES'))
            sum <- log(predrow$posterior.OTHER.OFFENSES) + sum
	    else if(rowCV$Category=="PORNOGRAPHY/OBSCENE MAT" && !is.na('predrow$posterior.PORNOGRAPHY.OBSCENE.MAT'))
            sum <- log(predrow$posterior.PORNOGRAPHY.OBSCENE.MAT) + sum
	    else if(rowCV$Category=="PROSTITUTION" && !is.na('predrow$posterior.PROSTITUTION'))
            sum <- log(predrow$posterior.PROSTITUTION) + sum
	    else if(rowCV$Category=="RECOVERED VEHICLE" && !is.na('predrow$posterior.RECOVERED.VEHICLE'))
            sum <- log(predrow$posterior.RECOVERED.VEHICLE) + sum
	    else if(rowCV$Category=="ROBBERY" && !is.na('predrow$posterior.ROBBERY'))
            sum <- log(predrow$posterior.ROBBERY) + sum
	    else if(rowCV$Category=="RUNAWAY" && !is.na('predrow$posterior.RUNAWAY'))
            sum <- log(predrow$posterior.RUNAWAY) + sum
	    else if(rowCV$Category=="SECONDARY CODES" && !is.na('predrow$posterior.SECONDARY.CODES'))
            sum <- log(predrow$posterior.SECONDARY.CODES) + sum
	    else if(rowCV$Category=="SEX OFFENSES FORCIBLE" && !is.na('predrow$posterior.SEX.OFFENSES.FORCIBLE'))
            sum <- log(predrow$posterior.SEX.OFFENSES.FORCIBLE) + sum
	    else if(rowCV$Category=="SEX OFFENSES NON FORCIBLE" && !is.na('predrow$posterior.SEX.OFFENSES.NON.FORCIBLE'))
            sum <- log(predrow$posterior.SEX.OFFENSES.NON.FORCIBLE) + sum
	    else if(rowCV$Category=="STOLEN PROPERTY" && !is.na('predrow$posterior.STOLEN.PROPERTY'))
            sum <- log(predrow$posterior.STOLEN.PROPERTY) + sum
	    else if(rowCV$Category=="SUICIDE" && !is.na('predrow$posterior.SUICIDE'))
            sum <- log(predrow$posterior.SUICIDE) + sum
	    else if(rowCV$Category=="SUSPICIOUS OCC" && !is.na('predrow$posterior.SUSPICIOUS.OCC'))
            sum <- log(predrow$posterior.SUSPICIOUS.OCC) + sum
	    else if(rowCV$Category=="TREA" && !is.na('predrow$posterior.TREA'))
            sum <- log(predrow$posterior.TREA) + sum
	    else if(rowCV$Category=="TRESPASS" && !is.na('predrow$posterior.TRESPASS'))
            sum <- log(predrow$posterior.TRESPASS) + sum
	    else if(rowCV$Category=="VANDALISM" && !is.na('predrow$posterior.VANDALISM'))
            sum <- log(predrow$posterior.VANDALISM) + sum
	    else if(rowCV$Category=="VEHICLE THEFT" && !is.na('predrow$posterior.VEHICLE.THEFT'))
            sum <- log(predrow$posterior.VEHICLE.THEFT) + sum
	    else if(rowCV$Category=="WARRANTS" && !is.na('predrow$posterior.WARRANTS'))
            sum <- log(predrow$posterior.WARRANTS) + sum
	    else if(rowCV$Category=="WEAPON LAWS" && !is.na('predrow$posterior.WEAPON.LAWS'))
            sum <- log(predrow$posterior.WEAPON.LAWS) + sum
		else {
			print("hit last else")
			write(i, "")
			write(k, "")
		}
	}
	logloss<- (-sum)/nrow(CV_CV)
	print("ith logloss is")
	write(logloss,"")
	logloss_sum<-logloss_sum+logloss
#2.547 for LDA
#3.54 when using dplyr to create training and validation set similar to RF
}

logloss_total<-logloss_sum/10 #or k, if k is not 10
#got logloss_total=26.68 => avg=2.668 for 10-fold CV
#1st place on kaggle is 2.11, 62nd is 2.40



predictions_LDAsubmission <- predict(lda.fit, testDF_subset, type = "prob")
predictions_LDAsubmission <- data.frame(predictions_LDAsubmission)
predictions_LDAsubmission<-predictions_LDAsubmission[,names(predictions_LDAsubmission)[-c(1,41,42,43)]]
#submissionfile <- data.frame(id = testDF_subset, pred = predictions_LDAsubmission)
#submission<-submissionfile[,names(submissionfile)[-c(1,2,3,4)]]
submissionfile <- data.frame(predictions_LDAsubmission)

id<-c(0:884261)
submission<-cbind(id,submission)
#submission<-submission[,names(submission)[-c(37,38,39)]]
colnames(submission) <- c("Id","ARSON", "ASSAULT", "BAD CHECKS","BRIBERY","BURGLARY","DISORDERLY CONDUCT",
"DRIVING UNDER THE INFLUENCE", "DRUG/NARCOTIC",
"DRUNKENNESS","EMBEZZLEMENT","EXTORTION", "FAMILY OFFENSES","FORGERY/COUNTERFEITING",
"FRAUD","GAMBLING","KIDNAPPING","LARCENY/THEFT","LIQUOR LAWS","LOITERING",
"MISSING PERSON","NON-CRIMINAL","OTHER OFFENSES","PORNOGRAPHY/OBSCENE MAT",
"PROSTITUTION","RECOVERED VEHICLE","ROBBERY","RUNAWAY","SECONDARY CODES","SEX OFFENSES FORCIBLE",
"SEX OFFENSES NON FORCIBLE","STOLEN PROPERTY","SUICIDE","SUSPICIOUS OCC","TREA","TRESPASS","VANDALISM",
"VEHICLE THEFT", "WARRANTS","WEAPON LAWS")
write.csv(submission, file = "LDA_submission.csv", row.names = FALSE)



#try RF	  
trainDF_dplyr<- group_by(trainDF_subset, Category) %>% sample_n(200, replace=F)


CV_CV<-trainDF_subset[1:50000,]
#if CV_CV is too small, can get numeric(0) errors because
#if predictions contains for example "ARSON" but CV_CV doesn't,
#then if(rowCV$Category=="ARSON") will get missed

#trainDF_subset<-trainDF_subset[600001:878049,]
trainDF_subset<-trainDF_dplyr
trainDF_subset$Category <- factor(trainDF_subset$Category)
table(trainDF_subset$Category)

CV_woCategory<-CV_CV[,names(CV_CV)[-c(1)]]
#use validation set without "Category"
#because you will use Training set to make predictions on 
#validation set without "Category" and then compare those predictions
#with the actual "Category" values in CV_CV 


#try using only alot less of training set since RF crashes when using only 10 trees
library(randomForest)
formula = as.formula(Category ~ timeDiff + X+Y+DayOfWeek)
#exclude DayOfWeek if dataset too large
RF = randomForest(formula, data=trainDF_subset, proximity=FALSE, mtry=2, ntree=5000, importance=TRUE)
#don't include "PdDistrict" because importance(rf,type) with both type=1 and 2 showed it was by far least important predictor
#could also exclude "DayOfWeek" as timeDiff,X and Y have greater importance values than it

predictions_RF <- predict(RF, CV_woCategory, type = "prob")
predictions <- data.frame(predictions_RF)
#predictions<-predictions[,names(predictions)[-c(1,41,42,43)]]
#don't need "class" and linear discriminant coefficients

subfile <- data.frame(id = CV_woCategory, pred=predictions)
#subfile <- data.frame(id = testDF_subset, pred = predictions)
write.csv(subfile, file = "RF_model_CV.csv", row.names = FALSE)

#to see how well model matched CV data
#final_LDA<-merge(predictions,CV_CV$Category) #merge predictions with actual results
#remove NA
#final_LDA<-final_LDA[complete.cases(final_LDA),]
#get results of accuracy


#IF GET ERROR BELOW ABOUT NON-NUMERIC ARGUMENT (EX. ERROR IN LOG(predrow$ARSON))
#IT IS BECAUSE THAT VARIABLE IS MISSING IN trainDF_subset<-trainDF_subset[600001:878049,]
#SO JUST RE-RUN THE FORLOOP BELOW, BUT WITHOUT THE MISSING PREDICTOR

#for(i in c(1:nrow(final_LDA))) {
eps=.0001
sum=0

#RF is wrong because the logloss value is too high (-170 when using eps=.1
# on 100 rows), so its predictions are wrong
#RF may be giving bad predictions because ntree=10 is too low
for(i in c(1:nrow(CV_CV))) {
    #row <- final_LDA[i, ]
	rowCV <- CV_CV[i, ]
	predrow<-predictions[i,]
    if(rowCV$Category=="ARSON" && !is.na('predrow$ARSON')) {
    #sum <- row$pred*row$Win + sum
	  predicted=predrow$ARSON
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
    } else if(rowCV$Category=="ASSAULT" && !is.na('predrow$ASSAULT')) {
      predicted=predrow$ASSAULT
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
    } else if(rowCV$Category=="BAD CHECKS" && !is.na('predrow$BAD.CHECKS')) {
      predicted=predrow$BAD.CHECKS
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="BRIBERY" && !is.na('predrow$BRIBERY')) {
      predicted=predrow$BRIBERY
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="BURGLARY" && !is.na('predrow$BURGLARY')) {
      predicted=predrow$BURGLARY
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="DISORDERLY CONDUCT" && !is.na('predrow$DISORDERLY.CONDUCT')) {
      predicted=predrow$DISORDERLY.CONDUCT
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="DRIVING UNDER THE INFLUENCE" && !is.na('predrow$DRIVING.UNDER.THE.INFLUENCE')) {
      predicted=predrow$DRIVING.UNDER.THE.INFLUENCE
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="DRUG/NARCOTIC" && !is.na('predrow$DRUG.NARCOTIC')) {
      predicted=predrow$DRUG.NARCOTIC
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="DRUNKENNESS" && !is.na('predrow$DRUNKENNESS')) {
      predicted=predrow$DRUNKENNESS
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="EMBEZZLEMENT" && !is.na('predrow$EMBEZZLEMENT')) {
      predicted=predrow$EMBEZZLEMENT
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="EXTORTION" && !is.na('predrow$EXTORTION')) {
      predicted=predrow$EXTORTION
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="FAMILY OFFENSES" && !is.na('predrow$FAMILY.OFFENSES')) {
      predicted=predrow$FAMILY.OFFENSES
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="FORGERY/COUNTERFEITING" && !is.na('predrow$FORGERY.COUNTERFEITING')) {
      predicted=predrow$FORGERY.COUNTERFEITING
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="FRAUD" && !is.na('predrow$FRAUD')) {
      predicted=predrow$FRAUD
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="GAMBLING" && !is.na('predrow$GAMBLING')) {
      predicted=predrow$GAMBLING
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="KIDNAPPING" && !is.na('predrow$KIDNAPPING')) {
      predicted=predrow$KIDNAPPING
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="LARCENY/THEFT" && !is.na('predrow$LARCENY.THEFT')) {
      predicted=predrow$LARCENY.THEFT
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="LIQUOR LAWS" && !is.na('predrow$LIQUOR.LAWS')) {
      predicted=predrow$LIQUOR.LAWS
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="LOITERING" && !is.na('predrow$LOITERING')) {
      predicted=predrow$LOITERING
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="MISSING PERSON" && !is.na('predrow$MISSING.PERSON')) {
      predicted=predrow$MISSING.PERSON
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="NON-CRIMINAL" && !is.na('predrow$NON.CRIMINAL')) {
      predicted=predrow$NON.CRIMINAL
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="OTHER OFFENSES" && !is.na('predrow$OTHER.OFFENSES')) {
      predicted=predrow$OTHER.OFFENSES
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="PORNOGRAPHY/OBSCENE MAT" && !is.na('predrow$PORNOGRAPHY.OBSCENE')) {
      predicted=predrow$PORNOGRAPHY.OBSCENE.MAT
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="PROSTITUTION" && !is.na('predrow$PROSTITUTION')) {
      predicted=predrow$PROSTITUTION
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="RECOVERED VEHICLE" && !is.na('predrow$RECOVERED.VEHICLE')) {
      predicted=predrow$RECOVERED.VEHICLE
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="ROBBERY" && !is.na('predrow$ROBBERY')) {
      predicted=predrow$ROBBERY
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="RUNAWAY" && !is.na('predrow$RUNAWAY')) {
      predicted=predrow$RUNAWAY
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="SECONDARY CODES" && !is.na('predrow$SECONDARY.CODES')) {
      predicted=predrow$SECONDARY.CODES
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="SEX OFFENSES FORCIBLE" && !is.na('predrow$SEX.OFFENSES.FORCIBLE')) {
      predicted=predrow$SEX.OFFENSES.FORCIBLE
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="SEX OFFENSES NON FORCIBLE" && !is.na('predrow$SEX.OFFENSES.NON.FORCIBLE')) {
      predicted=predrow$SEX.OFFENSES.NON.FORCIBLE
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="STOLEN PROPERTY" && !is.na('predrow$STOLEN.PROPERTY')) {
      predicted=predrow$STOLEN.PROPERTY
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="SUICIDE" && !is.na('predrow$SUICIDE')) {
      predicted=predrow$SUICIDE
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="SUSPICIOUS OCC" && !is.na('predrow$SUSPICIOUS.OCC')) {
      predicted=predrow$SUSPICIOUS.OCC
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="TREA" && !is.na('predrow$TREA')) {
      predicted=predrow$TREA
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="TRESPASS" && !is.na('predrow$TRESPASS')) {
      predicted=predrow$TRESPASS
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="VANDALISM" && !is.na('predrow$VANDALISM')) {
      predicted=predrow$VANDALISM
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="VEHICLE THEFT" && !is.na('predrow$VEHICLE.THEFT')) {
      predicted=predrow$VEHICLE.THEFT
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="WARRANTS" && !is.na('predrow$WARRANTS')) {
      predicted=predrow$WARRANTS
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else if(rowCV$Category=="WEAPON LAWS" && !is.na('predrow$WEAPONS.LAWS')) {
      predicted=predrow$WEAPON.LAWS
	  predicted <- pmin(pmax(predicted, eps), 1-eps)
      sum <- log(predicted) + sum
	} else {
	  print("hit last else")
	  write(i, "")
	}
}
logloss<- (-sum)/nrow(CV_CV)
#got -3.18 for 1st 100 rows of trainDF_subset<-trainDF_subset[850001:878049,] 
#and ntree=100

#got 3.856 for CV_CV<-trainDF_subset[1:50000,],
#trainDF_dplyr<- group_by(trainDF_subset, Category) %>% sample_n(6, replace=F)
#and ntree=5000

#4.856 for CV_CV<-trainDF_subset[1:50000,], 
#trainDF_dplyr<- group_by(trainDF_subset, Category) %>% sample_n(200, replace=F)
#and ntree=5000

#after CV
subfile <- data.frame(id = testDF, pred = predictions)
subfile<-subfile[,names(subfile)[-c(2)]] 
#only need prob of win, while the 2nd column was prob of loss
write.csv(subfile, file = "RF_model.csv", row.names = FALSE)


