library(data.table)
library(lubridate)
trainDF<-fread("train.csv",data.table=FALSE)
testDF<-fread("test.csv",data.table=FALSE)
#setting data.table=FALSE so that trainDF and testDF are data frames instead of data tables

trainDF$Dates<-fast_strptime(trainDF$Dates, format="%Y-%m-%d %H:%M:%S", tz="UTC") 
trainDF$Day<-day(trainDF$Dates) 
trainDF$Month<-month(trainDF$Dates) 
trainDF$Year<-year(trainDF$Dates) 
trainDF$Hour<-hour(trainDF$Dates) 
trainDF$Minute<-minute(trainDF$Dates) 
trainDF$Second<-second(trainDF$Dates) 

#got idea from https://brittlab.uwaterloo.ca/2015/11/01/KaggleSFcrime/
trainDF$Night<-ifelse(trainDF$Hour > 22 | trainDF$Hour < 6,1,0)
trainDF$Intersection<-grepl("/", trainDF$Address) 
trainDF$Intersection<-plyr::mapvalues(trainDF$Intersection,from=c("TRUE","FALSE"),to=c(1,0)) 

trainDF_subset<-trainDF[,names(trainDF)[-c(1,3,6,7,10,14,15)]]
#can't try PCA since we have categorial variables
#remove these columns because the 3rd column Descript and 
#6th column Resolution are not in the test set
#the 7th column Address cannot be quantified. Maybe convert it to Zipcode?
#Can also use X and Y instead of the 7th column

testDF$Dates<-fast_strptime(testDF$Dates, format="%Y-%m-%d %H:%M:%S", tz="UTC") 
testDF$Day<-day(testDF$Dates) 
testDF$Month<-month(testDF$Dates) 
testDF$Year<-year(testDF$Dates) 
testDF$Hour<-hour(testDF$Dates)
testDF$Minute<-minute(testDF$Dates) 
testDF$Second<-second(testDF$Dates)

testDF$Night<-ifelse(testDF$Hour > 22 | testDF$Hour < 6,1,0)
testDF$Intersection<-grepl("/", testDF$Address) 
testDF$Intersection<-plyr::mapvalues(testDF$Intersection,from=c("TRUE","FALSE"),to=c(1,0)) 

testDF_subset<-testDF[,names(testDF)[-c(1,2,5,8)]]

#convert to sparse matrix
index <- sample(1:nrow(trainDF), 600000)
trainDF_subsetCV<-trainDF_subset[index,]
categoryMatrix<-data.frame(with(trainDF_subsetCV,model.matrix(~Category+0))) 
names(categoryMatrix)<-sort(unique(trainDF$Category)) 
trainDF_subsetCV<-cbind(categoryMatrix,trainDF_subsetCV)
library(glmnet)
#got idea from https://brittlab.uwaterloo.ca/2015/11/01/KaggleSFcrime/
sparse.mat.tr<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+DayOfWeek+Intersection+Night,data=trainDF_subsetCV) 
object.size(sparse.mat.tr)
object.size(m) #m is the model matrix for the features, used in xgboost
#m uses about 2x memory as sparse.mat.tr
object.size(categoryMatrix)
#same as model.matrix(~Category, data=trainDF_subset)

#LDA
library('MASS')


inds=(1:870000) #don't use "sample" because that takes random sample
trainDF_subsetCV<-list()
trainDF_subsetCV<-trainDF_subset[inds,]
trainDF_subsetCV$Category <- factor(trainDF_subsetCV$Category)
table(trainDF_subsetCV$Category)

lda.fit =lda(Category ~ PdDistrict + Month + DayOfWeek + Hour + X + Y +Night, data=trainDF_subsetCV)

predictions_LDAsubmission <- predict(lda.fit, testDF_subset, type = "prob")
predictions_LDAsubmission <- data.frame(predictions_LDAsubmission)
predictions_LDAsubmission1<-predictions_LDAsubmission[,names(predictions_LDAsubmission)[-c(1,41:61)]]

#submissionfile <- data.frame(id = testDF_subset, pred = predictions_LDAsubmission)
#submission<-submissionfile[,names(submissionfile)[-c(1,2,3,4)]]
submissionfile <- data.frame(predictions_LDAsubmission1)

id<-c(0:884261)
submission<-cbind(id,submissionfile)
#submission<-submission[,names(submission)[-c(37,38,39)]]
colnames(submission) <- c("Id","ARSON", "ASSAULT", "BAD CHECKS","BRIBERY","BURGLARY","DISORDERLY CONDUCT",
                          "DRIVING UNDER THE INFLUENCE", "DRUG/NARCOTIC",
                          "DRUNKENNESS","EMBEZZLEMENT","EXTORTION", "FAMILY OFFENSES","FORGERY/COUNTERFEITING",
                          "FRAUD","GAMBLING","KIDNAPPING","LARCENY/THEFT","LIQUOR LAWS","LOITERING",
                          "MISSING PERSON","NON-CRIMINAL","OTHER OFFENSES","PORNOGRAPHY/OBSCENE MAT",
                          "PROSTITUTION","RECOVERED VEHICLE","ROBBERY","RUNAWAY","SECONDARY CODES","SEX OFFENSES FORCIBLE",
                          "SEX OFFENSES NON FORCIBLE","STOLEN PROPERTY","SUICIDE","SUSPICIOUS OCC","TREA","TRESPASS","VANDALISM",
                          "VEHICLE THEFT", "WARRANTS","WEAPON LAWS")
write.csv(submission, file = "LDAimproved.csv", row.names = FALSE)

trainDF_dplyr<- group_by(trainDF_subset, Category) %>% sample_n(10000, replace=TRUE)



#RANDOM FOREST
trainDF_dplyr<- group_by(trainDF_subset, Category) %>% sample_n(140, replace=TRUE)
#get 140 data points for each category, including low-number ones like TREA,PORNOGRAPHY,etc
#Night IS included. can't see with Tail, but CAN with Head

#inds=(1:150000) #don't use "sample" because that takes random sample
trainDF_subsetCV<-list()
#trainDF_subsetCV<-trainDF_subset[inds,]
trainDF_subsetCV<-trainDF_dplyr
trainDF_subsetCV$Category <- factor(trainDF_subsetCV$Category)
table(trainDF_subsetCV$Category)
formula = as.formula(Category ~ PdDistrict +DayOfWeek +Intersection + X + Y +Night)
RF = randomForest(formula, data=trainDF_subsetCV, proximity=FALSE, mtry=3, ntree=1000, importance=TRUE)

predictions_RF <- predict(RF, testDF_subset, type = "prob")
predictions <- data.frame(predictions_RF)
id<-c(0:884261)
submission1<-cbind(id,predictions)
#if RECOVEREDVEHICLE and PORNOGRAPHY are 0
t<-c(0:884261)
submission1<-cbind(submission1, t)
submission1<-submission1[,c(1:23,39,24:38)]
v<-c(0:884261)
submission1<-cbind(submission1, v)
submission1<-submission1[,c(1:25,40,26:39)]
submission1$t<-0
submission1$v<-0
colnames(submission1) <- c("Id","ARSON", "ASSAULT", "BAD CHECKS","BRIBERY","BURGLARY","DISORDERLY CONDUCT",
                           "DRIVING UNDER THE INFLUENCE", "DRUG/NARCOTIC",
                           "DRUNKENNESS","EMBEZZLEMENT","EXTORTION", "FAMILY OFFENSES","FORGERY/COUNTERFEITING",
                           "FRAUD","GAMBLING","KIDNAPPING","LARCENY/THEFT","LIQUOR LAWS","LOITERING",
                           "MISSING PERSON","NON-CRIMINAL","OTHER OFFENSES","PORNOGRAPHY/OBSCENE MAT",
                           "PROSTITUTION","RECOVERED VEHICLE","ROBBERY","RUNAWAY","SECONDARY CODES","SEX OFFENSES FORCIBLE",
                           "SEX OFFENSES NON FORCIBLE","STOLEN PROPERTY","SUICIDE","SUSPICIOUS OCC","TREA","TRESPASS","VANDALISM",
                           "VEHICLE THEFT", "WARRANTS","WEAPON LAWS")
write.csv(submission1, file = "RF_submissionIMPROVED.csv", row.names = FALSE)
#score of about 3.00, much worse than LDA

#also try this for RF
index <- sample(1:nrow(trainDF), 100000)
trainDF_subsetCV<-trainDF_subset[index,]
formula = as.formula(Category ~ PdDistrict + DayOfWeek + X + Y +Night+Intersection)
RF = randomForest(formula, data=trainDF_subsetCV, proximity=FALSE, mtry=3, ntree=1000, importance=TRUE)



#BIG RF
registerDoParallel(cores=detectCores(all.tests=TRUE))
trainDF_bigrf<-trainDF_subset[,names(trainDF_subset)[-c(6,7,8)]]
trainDF_bigrfx<-trainDF_bigrf[,names(trainDF_bigrf)[-c(1)]]
bigforest <- bigrfc(trainDF_bigrfx, trainDF_bigrf$Category, ntree=5000)


#NEURAL NETWORKS
library(nnet)
library(caret)
library(doMC)
registerDoMC(4)
index <- sample(1:nrow(trainDF), 600000)
trainDF_subsetCVnn<-trainDF_subset[index,]
#convert all the factor/categorical variables into binary/dummy variables

ynn <- model.matrix(~Category , data = trainDF_subsetCVnn)
mnn <- model.matrix( 
  ~PdDistrict + DayOfWeek + X + Y +Night+Intersection, data = trainDF_subsetCVnn)

#crimenet <- neuralnet(Category ~ PdDistrict + DayOfWeek + X + Y +Night+Intersection, data=mnn, hidden = 4, lifesign = "minimal", 
#                      linear.output = FALSE, threshold = 0.1)
#crimenet<-nnet(mnn, ynn, size=4, maxit = 200, softmax=TRUE)

yCat<-make.names(trainDF_subsetCVnn$Category, unique=FALSE, allow_=TRUE)

nnTrControl=trainControl(method = "cv",number = 2,verboseIter = TRUE,
                         returnData = FALSE, returnResamp = "all", # save losses across all models
                         classProbs = TRUE,
                         summaryFunction = multiClassSummary,allowParallel = TRUE)
nnGrid = expand.grid(.size=c(1,4,7),.decay=c(0,0.001,0.1))

model <- train(y=yCat, x=mnn, method='nnet',linout=TRUE, trace = FALSE,
               trControl = nnTrControl, metric="logLoss", tuneGrid=nnGrid)

plot(crimenet, rep = "best")

#temp_test <- subset(testDF_subset, select = c("LTI", "age"))
temp_test<-testDF_subset
crimenet.results <- compute(crimenet, temp_test)


#Gradient Boosting
library(caret)
library(Metrics)
library(gbm)
library(xgboost)
library(doMC)
registerDoMC(4)
set.seed(999)

#need PdDistrct and DayOfWeek to be converted to Factor
#model.matrix then converts the factors into dummy variables, that is
#Monday = (1,0,0,...), Tuesday = (0,1,0,0,..)
m <- model.matrix( 
  ~ PdDistrict + DayOfWeek + X + Y +Night+Intersection, data = trainDF_subsetCV
)

trainDF_subsetCV$Category<-factor(trainDF_subsetCV$Category)
num.class=length(levels(trainDF_subsetCV$Category))
levels(trainDF_subsetCV$Category)=1:num.class
ynum = as.matrix(as.integer(trainDF_subsetCV$Category)-1)
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss", "nthread" = 4,
              "num_class" = num.class, "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3)    # step size shrinkage 
bst.cv = xgb.cv(param=param, data = m, label = ynum, 
                nfold = 3, nrounds = 20) #nrounds = max number of iterations
#Of the nfold subsamples, a single subsample is retained as the validation data for testing 
#the model, and the remaining nfold - 1 subsamples are used as training data.
#The cross-validation process is then repeated nrounds times, with each of the nfold subsamples
#used exactly once as the validation data

#locate iteration with lowest logloss score on validation set
min.merror.idx = which.min(bst.cv$dt[, test.mlogloss.mean]) 
min.merror.idx
bst.cv$dt[min.merror.idx,]
#best CV was 16th iteration with cv nfolds=3 and logloss.mean=2.56
#now fit all training set, instead of just CV, onto boosting
bst <- xgboost(param=param, data=sparse.mat.tr, label=ynum, nrounds=min.merror.idx, verbose=0)


bst <- xgboost(param=param, data=sparse.mat.tr, label=ynum, nrounds=16, verbose=TRUE)
#nrounds=50, eta=.1
#eta=.01 produces training error too large, due to underfitting perhaps
#results in logloss of 2.43 on Kaggle dashboard page

testDF_subset1<-subset(testDF, select = c("X", "Y", "DayOfWeek","PdDistrict","Intersection","Night"))
matMod.actualtest<-sparse.model.matrix(~as.factor(PdDistrict)+X+Y+DayOfWeek+
                                         Intersection+Night, data=testDF_subset1)
pred <- predict(bst, matMod.actualtest)
head(pred)
prob.matrix <- matrix(pred, ncol = 39, byrow = T)
prob.matrix<-lapply(prob.matrix,round,4)
id<-c(0:884261)
submission1<-cbind(id,prob.matrix)
colnames(submission1) <- c("Id","ARSON", "ASSAULT", "BAD CHECKS","BRIBERY","BURGLARY","DISORDERLY CONDUCT",
                           "DRIVING UNDER THE INFLUENCE", "DRUG/NARCOTIC",
                           "DRUNKENNESS","EMBEZZLEMENT","EXTORTION", "FAMILY OFFENSES","FORGERY/COUNTERFEITING",
                           "FRAUD","GAMBLING","KIDNAPPING","LARCENY/THEFT","LIQUOR LAWS","LOITERING",
                           "MISSING PERSON","NON-CRIMINAL","OTHER OFFENSES","PORNOGRAPHY/OBSCENE MAT",
                           "PROSTITUTION","RECOVERED VEHICLE","ROBBERY","RUNAWAY","SECONDARY CODES","SEX OFFENSES FORCIBLE",
                           "SEX OFFENSES NON FORCIBLE","STOLEN PROPERTY","SUICIDE","SUSPICIOUS OCC","TREA","TRESPASS","VANDALISM",
                           "VEHICLE THEFT", "WARRANTS","WEAPON LAWS")
write.csv(submission1, file = "Boost_sparse.csv", row.names = FALSE)




#determine feature importance
model = xgb.dump(bst, with.stats=TRUE)
# get the feature real names
names = dimnames(sparse.mat.tr)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=bst)
# plot
library(Ckmeans.1d.dp)
gp = xgb.plot.importance(importance_matrix)
print(gp)



#Hypertune the xgboost parameters
xgb_grid_1 = expand.grid(nrounds = 4 #max 10 iterations
                         ,eta = c(0.01, .1, .3) #stepsize shrinkage to prevent overfitting
                         ,max_depth = 3
                         ,gamma = 1 #minimum loss reduction required to make a further partition on a leaf node of the tree.
                         , colsample_bytree = .7,
                         min_child_weight = 1 )
# pack the training control parameters
xgb_trcontrol_1 = trainControl(method = "cv",number = 2,verboseIter = TRUE,
                               returnData = FALSE, returnResamp = "all", # save losses across all models
                               classProbs = TRUE,  # set to TRUE for AUC to be computed
                               summaryFunction = multiClassSummary,allowParallel = TRUE)
# train the model for each parameter combination in the grid, 
#   using CV to evaluate

yCat<-make.names(trainDF_subsetCV$Category, unique=FALSE, allow_=TRUE)
#xgb_train_1 = train(y=yCat, x=m,trControl = xgb_trcontrol_1,
xgb_train_1 = train(y=yCat, x=sparse.mat.tr, trControl = xgb_trcontrol_1,
                    tuneGrid = xgb_grid_1,metric="logLoss",method = "xgbTree")
#eta=.01 better than .0011
#eta=.3 better than .1, .01
#lower values for eta is better for to prevent overfitting
#however, to use lower eta values, must increase nrounds
