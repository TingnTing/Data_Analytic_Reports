Appendix
# set working directory
setwd("/Users/aliciang/Downloads/FIT3152-DATAANALYTICS/FIT3152_Assignment2")
install.packages("rpart")
install.packages("tree")
install.packages("e1071") # for naivebayes in Q4
install.packages("adabag") # for bagging in Q4
install.packages("ROCR") # for prediction in Q6
install.packages("randomForest") # for randomForest in Q4
install.packages("pROC") # for roc in NN
library(tree)
library(e1071)
library(adabag)
library(rpart)
library(ROCR)
library(randomForest)
library(pROC)
rm(list = ls())
WAUS <- read.csv("HumidPredict2023D.csv")
L <- as.data.frame(c(1:49))
set.seed(31861148) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample
2000 rows
# Question 1 -------------------------------------------------------
------------
  dim(WAUS) # Output : 2000 rows 22 columns
str(WAUS) # Output : 4 char, 10 num, 8 int
# Proportion of Humidity values
sum(WAUS$MHT == 1, na.rm = TRUE) / 20 # Output : 46.05% of rows
where MHT is 1
sum(WAUS$MHT == 0, na.rm = TRUE) / 20 # Output : 45.4% of rows
where MHT is 0
# Descriptions of independent variables
summary(WAUS)
# Question 2 -------------------------------------------------------
------------
  # Turns all char type to factors
  WAUS <- as.data.frame(unclass(WAUS),stringsAsFactors=T)
# Change Class to a factor (not needed for NN)
WAUS$MHT <- as.factor(WAUS$MHT)

# Remove all rows where MHT is NA (not needed for NN)
WAUS <- WAUS[!is.na(WAUS$MHT), ]
dim(WAUS) # Output : 1829 rows 22 columns
# Remove Year Column as redundant for data training
WAUS <- WAUS[,2:22]
# Question 3 -------------------------------------------------------
------------
  set.seed(31861148) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]
# Question 4 -------------------------------------------------------
------------
  # Decision Tree
  DecisionTreeModel <- tree(MHT ~ ., data = WAUS.train)
# Naive Bayes
NaiveBayesModel <- naiveBayes(MHT ~. - MHT, data = WAUS.train)
# Bagging
set.seed(31861148)
BaggingModel <- bagging(MHT ~ ., data = WAUS.train, mfinal = 5)
# Boosting
set.seed(31861148)
BoostingModel <- boosting(MHT ~ .-MHT, data = WAUS.train, mfinal =
                            10)
# Random Forest
WAUS.train <- na.omit(WAUS.train) # because random forest doesn't
work on NA
set.seed(31861148)
RandomForestModel <- randomForest(MHT ~ . -MHT, data = WAUS.train)
# reset
WAUS.train = WAUS[train.row,]
# Question 5 -------------------------------------------------------
------------
  # List to display accuracy in Q7
  ModelAccuracyList = list()
# function to calculate accuracy
acc_func = function(cm) {
  
  TP <- cm[2, 2]
  TN <- cm[1, 1]
  FP <- cm[1, 2]
  FN <- cm[2, 1]
  
  
  # Calculate the accuracy
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  
  return(accuracy)
}
# Decision Tree
DecisionTreePred <- predict(DecisionTreeModel, newdata = WAUS.test,
                            type = "class")
DecisionTreeConf <- table(observed = WAUS.test$MHT,
                          predicted = DecisionTreePred)
ModelAccuracyList = append(ModelAccuracyList,
                           acc_func(DecisionTreeConf))
# Naive Bayes
NaiveBayesPred <- predict(NaiveBayesModel, WAUS.test)
NaiveBayesConf <- table(actual = WAUS.test$MHT, predicted =
                          NaiveBayesPred)
ModelAccuracyList = append(ModelAccuracyList,
                           acc_func(NaiveBayesConf))
# Bagging
BaggingPred = predict.bagging(BaggingModel, newdata = WAUS.test)
ModelAccuracyList = append(ModelAccuracyList,
                           acc_func(BaggingPred$confusion))
# Boosting
BoostingPred = predict.boosting(BoostingModel, newdata = WAUS.test)
ModelAccuracyList = append(ModelAccuracyList,
                           acc_func(BoostingPred$confusion))
# Random Forest
WAUS.test = na.omit(WAUS.test)
RandomForestPred <- predict(RandomForestModel, newdata = WAUS.test,
                            type = "class")
RandomForestConf <- table(observed = WAUS.test$MHT,
                          predicted = RandomForestPred)
ModelAccuracyList = append(ModelAccuracyList,
                           acc_func(RandomForestConf))
# reset due to Random Forest
WAUS.test = WAUS[-train.row,]
# Question 6 -------------------------------------------------------
------------
  # List to display AUC in Q7
  AucOfModelList = list()
WAUS.test$MHT <- as.numeric(WAUS.test$MHT) # dont need for DT
# Decision Tree's ROC
DecisionTreeConf <- predict(DecisionTreeModel, WAUS.test, type =
                              "vector")
DecisionTreePredObj <- prediction(DecisionTreeConf[,2],
                                  WAUS.test$MHT)

DecisionTreePerf <- performance(DecisionTreePredObj, "tpr", "fpr")
plot(DecisionTreePerf, col = "red", main = "ROC curve of the
models")
abline(0,1)
cauc = performance(DecisionTreePredObj, "auc")
AucOfModelList = append(AucOfModelList, as.numeric(cauc@y.values))
# Naive Bayes' ROC
NaiveBayesConf <- predict(NaiveBayesModel, WAUS.test, type = 'raw')
NaiveBayesPredObj <- prediction( NaiveBayesConf[,2], WAUS.test$MHT)
NaiveBayesPerf <- performance(NaiveBayesPredObj,"tpr","fpr")
plot(NaiveBayesPerf, col ="orange", add = TRUE)
cauc = performance(NaiveBayesPredObj, "auc")
AucOfModelList = append(AucOfModelList, as.numeric(cauc@y.values))
# Bagging's ROC
BaggingConf <- BaggingPred$prob
BaggingPredObj <- prediction( BaggingConf[,2], WAUS.test$MHT)
BaggingPerf <- performance(BaggingPredObj,"tpr","fpr")
plot(BaggingPerf, col ="green", add = TRUE)
cauc = performance(BaggingPredObj, "auc")
AucOfModelList = append(AucOfModelList, as.numeric(cauc@y.values))
# Boosting's ROC
BoostingConf <- BoostingPred$prob
BoostingPredObj <- prediction( BoostingConf[,2], WAUS.test$MHT)
BoostingPerf <- performance(BoostingPredObj,"tpr","fpr")
plot(BoostingPerf, col ="blue", add = TRUE)
cauc = performance(BoostingPredObj, "auc")
AucOfModelList = append(AucOfModelList, as.numeric(cauc@y.values))
# Random Forest's ROC
WAUS.test = na.omit(WAUS.test)
RandomForestConf = predict(RandomForestModel, WAUS.test, type =
                             "prob")
RandomForestPredObj <- prediction( RandomForestConf[,2],
                                   WAUS.test$MHT)
RandomForestPerf <- performance(RandomForestPredObj,"tpr","fpr")
plot(RandomForestPerf, col ="purple", add = TRUE)
cauc = performance(RandomForestPredObj, "auc")
AucOfModelList = append(AucOfModelList, as.numeric(cauc@y.values))
# reset
WAUS.test = WAUS[-train.row,]
# To add legend
legend("bottomright",
       legend = c("Decision Tree","Naive Bayes", "Bagging",
                  "Boosting",
                  "Random Forest"),
       col = c("red", "orange", "green","blue","purple"),
       lty = 1)
# Function to get back the ROC plot for Decision Tree and Random
Forest
getBackPlot = function() { plot(DecisionTreePerf, col = "red", main = "ROC curve of the
models")
  plot(RandomForestPerf, col ="purple", add = TRUE)
  abline(0,1)
  
}
# Question 7 -------------------------------------------------------
------------
  # table summary
  ModelNames = c("Decision Tree", "Naive Bayes", "Bagging",
                 "Boosting",
                 "Random Forest")
Q7Summary = cbind(ModelAccuracyList, AucOfModelList)
Q7Summary = cbind(ModelNames, Q7Summary)
Q7Summary <- data.frame(Q7Summary[,-1], row.names = Q7Summary[,1])
Q7Summary
# Question 8 -------------------------------------------------------
------------
  # Important attributes for Decision tree
  summary(DecisionTreeModel)
# Explanation : Given the summary these 12 are meaningful
BaggingModel$importance
BoostingModel$importance
RandomForestModel$importance
import <- RandomForestModel$importance
import[order(import, decreasing=TRUE),,drop = FALSE]
# WAUS.train w/o Location, RainToday, RISKMM
WAUS.trainQ10 = WAUS.train[,-c(1)]
colnames(WAUS.trainQ10)
# Question 9 -------------------------------------------------------
------------
  # Simplified Decision Tree
  SimpleDecisionTreeModel <- tree(MHT ~ WindGustDir + MaxTemp +
                                    WindDir9am + WindSpeed3pm + RISK_MM + WindSpeed9am +
                                    Pressure9am +
                                    Temp9am + Rainfall , data = WAUS.train)
plot(DecisionTreeModel)
text(DecisionTreeModel, pretty = 0)
summary(DecisionTreeModel)
# type = class for classification
SimpleDecisionTreePred <- predict(SimpleDecisionTreeModel, newdata =
                                    WAUS.test,
                                  type = "class")
acc_func(table(observed = WAUS.test$MHT, predicted =
                 SimpleDecisionTreePred))
# Getting Back Plot
getBackPlot()
SimpleDecisionTreeConf <- predict(SimpleDecisionTreeModel,
                                  WAUS.test,
                                  type = "vector")
SimpleDecisionTreePredObj <- prediction(SimpleDecisionTreeConf[,2],
                                        WAUS.test$MHT)
SimpleDecisionTreePerf <- performance(SimpleDecisionTreePredObj,
                                      "tpr", "fpr")
plot(SimpleDecisionTreePerf, col = "green", add = TRUE)
abline(0,1)
cauc = performance(SimpleDecisionTreePredObj, "auc")
as.numeric(cauc@y.values)
legend("bottomright",
       legend = c("Decision Tree","Random Forest","Simple Tree"),
       col = c("red","purple", "green"),
       lty = 1)
# Question 10 ------------------------------------------------------
------------
  # Cross Validation for Random Forest - plot shows that the model
  # does well with most of the predictors
  set.seed(31861148)
WAUS.trainOmitted = na.omit(WAUS.train)
set.seed(31861148)
RandomForestCV <- rfcv(WAUS.trainOmitted[,1:20],
                       WAUS.trainOmitted$MHT)
with(RandomForestCV, plot(n.var, error.cv, log="x", type="o", lwd=2,
                          xlab="Number of Variables", ylab="Error
Rate",
                          main = "Best Number of Variables in Random
Forest"))
# The best choice is the remove predictors from model one by one
# Order importance of predictors
import <- RandomForestModel$importance
import[order(import, decreasing=TRUE),,drop = FALSE]
### ACTUAL RANDOM FOREST MODEL ###
WAUS.train = na.omit(WAUS.train)
# Removed NA rows, Location and WindDir3pm , maybe 3
WAUS.trainQ10 = WAUS.train[,-c(1,2,3,10)]
# Restore back original value
WAUS.train = WAUS[train.row,]
set.seed(31861148)
RandomForestModel <- randomForest(MHT~., data = WAUS.trainQ10, ntree
                                  = 2500,
                                  max.depth = 150, min.node.size =
                                    2)
WAUS.test = na.omit(WAUS.test)
RandomForestPred <- predict(RandomForestModel, newdata = WAUS.test,
                            type = "class")
# Confusion Matrix, Accuracy = 0.674603
RandomForestConf <- table(observed = WAUS.test$MHT,
                          predicted = RandomForestPred)
acc_func(RandomForestConf)
WAUS.test = WAUS[-train.row,]
# Area under curve = 0.6442602
getBackPlot()
WAUS.test = na.omit(WAUS.test)
RandomForestCVConf = predict(RandomForestModel, WAUS.test, type =
                               "prob")
RandomForestPredObj <- prediction( RandomForestCVConf[,2],
                                   WAUS.test$MHT)
RandomForestPerf <- performance(RandomForestPredObj,"tpr","fpr")
plot(RandomForestPerf, col ="blue", add =TRUE)
cauc = performance(RandomForestPredObj, "auc")
AucOfModelList = append(AucOfModelList, as.numeric(cauc@y.values))
# Question 11 ------------------------------------------------------
------------
  # Note :it can't be placed at the top as it has the prediction()
  # function too and it will mask the function in Question 6
  install.packages("neuralnet")
install.packages("caret")
library(neuralnet)
library(caret)
# Artificial Neural Network
# Pre-processing
WAUSANN = WAUS[complete.cases(WAUS),] # remove NA rows
WAUSANN$MHT = as.numeric(WAUSANN$MHT) # change MHT to numeric
numeric_cols <- sapply(WAUSANN, is.numeric)
set.seed(31861148) #Student ID as random seed
WAUSANN[, numeric_cols] <- scale(WAUSANN[, numeric_cols])
# train and test set
set.seed(31861148) #Student ID as random seed
train.row = sample(1:nrow(WAUSANN), 0.7*nrow(WAUSANN))
WAUSANN.train = WAUSANN[train.row,]
WAUSANN.test = WAUSANN[-train.row,]
# Creating indicators for training set
MHT = WAUSANN.train[,21]
WAUSANN.train = WAUSANN.train[,1:20]
dummy <- dummyVars(" ~ .", data = WAUSANN.train)
newWAUSANN.train <- data.frame(predict(dummy, newdata =
                                         WAUSANN.train))
WAUSANN.train = cbind(newWAUSANN.train, MHT)
# Creating indicators for testing set
MHT = WAUSANN.test[,21]
WAUSANN.test = WAUSANN.test[,1:20]
dummy <- dummyVars(" ~ .", data = WAUSANN.test)
newWAUSANN.test <- data.frame(predict(dummy, newdata =
                                        WAUSANN.test))
WAUSANN.test = cbind(newWAUSANN.test, MHT)
dim(WAUSANN.train)
dim(WAUSANN.test)
### NEURAL NETWORK MODEL ###
set.seed(31861148)
WAUS.nn = neuralnet(MHT ~ MinTemp+ MaxTemp+ Rainfall+ Evaporation+
                      Sunshine+ WindGustDir.E+ WindGustDir.ENE+
                      WindGustDir.ESE+
                      WindGustDir.N+ WindGustDir.NE+
                      WindGustDir.NNE+
                      WindGustDir.NNW+ WindGustDir.NW+
                      WindGustDir.S+
                      WindGustDir.SE+ WindGustDir.SSE+
                      WindGustDir.SSW+ WindGustDir.SW+
                      WindGustDir.W+
                      WindGustDir.WNW+ WindGustDir.WSW+
                      WindGustSpeed+
                      WindDir9am.E+ WindDir9am.ENE+
                      WindDir9am.ESE+WindDir9am.N+
                      WindDir9am.NE+ WindDir9am.NNE+WindDir9am.NNW+
                      WindDir9am.NW+ WindDir9am.S+ WindDir9am.SE+
                      WindDir9am.SSE+ WindDir9am.SSW+ WindDir9am.SW+
                      WindDir9am.W+ WindDir9am.WNW+ WindDir9am.WSW +
                      WindSpeed9am+ WindSpeed3pm+
                      Pressure9am+ Pressure3pm+ Cloud9am+ Cloud3pm+
                      Temp9am+
                      Temp3pm+ RainToday.No+ RainToday.Yes+ RISK_MM,
                    WAUSANN.train,
                    hidden=3, err.fct ="sse", threshold = 0.05,
                    linear.output = TRUE)
plot(WAUS.nn, rep="best")
WAUS.nn$result.matrix
WAUS.pred = compute(WAUS.nn, WAUSANN.test[,1:66])
# Cut off point to label them to 0 and 1
WAUS.predRescaled <- ifelse(WAUS.pred$net.result <= 0.5, 0, 1)
# Confusion matrix
nnConf <- table(observed = WAUSANN.test$MHT, predicted =
                  WAUS.predRescaled)
acc_func(nnConf)
# ROC
# Note : Install package pROC for this to work
auc <- roc(WAUSANN.test$MHT, WAUS.predRescaled)$auc
NeuralNetworkROC <- roc(WAUSANN.test$MHT, WAUS.predRescaled)
plot(NeuralNetworkROC, col = "blue", main = "ROC Curve")
# Question 12 ------------------------------------------------------
------------
  # XGBoost packages
  install.packages("caret")
install.packages("xgboost")
install.packages("mltools")
install.packages("data.table")
library(caret) # Machine Learning Library, for dummyVars
library(xgboost) # XGBoost library
library(mltools)
library(data.table)
# Can reused the indicators made in Q11 but have to change them back
to factors
WAUSANN.train$MHT <- as.factor(WAUSANN.train$MHT)
# Resampling for 400 test data
set.seed(31861148)
WAUSANN.test <- WAUSANN.test[sample(nrow(WAUSANN.test), size = 400,
                                    replace = TRUE), ]
WAUSANN.test$MHT <- as.factor(WAUSANN.test$MHT)
### Cross Validation for XGBoost ###
# Basic Grid Tuning
grid_tune <- expand.grid(
  nrounds = c(500,1000,1500), # number of trees
  max_depth = c(2,4,6),
  eta = 0.3, #Learning rate
  gamma = 0, # pruning
  colsample_bytree = 1, # subsample ratio of columns for tree
  min_child_weight = 1,
  subsample = 1 # to prevent overfitting by sampling X% training
)
# Cross Validation settings
train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
# Training Cross Validation model
set.seed(31861148)
xgb_tune <- train(x = WAUSANN.train[,-67],
                  y = WAUSANN.train[,67],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE)
# Best tune parameters
xgb_tune$bestTune
### Getting best model based on Cross Validation ###
# Update model settings for final model
train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
# New settings based on bestTune from Cross Validation
final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree =
                            xgb_tune$bestTune$colsample_bytree,
                          min_child_weight =
                            xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)
# Training final XGBoost model
set.seed(31861148)
xgb_model <- train(x = WAUSANN.train[,-67],
                   y = WAUSANN.train[,67],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)
# Predicting test data
xgb.pred <- predict(xgb_model, WAUSANN.test)
# Confusion Matrix
confusionMatrix(as.factor(as.numeric(xgb.pred)),
                as.factor(as.numeric(WAUSANN.test$MHT)))