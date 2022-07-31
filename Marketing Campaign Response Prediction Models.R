library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)
library(tidymodels)

setwd("/Users/sabrina/Documents/2021/Data Science/Masters_APU/Sem 1/AML/AML Assignment/Dataset")

balanced_train_set <- read.csv('marketing_smice_train.csv')
class(balanced_train_set$Response)
balanced_train_set$Response = as.factor(balanced_train_set$Response)

colnames(balanced_train_set)

test_set <- read.csv('marketing_smice_test.csv')
class(test_set$Response)
test_set$Response = as.factor(test_set$Response)

head(balanced_train_set)
head(test_set)
test_set <- test_set[-c(1)]
balanced_train_set <- balanced_train_set[-c(1)]

#minsplit = 'minimum number of observations that must exist in a node in order for split to be attempted'
#maxbucket = 'minimum number of observations in any terminal node'

#Perform Tree classification
tree = rpart(Response~ ., data=balanced_train_set, method="class") #basic tree

rpart.plot(tree, extra = 101)
printcp(tree)
plotcp(tree)

tree = rpart(Response~ ., data=balanced_train_set, method="class", cp=0.01) 

#Feature Importance
FeatImp = varImp(tree)
FeatImp = FeatImp %>% arrange(desc(Overall))
FeatImp

#Perform prediction with training set
train_pred <- predict(tree, balanced_train_set, type='class')
#Perform prediction with test set
test_pred <- predict(tree, test_set, type='class')

#Confusion Matrix for training set
confusionMatrix(train_pred, balanced_train_set$Response, positive='1')
#Confusion Matrix for test set
confusionMatrix(test_pred, test_set$Response, positive='1')

#plot ROC
test_pred_prob <- predict(tree, test_set, type='prob')
test_pred_prob = test_pred_prob[,2]
r = multiclass.roc(test_set$Response, test_pred_prob, percent = TRUE)
roc = r[['rocs']]
r1 = roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         max.auc.polygon=TRUE,
         auc.polygon.col="light cyan",
         print.thres=TRUE,
         main= 'ROC Curve for Decision Tree')


#Parameter Tuning with Grid Search
tree_grid <- list(minsplit=seq(1,10,2), maxdepth=seq(1,10,2), cp=seq(0.01,1,0.01)) %>% 
  cross_df() # Convert to data frame grid
tree_grid

tree_func <- function (...){tree}

tree_grid = tree_grid %>% mutate(fit = pmap(tree_grid , tree_func))
tree_grid

#Create function to compute accuracy
compute_accuracy <- function(fit, test_features, test_labels) {
  predicted <- predict(fit, test_features, type = "class")
  mean(predicted == test_labels)
}

test_features <- balanced_train_set %>% select(-Response)
test_labels   <- balanced_train_set$Response
tree_grid <- tree_grid %>%
  mutate(test_accuracy = map_dbl(fit, compute_accuracy,
                                 test_features, test_labels))

tree_grid <- tree_grid %>% arrange(desc(test_accuracy), desc(minsplit))
tree_grid

#selected parameters: minsplit=9, maxdepth=1, cp=0.01

prop.table(table(balanced_train_set$Response))

#build tree according to tuned parameters
tree = rpart(Response~ ., data=balanced_train_set, method="class", minsplit=9,maxdepth=1, cp=0.01)
printcp(tree)
rpart.plot(tree, extra = 101)

#Perform prediction with training set
train_pred <- predict(tree, balanced_train_set, type='class')
#Perform prediction with test set
test_pred <- predict(tree, test_set, type='class')

#Confusion Matrix for training set
confusionMatrix(train_pred, balanced_train_set$Response, positive='1') 

#Confusion Matrix for test set
confusionMatrix(test_pred, test_set$Response, positive='1')

#plot ROC
head(test_pred)
test_pred_prob <- predict(tree, test_set, type='prob')
test_pred_prob = test_pred_prob[,2]
r = multiclass.roc(test_set$Response, test_pred_prob, percent = TRUE)
roc = r[['rocs']]
r1 = roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         max.auc.polygon=TRUE,
         auc.polygon.col="light cyan",
         print.thres=TRUE,
         main= 'ROC Curve for Decision Tree after Hyperparameters Tuning')


#Logistic Regression
library(tidyverse)
library(caret)

#Build classifier
classifier = glm(Response ~.,
                 balanced_train_set,
                 family = binomial)

summary(classifier)

colnames(balanced_train_set)

prob_pred_train = predict(classifier, type = 'response', balanced_train_set[ ,-20] )
y_pred_train = ifelse(prob_pred_train > 0.5, 1, 0)

pred_prob_test = classifier %>% predict(test_set, type = 'response')
y_pred_test = ifelse (pred_prob_test > 0.5 , 1, 0)


cm_train = table(balanced_train_set$Response,y_pred_train)
cm_train

accuracy_train = sum(diag(cm_train))/sum(cm_train)
accuracy_train 


cm_test = table(test_set$Response,y_pred_test)
cm_test

accuracy_test = sum(diag(cm_test))/sum(cm_test)
accuracy_test 

head(test_pred)
length(pred_prob_test)


r = multiclass.roc(test_set$Response, pred_prob_test, percent = TRUE)
roc = r[['rocs']]
r1 = roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         max.auc.polygon=TRUE,
         auc.polygon.col="light cyan",
         print.thres=TRUE,
         main= 'ROC Curve for Logistic Regression')

#Support Vector Machine
library(caTools)
library(ggplot2)
library(e1071)
set.seed(123)

svm_rbf <- svm(Response~., data = balanced_train_set) 
summary(svm_rbf)
svm_rbf$gamma
svm_rbf$cost

#Performing predictions on train and test set
pred_train = predict (svm_rbf, balanced_train_set)
pred_test = predict (svm_rbf, test_set)


cm_train = table(Predicted = pred_train, Actual = balanced_train_set$Response)
cm_train
accuracy_train = sum(diag(cm_train))/sum(cm_train)*100
accuracy_train 


cm_test = table(Predicted = pred_test, Actual = test_set$Response)
cm_test
accuracy_test = sum(diag(cm_test))/sum(cm_test)*100
accuracy_test 

svm_rbf <- svm(Response~., data = balanced_train_set, probability = TRUE)

prob_pred_test = predict (svm_rbf, test_set, probability = TRUE)
length(prob_pred_test)
r = multiclass.roc(test_set$Response, as.numeric(prob_pred_test), percent = TRUE)
roc = r[['rocs']]
r1 = roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         max.auc.polygon=TRUE,
         auc.polygon.col="light cyan",
         print.thres=TRUE,
         main= 'ROC Curve for Support Vector Machine')

# ~~~~~~~~~~~~~~~~~~~~  Model Tuning  ~~~~~~~~~~~~~~~~~~~~
set.seed(123)
# tune function tunes the hyperparameters of the model using grid search method
tuned_model = tune(svm, Response~., data=balanced_train_set,
                   ranges = list(gamma = seq (0, 1, 0.2), cost = seq(0.1, 1, 0.2)))
plot (tuned_model)
summary (tuned_model)

opt_model = tuned_model$best.model
summary(opt_model)
opt_model$gamma

# Building the best model
svm_tuned <- svm (Response~., data = balanced_train_set, gamma = 0.2, cost = 0.9)
summary(svm_tuned)

pred_train = predict (svm_tuned, balanced_train_set)
pred_test = predict (svm_tuned, test_set)



cm_train = table(Predicted = pred_train, Actual = balanced_train_set$Response)
cm_train
accuracy_train = sum(diag(cm_train))/sum(cm_train)*100
accuracy_train

cm_test = table(Predicted = pred_test, Actual = test_set$Response)
cm_test
accuracy_test = sum(diag(cm_test))/sum(cm_test)*100
accuracy_test

svm_tuned <- svm (Response~., data = balanced_train_set, gamma = 0.2, cost = 0.9, probability = TRUE)

prob_pred_test_tuned = predict (svm_tuned, test_set, probability = TRUE)
length(prob_pred_test_tuned)
r = multiclass.roc(test_set$Response, as.numeric(prob_pred_test_tuned), percent = TRUE)
roc = r[['rocs']]
r1 = roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         max.auc.polygon=TRUE,
         auc.polygon.col="light cyan",
         print.thres=TRUE,
         main= 'ROC Curve for Support Vector Machine with Parameter Tuning')
