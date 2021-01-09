#packages
library(DAAG) #Spam7 Data Set
library(e1071) #Support Vector Machine Library

#Error Function - confusion matrix
compute_error = function(a,b)
{
  tmp = table(a,b)
  out = ( sum(tmp) - sum(diag(tmp)) ) / sum(tmp) 
  out
}

#Test if Factor
class(train)
?spam7
set.seed(667)
ind = sample(1:5, size = nrow(spam7), replace = TRUE)
#test dataset is 1/5 size of total dataset
#train data set is 4/5 size of total dataset

test_index =  which(ind == 1)
train      =  spam7[ -test_index, ] 
test       =  spam7[  test_index, ] 

dim(train)
dim(test)
train
test

#SVM Model
svm.spam = svm(yesno ~ . , data = train, kernel = 'linear', gamma = 1, cost = 1e5) 
summary(svm.spam)
svm.spam$index
ypredict_1 = predict(svm.spam, test)
table(predict = ypredict_1, truth = test$yesno)
#Error of prediction
svm_svm_error = compute_error(test$y, ypredict_1) 
svm_svm_error




#Tuned model through cross validation
tune.out=tune(svm ,yesno ??? . ,data=train ,kernel = 'linear', 
              ranges=list(cost=c(.0001, 0.001, 0.01, 0.1, 1,5,10,100), gamma = c(0.001, 0.01, .1, 1) ))
tune.out
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
#Prediction through test data set
ypredict = predict(bestmod, test)
table(predict = ypredict, truth = test$yesno)
#Error of prediction
svm_bestmodel_error = compute_error(test$y, ypredict ) 
svm_bestmodel_error

