# Predicting Movement



### Background and Introduction

Background and Introduction
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.

In this project, we will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participant They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. The five ways are exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Only Class A corresponds to correct performance. The goal of this project is to predict the manner in which they did the exercise, i.e., Class A to E. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).



### Data Acquistion and Setup

The purpose of this section is provide the code used to load the data into R, clean the data, and partition the data. The training and test data are loaded into R. The training data are split into a training-train and training-test sets. The purpose of this process is to create a data set that can be used for evaluating the model and estimating out of sample error rates. The data are cleaned to remove any columns with missing data. 


```r
train.link = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.link = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training = read.csv(url(train.link), na.strings=c("NA","#DIV/0!","")) #Excel errors are not recognized as NaN
testing = read.csv(url(test.link), na.strings=c("NA","#DIV/0!","")) #Excel errors are not recognized as NaN
```

Loading the R packages required. Note that echo = FALSE so this doesn't print to the report to prevent unncessary information from the R packages from being displayed. 


```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## Rattle: A free graphical interface for data mining with R.
## Version 4.1.0 Copyright (c) 2006-2015 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data.
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: splines
```

```
## Loading required package: parallel
```

```
## Loaded gbm 2.1.3
```

Setting up the initial data partitions.


```r
set.seed(1)

## missing values are deleleted. 
training = training[, colSums(is.na(training)) == 0]
testing = testing[, colSums(is.na(testing)) == 0]

## getting rid of variables that shouldn't have any correlation to the outcome, e.g. name
training = training[, -c(1:7)]
testing = testing[, -c(1:7)]

training.split =createDataPartition(training$classe, p = .7, list = FALSE)
training.train = training[training.split,]
training.test = training[-training.split,]
```

### Model Fitting

The model fitting is done with (1) rpartand (2) random forest. The results of each is compared and the best model then used to predict the outcome for the testing dataset. 


```r
mod_rpart = train(classe ~ ., data = training.train, method = "rpart")
mod_rf = train(classe ~ ., data = training.train, method = "rf")
              #, 
              # trControl = trainControl(method = "cv", number = 5))

pred_rpart = predict(mod_rpart, training.test)
pred_rf = predict(mod_rf, training.test)
```



```r
## accuracy of the rpart
confusionMatrix(pred_rpart, training.test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1510  463  462  427  179
##          B   27  389   36  163  141
##          C  132  287  528  374  299
##          D    0    0    0    0    0
##          E    5    0    0    0  463
## 
## Overall Statistics
##                                           
##                Accuracy : 0.4911          
##                  95% CI : (0.4782, 0.5039)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.3352          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9020   0.3415  0.51462   0.0000  0.42791
## Specificity            0.6364   0.9227  0.77526   1.0000  0.99896
## Pos Pred Value         0.4965   0.5146  0.32593      NaN  0.98932
## Neg Pred Value         0.9423   0.8538  0.88324   0.8362  0.88573
## Prevalence             0.2845   0.1935  0.17434   0.1638  0.18386
## Detection Rate         0.2566   0.0661  0.08972   0.0000  0.07867
## Detection Prevalence   0.5167   0.1285  0.27528   0.0000  0.07952
## Balanced Accuracy      0.7692   0.6321  0.64494   0.5000  0.71344
```



```r
## accuracy using linear discriminant analysis
confusionMatrix(pred_rf, training.test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1669    7    0    0    0
##          B    3 1130    3    0    1
##          C    2    2 1020    5    0
##          D    0    0    3  957    2
##          E    0    0    0    2 1079
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9949          
##                  95% CI : (0.9927, 0.9966)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9936          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9970   0.9921   0.9942   0.9927   0.9972
## Specificity            0.9983   0.9985   0.9981   0.9990   0.9996
## Pos Pred Value         0.9958   0.9938   0.9913   0.9948   0.9981
## Neg Pred Value         0.9988   0.9981   0.9988   0.9986   0.9994
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2836   0.1920   0.1733   0.1626   0.1833
## Detection Prevalence   0.2848   0.1932   0.1749   0.1635   0.1837
## Balanced Accuracy      0.9977   0.9953   0.9961   0.9959   0.9984
```


### Applying the best model to the test dataset:

The random forest model is clearly superior and is used to generate predictions for the testing set. The results are then printed to the file. 


```r
pred_rf_test = predict(mod_rf, testing)
pred_rf_test
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
